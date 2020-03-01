
# Custom operators

`%!in%` = Negate(`%in%`)
`%nin%` = Negate(`%in%`)

`%+%` <- function(x,y) paste(x,y,sep='')
`%+_%` <- function(x,y) paste(x,y,sep=' ')
`%+,%` <- function(x,y) paste(x,y,sep=',')


aprox_in <- function(x, y, fixed=T) {
  for (i in y) if(grepl(pattern = x, x = i, fixed = fixed)==T) return (T)
  return (F)
}

`%@in%` <- function(x, y) {
  for (i in y) if(grepl(pattern = x, x = i, fixed = T)==T) return (T)
  return (F)
}

. <- function(x) "`" %+% x %+% "`"

testf <- function(x) {
  DNAME <- deparse(substitute(x))
  print(DNAME)
}
#testf(x)

remove_empty_columns <- function(df) {  
  df %>% select_if(function(x) !(all(is.na(x)) | all(x=="")))
}

my_tab_yes <- function(x, look = "yes") {
  t <- table(x)
  t[look]/sum(t)
}

find_nearest <- function(x, divs = c(1, 2.5, 5, 10)){
  ord = 10^floor(log10(x))
  return( divs[ which.min(abs(divs-x/ord)) ]*ord )
}

round_to_multiple <- function (x, multiple = 1) round(x/multiple)*multiple

infer_limits <- function(x, suggested=NA,  divs = c(1, 2.5, 5, 10)) {
  m <- min(x, na.rm=T)
  M <- max(x, na.rm=T)
  N <- length(na.omit(x))
  
  if(is.na(suggested)) suggested <- 2*log(N)
  
  optimal_bin_raw = (M-m)/suggested
  bin_width=find_nearest(optimal_bin_raw, divs)
  
  a <- round_to_multiple(m, bin_width) # DescTools::RoundTo(m, bin_width)
  if(m < a) a <- a-bin_width
  
  b <- round_to_multiple(M, bin_width) # DescTools::RoundTo(m, bin_width)
  if(M > b) b <- b+bin_width
  
  return(c(a, b, bin_width))
}

make_metadata_factor <- function(DB, var, suggested = "ordered", lang="ro",  label=var, group=var) {
  
  levels <- levels(factor(as.character(DB[[var]])))
  true_levels <- unique(str_trim(tolower(levels), side = "both"))
  
  # defaults:
  Role <- suggested
  Levels <- paste(levels, collapse = ";")
  New.levels <- Levels #%>% tolower()
  
  # 2 true levels: "da", "Da", "nu" -> "da;da;nu"
  if (length(true_levels == 2)) {
    if( paste(sort(true_levels), collapse = ";") %in% c("da;nu", "oui;non")) {
      Role <- "danu"
      Levels <- paste(sort(levels), collapse = ";") # Da;da;nu
      New.levels <- tolower(Levels) # da;da;nu
    }
    else if( paste(sort(true_levels), collapse = ";") %in% c("no;yes")) {
      Role <- "danu"
      Levels <- paste(sort(levels, decreasing = T), collapse = ";")
      New.levels <- tolower(Levels)
    }
    else if( paste(sort(true_levels), collapse = ";") %in% c("0;1")) {
      Role <- "danu"
      Levels <- "1;0" # 0;1
      New.levels <- switch (lang, "ro" = "da;nu", "en" = "yes;no", "fr" = "oui;non", "1;0")
    }
    else if( paste(sort(true_levels), collapse = ";") %in% c("f;m")) {
      Role <- "binary"
      Levels <- paste(sort(levels), collapse = ";")
      New.levels <- toupper(Levels)
    }
    else if( paste(sort(true_levels), collapse = ";") %in% c("rural;urban")) {
      Role <- "binary"
      Levels <- paste(sort(levels), collapse = ";")
      New.levels <- tolower(Levels)
    }
  }
  # 1 true level: "da", "Da" -> "da;da;nu"
  else if (length(true_levels == 1)) {
    if ( true_levels %in% c("da", "nu", "yes", "no", "oui", "non", "1", "0") ) {
      Role <- "danu"
      Levels <- switch (true_levels, 
                        "da" = paste(levels, collapse=";") %+% ";nu",
                        "yes" = paste(levels, collapse=";") %+% ";no",
                        "oui" = paste(levels, collapse=";") %+% ";non",
                        "1" = paste(levels, collapse=";") %+% ";0",
                        "nu" = "da;" %+% paste(levels, collapse=";"),
                        "no" = "yes;" %+% paste(levels, collapse=";"),
                        "non" = "oui;" %+% paste(levels, collapse=";"),
                        "0" = "1;" %+% paste(levels, collapse=";"),
                        paste(levels, collapse=";")
      )
      New.levels <- tolower(Levels)
    }
    else {
      Role <- suggested
      Levels <- paste(levels, collapse = ";")
      New.levels <- Levels #%>% tolower()
    } 
  }
  # positive/negative/*: 
  else if ( regexpr(text = paste(sort(true_levels), collapse = ";"), pattern = "(neg.*)+;(po[szi].*)+")[1]>0 ) {
    Role <- "ordered"
    Levels <- paste(sort(levels, decreasing = T), collapse = ";")
    New.levels <- tolower(Levels)
  }
  # else:
  else {
    Role <- suggested
    if (suggested == "ordered") {
      Levels <- paste(sort(levels), collapse = ";")
    }
    else if (suggested == "factor") {
      Levels <- DB[[var]] %>% factor() %>% table %>% sort(decreasing = T) %>% names() %>% paste(collapse = ";")
    }
    else Levels <- paste(levels, collapse = ";")
    
    New.levels <- Levels #%>% tolower()
    
  }
 
  meta_row <- data.frame(`Levels` = Levels,
                         # `New.levels` = New.levels,
                         `Var` = var, `Label` = label, Group=group,
                         `Role` = Role, `Type` = "text") %>% 
    mutate_all(as.character)
  meta_row
}

make_metadata_numeric <- function(DB, var, 
                                  suggested = 4, scale="lin", label=var, group=var) {
  
  x.breaks <- infer_limits(DB[[var]], suggested, divs=c(0, 1, 2.5, 3, 5, 10))
  x.breaks_seq <- seq(x.breaks[1], x.breaks[2], x.breaks[3])
  x.breaks_seq_str <- paste(x.breaks_seq, collapse = ";")
  
  bins <- infer_limits(DB[[var]])
  bins_seq <- seq(bins[1], bins[2], bins[3])
  bins_seq_str <- paste(bins_seq, collapse = ";")
  
  meta_row <- data.frame(`Min` = x.breaks[1], `Max` = x.breaks[2], `W` = x.breaks[3],
                         `Scale` = scale, `Breaks` = x.breaks_seq_str,
                         `Bins.min` = bins[1], `Bins.max` = bins[2], `Bins.w` = bins[3],
                         `Bins` = bins_seq_str,
                         `Var` = var, `Label` = label, Group=group,
                         `Role` = "numeric", `Type` = "numeric") %>% 
    mutate_if(is.factor, as.character)
  meta_row
}

update_var_metadata <- function(DB, var, metadata=NULL, replace.exising.cells = F, # only if row already exists 
                                label = var, group=var, 
                                suggested.factor = "ordered", suggested.numeric = 4, scale = "lin") {
  
  if(is.null(metadata)) 
    metadata <- data.frame(`ID` = c(), `Var` = c(), `Role` = c(), `Type` = c(), `Group` = c(), `Label` = c())
  
  if (var %in% metadata$Var) { # known 
    row <- which(metadata$Var == var)[1]
    if (metadata$Type[row] == "numeric") { # known numeric, use make_metadata_numeric()
      meta_temp <- make_metadata_numeric(DB = DB, var = var, 
                                         label = label, suggested = suggested.numeric, scale = scale, group=group)
      
    }
    else { # known text / something else, use make_metadata_factor()
      meta_temp <- make_metadata_factor(DB = DB, var = var, 
                                        label = label, suggested = suggested.factor, group=group) 
    }
    
    for (col in names(meta_temp)) if (col %!in% c("New.levels")) {
      if (col %!in% names(metadata)) 
        metadata[[col]] <- rep(NA, nrow(metadata))
      if (replace.exising.cells == T)
        metadata[[row, col]] <- meta_temp[[1, col]]
      else if (is.na(metadata[[row, col]]))
        metadata[[row, col]] <- meta_temp[[1, col]]
    }
  }
  else { # unknown, decide
    if (is.numeric(DB[[var]])) { # new/unknown numeric, use make_metadata_numeric
      meta_temp <- make_metadata_numeric(DB = DB, var = var, 
                                         label = label, suggested = suggested.numeric, scale = scale, group=group) 
      }
    else { # new/unknown factor, use make_metadata_factor()
      meta_temp <- make_metadata_factor(DB = DB, var = var, 
                                        label = label, suggested = suggested.factor, group=group) 
      }
    
    meta_temp %<>% mutate(`ID` = max(c(0, metadata$ID), na.rm=T)+1)
    metadata <- bind_rows(metadata, meta_temp)
  }
  
  return(metadata)
  
}

make_metadata <- function(DB, vars=colnames(DB), existing.metadata=NULL, replace.exising.cells = F, # only if row already exists
                          label = vars, group=vars,
                          suggested.factor = "ordered", suggested.numeric = 4, scale = "lin") {
  
  replace.exising.cells = rep_len(replace.exising.cells, length(vars))
  group = rep_len(group, length(vars)) # makes sense if these is the same group for all variables
  label = rep_len(label, length(vars)) # only makes sense if these is the same label for all variables
  suggested.factor = rep_len(suggested.factor, length(vars))
  suggested.numeric = rep_len(suggested.numeric, length(vars))
  scale = rep_len(scale, length(vars))
  
  
  for (i in seq_along(vars)) {
    existing.metadata <- update_var_metadata(DB = DB, var = vars[i], metadata = existing.metadata,
                                             replace.exising.cells = replace.exising.cells[i],
                                             label = label[i],
                                             group = group[i],
                                             suggested.factor = suggested.factor[i],
                                             suggested.numeric = suggested.numeric[i],
                                             scale = scale[i] )
  }
  
  return(existing.metadata)
  
}

# for my_box, where data is renamed and transform if log scale
get_limits_from_metadata_1 <- function(db, var, metadata=NULL, log.correction=0.001) {
  if (is.null(metadata)) metadata <- update_var_metadata(db, metadata, var)
  if (var %!in% metadata$Var) metadata <- update_var_metadata(db, metadata, var)
  
  min.var <- metadata$Min[metadata$Var==var]
  max.var <- metadata$Max[metadata$Var==var]
  w.var <- metadata$W[metadata$Var==var]
  breaks.var <- seq(min.var, max.var, w.var)
  
  if("Breaks" %in% names(metadata)) {  if(!is.na(metadata$Breaks[metadata$Var==var])) {
    breaks.var <- metadata$Breaks[metadata$Var==var] %>% str_split(";", simplify = T) %>% as.numeric() %>% unique() %>% sort()
    min.var <- breaks.var[1]
    max.var <- breaks.var[length(breaks.var)]
    w.var <- (max.var-min.var)/10
  }} 
  if(is.na(min.var)) min.var = min(db$y, na.rm = T)
  if(is.na(max.var)) max.var = max(db$y, na.rm = T)
  if(is.na(w.var)) w.var = 1
  labels.var <- breaks.var
  
  scale.var <- metadata$Scale[metadata$Var==var]
  if(is.null(scale.var)) scale.var <- "lin"
  if(is.na(scale.var)) scale.var <- "lin"
  if(scale.var == "log2") {
    db$y <- log2(db$y+log.correction)
    min.var <- log2(min.var+log.correction)
    max.var <- log2(max.var)
    w.var <- log2(w.var)
    if (breaks.var[1]==0) breaks.var[1] <- breaks.var + log.correction
    breaks.var <- log2(breaks.var)
  } else if(scale.var == "log10") {
    db$y <- log10(db$y+log.correction)
    min.var <- log10(min.var+log.correction)
    max.var <- log10(max.var)
    w.var <- log10(w.var)
    if (breaks.var[1]==0) breaks.var[1] <- breaks.var[1] + log.correction
    breaks.var <- log10(breaks.var)
  }

  return (list(Min = min.var, Max = max.var, W = w.var, 
               Breaks = breaks.var, Scale = scale.var, Labels = labels.var,
               Y = db$y, Metadata = metadata))
}

# for my_hist, where data is not renamed or transform if log scale
get_limits_from_metadata_2 <- function(db, var, metadata=NULL, log.correction=0.001) {
  if (is.null(metadata)) metadata <- update_var_metadata(db, metadata, var)
  if (var %!in% metadata$Var) metadata <- update_var_metadata(db, metadata, var)
  
  min.var <- metadata$Min[metadata$Var==var]
  max.var <- metadata$Max[metadata$Var==var]
  w.var <- metadata$W[metadata$Var==var]
  breaks.var <- seq(min.var, max.var, w.var)
  
  if("Breaks" %in% names(metadata)) {  if(!is.na(metadata$Breaks[metadata$Var==var])) {
    breaks.var <- metadata$Breaks[metadata$Var==var] %>% str_split(";",simplify = T) %>% as.numeric() %>% unique() %>% sort()
    min.var <- breaks.var[1]
    max.var <- breaks.var[length(breaks.var)]
    w.var <- (max.var-min.var)/10
  }} 
  if(is.na(min.var)) min.var = min(db[var], na.rm = T)
  if(is.na(max.var)) max.var = max(db[var], na.rm = T)
  if(is.na(w.var)) w.var = 1
  labels.var <- breaks.var 
  
  scale.var <- metadata$Scale[metadata$Var==var]
  if(is.null(scale.var)) scale.var <- "lin"
  if(is.na(scale.var)) scale.var <- "lin"
  if(scale.var %in% c("log2", "log10")) {
    # db[var][db[var]==0] <- db[var][db[var]==0] + log.correction
    db[var][db[var]<0] <- NA
    db[var] <- db[var] + log.correction
    #min.var <- min.var + log.correction
    if (min.var==0) min.var <- min.var + log.correction
    if (breaks.var[1]==0) breaks.var[1] <- breaks.var[1] + log.correction
  }
  
  return (list(Min = min.var, Max = max.var, W = w.var, 
               Breaks = breaks.var, Scale = scale.var, Labels = labels.var,
               Metadata = metadata))
}

grubbs.flag <- function(x) {
  outliers <- NULL
  test <- x
  grubbs.result <- outliers::grubbs.test(test)
  pv <- grubbs.result$p.value
  # throw an error if there are too few values for the Grubb's test
  if (length(test) < 3 ) stop("Grubb's test requires > 2 input values")
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    # stop if all but two values are flagged as outliers
    if (length(test) < 3 ) {
      warning("All but two values flagged as outliers")
      break
    }
    grubbs.result <- outliers::grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}


na_duplicates <- function(x) {
  if (length(x)<=1) return (x)
  
  y=c(x[1])
  for (i in 2:length(x)) {
    if (x[i]==x[i-1]) y[i] <- NA
    else y[i] <- x[i]
  }
  y
}

mean_or_first <- function(x) {
  if(is.numeric(x)) mean(x, na.rm=T)
  else x[1]
}

nan_to_na <- function(x){
  x[is.nan(x)] <- NA
  x
}

limit_between <- function(x, low=0.001, high=1000, to.string=T, symbols=F, custom.symbols=c("≈0", "∞")) {
  #Error in if (x[i] < low) y[i] <- ifelse(to.string == T, ifelse(symbols == : missing value where TRUE/FALSE needed
  y <- rep(NA, length(x))
  for (i in 1:length(x)){
    if(!is.na(x[i])) {
      if (x[i]<low) y[i] <- ifelse(to.string==T, ifelse(symbols==T, custom.symbols[1], "<" %+% low), low)
      else if (x[i]>high)  y[i] <- ifelse(to.string==T, ifelse(symbols==T, custom.symbols[2], ">" %+% high), high)
      else  y[i] <- ifelse(to.string==T, round(x[i], digits = -log10(low)), x[i])
    }
    else y[i] <- NA
  }
  if (to.string==T) as.character(y) else y
}

# check var type
is.categ <- function(x, treshold=2) return(is.factor(x) & length(levels(factor(x) ))>treshold)
is.binary <- function(x) return(is.factor(x) & length(levels(factor(x) )) == 2)

# better as.numeric: adds a small jitter to make sure that certain packages do not treat it as integer
as.numeric.jitter <- function(x, sd=0.000001, positive=F) { 
  extra <- rnorm(length(x), 0, sd)
  if (positive) extra <- abs(extra)
  y = as.numeric(x)+extra
  
  if(!is.null(attr(x, "label")))
    attr(y, "label") <- attr(x, "label")
  
  return(y)
}

# better as.factor: pulls desired levels and label from metadata
as.factor_metadata <- function(x, metadata) {
  label <- attr(x, "label")
  # print(label)
  levels <- metadata$Levels[metadata$Var==label] %>% str_split(";", simplify=T) %>% as.vector()
  #if (is.na(levels)) levels <- levels(factor(x))
  if (paste(levels, sep="", collapse="") == "NA" ) { # avoid printig warnings
    if( metadata$Role[metadata$Var==label]=="factor")  levels <- names(sort(table(x),decreasing = T))
    else levels <- sort(levels(factor(x))) 
  }
  
  new.levels <- metadata$New.levels[metadata$Var==label] %>% str_split(";", simplify=T) %>% as.vector()
  if (is.na(new.levels)) new.levels = levels
  #print (paste(label, paste(levels, collapse=", "), paste(new.levels, collapse=", "), sep=": "))
  
  #if (length(levels)==0 & metadata$Type[metadata$Var==label] == "numeric") x <- factor(x)
  #if (paste(levels, sep="", collapse="") == "NA" ) levels <- sort(levels(factor(x))) # avoid printig warnings
  x <- factor(x, levels=levels)
  levels(x) <- new.levels
  
  #print(paste(label, paste(levels, collapse=", ") , length(levels), sep = ": "))
  attr(x, "label") = label
  x
}

# gets the desired label from metadata, if it exists.
metadata.var_to_labels <- function(var_names, metadata) {
  a <- rep(NA, length(var_names))
  b <- rep(NA, length(var_names))
  for (i in seq_along(var_names)) {
    a[i] <- which(metadata$Var==var_names[i])[1]
    
    if (is.na(a[i])) b[i] <- var_names[i]
    else b[i] <- metadata$Label[a[i]]
  }
  b
}

# polychoric R, as text
polyR.text <- function(x, y) {
  #cramer.V <- CramerV(droplevels(factor(x)), droplevels(factor(y)))
  polychor <- DescTools::CorPolychor(x, y, std.err=T)
  paste0("pR=", polychor$rho %>% scales::number(0.001), " (", 
         pchisq(polychor$chisq, polychor$df, lower.tail=F) %>% scales::pvalue(add_p=T), ")")
  #"; V=", cramer.V %>% scales::number(0.001))
}

# makes star symbols for p values
pstars <- function(x) cut(x, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1), include.lowest = T, labels = c('***', '**', '*', '⁺', '')) %>% as.character()

# sets a label to a variable, taking into consideration he desired metadata label if possible
set_label <- function(x, label=NULL, metadata=NULL, replace=F) {
  if (is.null(label)) { # no label provided, auto behavior
    #print(attr(x, "label"))
    if (is.null(attr(x, "label")) | replace == T) { # no previous label or don't care about it
      if(!is.null(metadata)) { # metadata provided
        candidate <- metadata$Label[metadata$Var==colnames(x)]
        if(!is.null(candidate)) attr(x, "label") <- candidate # new label found
        else attr(x, "label") <- names(x) # set label to var name: not working
      } 
      else { # no metadata, set label to var name
        #print("no arguments")
        attr(x, "label") <- names(x) # : not working
      }
    }
    # previous label found and orders to not replace it
    # do nothing
  } # new label explicitly provided
  else if (is.na(label)) attr(x, "label") = NA # remove label
  else attr(x, "label") = label
  
  #print(attr(x, "label"))
  return(x)
}

set_labels_df <- function(df, labels=NULL, metadata=NULL, replace=F) {
  # to do
}

# returns an array of variable names with the same group in the metadata
make_groups_metadata <- function(x, metadata){
  if(!is.null(metadata$Order)) metadata <- metadata %>% arrange(`Order`) 
  
  temp <- na.omit(c(metadata$Var[metadata$Group==x & metadata$Role %!in% c("exclude", "id", "separate")]))
  return( temp )
}

# make several binary vars into a cumulative binary using an OR operation
cumulate <- function(db, columns, target = c("da", "nu")) {
  x <- db %>% 
    select(columns) %>%
    mutate_all(function(x){abs(as.numeric(x)-2)}) %>% #ncol()
    rowSums() > 0
  x <- as.numeric(x) %>% factor(levels=c(1, 0))
  levels(x) <- target #c("da", "nu")
  x
  
}
# make several binary vars into a cumulative binary using an AND operation
cumulate.and <- function(db, columns, target = c("da", "nu")) {
  x <- db %>% 
    select(columns) %>%
    mutate_all(function(x){abs(as.numeric(x)-2)}) %>% #ncol()
    rowSums() = length(columns)
  x <- as.numeric(x) %>% factor(levels=c(1, 0))
  levels(x) <- target #c("da", "nu")
  x
  
}

# breaks an enumeration-type column into several individual yes/no columns based on metadata specifications
# ; between final columns, | between synonyms ?
separate_metadata_old <- function(db, x, metadata, add.prefix=T, exact=T, target.levels=c("da", "nu")){
  # exact==T: each new level can hold several old levels
  # exact==T : partial: only one old level for each new level
  
  # gather data
  db <- db[x]
  label <- attr(db[[x]], "label")
  levels <- metadata$Levels[metadata$Var==x] %>% str_split(";", simplify=T) %>% as.vector()
  #new.levels <- metadata$New.levels[metadata$Var==x] %>% str_split(";", simplify=T) 
  new.levels <- metadata$New.levels[metadata$Var==x] %>% str_split(";", simplify=T) #%>% as.vector()
  all.new.levels <- str_split(new.levels, "\\|", simplify=F) %>% unlist() %>% unique()
  
  #print(new.levels)
  
  # list of arrays with names of the new columns
  for (i in 1:length(new.levels)) {
    new.levels[i] = str_split(new.levels[[i]], "\\|", simplify=F) #%>% as.vector()
  }
  
  #print(levels)
  # make new columns
  for (a in all.new.levels) {
    #  which index in new.levels has `a`
    which.index.has.a <- c()
    for (i in 1:length(new.levels)) {
      if(a %in% new.levels[[i]]) which.index.has.a <- c(which.index.has.a, i)
    }
    
    # new empty column with "da" if original data has one of the levels correonding to the new column name
    b <- factor(rep(NA, nrow(db)), levels = target.levels)
    if (exact == T) temp <- which(db[[x]] %in% levels[which.index.has.a]) # exact: each new level can hold several old levels
    else temp <- grep(levels[which.index.has.a], db[[x]], fixed = T) # partial: only one old level for each new level
    #print(temp)
    #print(levels[which.index.has.a])
    if (length(temp)==0) b <- factor(rep(target.levels[2], nrow(db)), levels = target.levels)
    else {
      b[temp] <- target.levels[1]
      b[-temp] <- target.levels[2]
    }
    b[which(is.na(db[[x]]))] <- NA
    attr(b, "label") <- paste0(label, ": ", a)
    db <- bind_cols(db, new=b)
  }
  
  if (add.prefix==T) all.new.levels <- str_replace_all(paste0(x, ": ", all.new.levels), "::", ":")
  names(db) <- c(x, all.new.levels)
  #View(db)
  
  return (db[-1])
}


# breaks an enumeration-type column into several individual yes/no columns based on metadata specifications
# ; between final columns, | between synonyms ?
separate_metadata <- function(db, x, metadata, add.prefix=T, exact=T, target.levels=c("da", "nu")){
  # exact==T: each new level can hold several old levels
  # exact==T : partial: only one old level for each new level
  
  # gather data
  db <- db[x]
  label <- attr(db[[x]], "label")
  levels <- metadata$Levels[metadata$Var==x] %>% str_split(";", simplify=T) %>% as.vector()
  #new.levels <- metadata$New.levels[metadata$Var==x] %>% str_split(";", simplify=T) 
  new.levels <- metadata$New.levels[metadata$Var==x] %>% str_split(";", simplify=T) #%>% as.vector()
  all.new.levels <- str_split(new.levels, "\\|", simplify=F) %>% unlist() %>% unique()
  
  #print(new.levels)
  
  # list of arrays with names of the new columns
  for (i in 1:length(new.levels)) {
    new.levels[i] = str_split(new.levels[[i]], "\\|", simplify=F) #%>% as.vector()
  }
  
  #print(levels)
  # make new columns
  for (a in all.new.levels) {
    #  which index in new.levels has `a`
    which.index.has.a <- c()
    for (i in 1:length(new.levels)) {
      
      if (exact == T) {
        if(a %in% new.levels[[i]]) 
          which.index.has.a <- c(which.index.has.a, i) # exact: each new level can hold several old levels
      }
      else { 
        if(a %@in% new.levels[[i]])
          which.index.has.a <- c(which.index.has.a, i) # partial: only one old level for each new level
      }
    }
    
    # new empty column with "da" if original data has one of the levels correonding to the new column name
    b <- factor(rep(NA, nrow(db)), levels = target.levels)
    
    # ! there may be overlaps !
    temp  <- c()
    for (level in levels[which.index.has.a])
      temp <- c(temp, grep(level, db[[x]], fixed = T))
    temp <- sort(unique(temp))
    # if (exact == T) temp <- which(db[[x]] %in% levels[which.index.has.a]) # exact: each new level can hold several old levels
    # else temp <- grep(levels[which.index.has.a], db[[x]], fixed = T) # partial: only one old level for each new leve
    
    #print(temp)
    #print(levels[which.index.has.a])
    if (length(temp)==0) b <- factor(rep(target.levels[2], nrow(db)), levels = target.levels)
    else {
      b[temp] <- target.levels[1]
      b[-temp] <- target.levels[2]
    }
    b[which(is.na(db[[x]]))] <- NA
    # attr(b, "label") <- paste0(label, ": ", a)
    if (add.prefix==T) attr(b, "label") <- paste0(label, ": ", a)
    else attr(b, "label") <- a
    db <- bind_cols(db, new=b)
  }
  
  db <- labelled::set_variable_labels(db, .labels = c(x, all.new.levels))
  all.new.levels <- str_replace_all(paste0(x, ": ", all.new.levels), "::", ":")
  # if (add.prefix==T) all.new.levels <- str_replace_all(paste0(x, ": ", all.new.levels), "::", ":")
  names(db) <- c(x, all.new.levels)
  #View(db)
  
  return (db[-1])
}

# make a summary table based on a continuous variable
make_summary_table_cont <- function(db, continuous, vars=NULL, metadata=NULL, trim_to_metadata = T, gsd=F) {
  if(is.null(vars)) vars <- names(db)
  vars <- vars[vars != continuous]
  
  if(trim_to_metadata==T)
    vars <- intersect(metadata$Var[metadata$Role %in% c("danu", "binary", "factor", "ordered")], vars)
  #vars = c("SEX", "MEDIU", "FUMATOR", "NOXE", 'GOLD', "CLASA DE RISC")
  labels <- metadata.var_to_labels(vars, metadata)
  
  db <- db %>% select(c(continuous, vars)) 
  #print(vars)
  
  tables <- list()
  for (v in vars) {
    t <- mySSby0(db, numerica=continuous, categ=v, gm_correction=0.001)
    tables[[v]] <- list(tab = t, test = attr(t, "test"), text = attr(t, "test_text"))
  }
  
  
  res0 <- mySSby0(db, numerica=continuous, categ=NA, gm_correction=0.001)
  res0$Grup = "(total)"
  res0$var <- paste(metadata.var_to_labels(continuous, metadata), attr(res0, "test_text"))
  for (v in vars) {
    t <- tables[[v]]$tab
    group.label <- paste(metadata.var_to_labels(v, metadata), attr(t, "test_text"))
    t$`var` <- rep(group.label, nrow(t))
    #print(t)
    res0 <- bind_rows(res0, t)
  }
  
  
  
  res1 <- res0 %>% mutate(
    `N` = paste0(`n`, " (", scales::percent(`%`,0.1), ")"),
    `Media ±SD` = paste0(scales::number(`mean`, 0.01), " ±", scales::number(`sd`, 0.1)),
    `Media geom, SD` = paste0(scales::number(`GM`, 0.01), ", ", scales::number(`GSD`, 0.1)),
    `Med (Min:Max)` = paste0(scales::number(`median`, 0.1), " (", scales::number(`min`, 0.1), ":", scales::number(`max`, 0.1), ")")
  )# %>%
  if(gsd==T) res1 %<>%
    select(`Subset` = `Grup`, `N`, `Media ±SD`, `Med (Min:Max)`, `Media geom, SD`, `Grup`=`var`)
  else res1 %<>%
    select(`Subset` = `Grup`, `N`, `Media ±SD`, `Med (Min:Max)`, `Grup`=`var`)
  
  res1 %<>%
    flextable::as_grouped_data(groups = c("Grup")) %>% 
    # flextable::as_flextable.grouped_data()
    flextable::as_flextable(hide_grouplabel=T) %>%
    flextable::theme_booktabs(10) %>%
    flextable::bold(part="header")  %>%
    flextable::align(align="left", part = "all") %>%
    flextable::autofit(add_w = 0, add_h = 0) %>%
    #flextable::height_all(0.05) %>%
    flextable::padding(padding = 0) %>%
    flextable::bold(j = 1, i = ~ !is.na(`Grup`), bold = TRUE, part = "body" )  %>% 
    flextable::italic(j = 1, i = ~ !is.na(`Grup`), italic = TRUE, part = "body" ) %>%
  # flextable::hline(i = ~ !is.na(`Grup`), border=officer::fp_border(color = "black", style = "solid", width = 1))  %>%
  # flextable::add_footer_lines(values="*M(R) = Mediana (minim:maxim); MW = Test Mann-Whitney; OR/RR = odds-ratio / risc relativ [cu IC 95%] și p calculat prin testul Fisher); V = Cramer V (p calculat prin testul Chi2).") %>%  
  # flextable::italic(part = "footer")
    flextable::fix_border_issues()
  
  res1
}






