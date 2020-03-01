
library(dplyr)
library(knitr)
library(kableExtra)
library(DescTools)
library(vcd)


proc_total.mxn <- function(row.var, col.var, data, useNA="no", structure = c("#", "row%"), accuracy=0.1, 
                           row.total="right", col.total="bottom", total.string="(total)", keep.type=F)  {
  
  t <- table(data[[row.var]], data[[col.var]], useNA = useNA)
  totals.row <- table(data[[row.var]], useNA = useNA)
  totals.col <- table(data[[col.var]], useNA = useNA)
  s <- sum(totals.row)
  
  numbers <- t %>% as.character()
  row_p <- prop.table(t, margin=1) %>% as.numeric() %>% scales::percent(accuracy)
  col_p <- prop.table(t, margin=2) %>% as.numeric() %>% scales::percent(accuracy)
  
  totals.numbers.row <- totals.row %>% as.character()
  totals.row_p <- prop.table(totals.row) %>% as.numeric() %>% scales::percent(accuracy)
  totals.numbers.col <- totals.col %>% as.character()
  totals.col_p <- prop.table(totals.col) %>% as.numeric() %>% scales::percent(accuracy)
  
  row.levels <- c(attr(t, "dimnames")[[1]], total.string)
  #if(row.total=="left") row.levels <- c(total.string, attr(t, "dimnames")[[1]])
  #else row.levels <- c(attr(t, "dimnames")[[1]], total.string)
  
  col.levels <- c(attr(t, "dimnames")[[2]], total.string)
  #if(col.total=="top") col.levels <- c(total.string, attr(t, "dimnames")[[2]]) 
  #else col.levels <- c(attr(t, "dimnames")[[2]], total.string)
  
  #print(col.levels)
  
  res.numbers <- c(numbers, totals.numbers.row) %>% 
    matrix(nrow = length(row.levels)-1) %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    rbind( c(totals.numbers.col, s) ) %>%
    set_colnames( col.levels ) %>% 
    mutate(row = row.levels, type="#")
  
  res.row.percents <- c(row_p, totals.row_p) %>% 
    matrix(nrow = length(row.levels)-1) %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    rbind( c(totals.col_p, "100%") ) %>%
    set_colnames( col.levels ) %>% 
    mutate(row = row.levels, type="row%")
  
  res.col.percents <- c(col_p, totals.row_p) %>% 
    matrix(nrow = length(row.levels)-1) %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    rbind( c(totals.col_p, "100%") ) %>%
    set_colnames( col.levels ) %>% 
    mutate(row = row.levels, type="col%")
  
  res.percents <- c(paste0( row_p, " / ",  col_p), totals.row_p) %>% 
    matrix(nrow = length(row.levels)-1) %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    rbind( c(totals.col_p, "100%") ) %>%
    set_colnames( col.levels ) %>% 
    mutate(row = row.levels, type="%")
  
  res.numbers.row.percents <- c(paste0(numbers, " (", row_p, ")"), 
                                paste0(totals.numbers.row, " (", totals.row_p, ")")) %>% 
    matrix(nrow = length(row.levels)-1) %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    rbind( c(paste0(totals.numbers.col, " (", totals.col_p, ")"), paste0(s, " (100%)")) ) %>%
    set_colnames( col.levels ) %>% 
    mutate(row = row.levels, type="#.row%")
  
  res.numbers.col.percents <- c(paste0(numbers, " (", col_p, ")"), 
                                paste0(totals.numbers.row, " (", totals.row_p, ")")) %>% 
    matrix(nrow = length(row.levels)-1) %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    rbind( c(paste0(totals.numbers.col, " (", totals.col_p, ")"), paste0(s, " (100%)")) ) %>%
    set_colnames( col.levels ) %>% 
    mutate(row = row.levels, type="#.col%")
  
  res.numbers.percents <- c(paste0(numbers, " (", row_p, " / ", col_p, ")"), 
                            paste0(totals.numbers.row, " (", totals.row_p, ")")) %>% 
    matrix(nrow = length(row.levels)-1) %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    rbind( c(paste0(totals.numbers.col, " (", totals.col_p, ")"), paste0(s, " (100%)") ) ) %>%
    set_colnames( col.levels ) %>% 
    mutate(row = row.levels, type="#.%")
  
  
  col.order <- NA
  if (row.total=="no") col.order <- attr(t, "dimnames")[[2]]
  else if(row.total=="left") col.order <- c(total.string, attr(t, "dimnames")[[2]])
  else if(row.total=="right") col.order <- c(attr(t, "dimnames")[[2]], total.string)
  

  row.order <- NA
  if(col.total=="no") row.order <- attr(t, "dimnames")[[1]]
  else if(col.total=="top") row.order <- c(total.string, attr(t, "dimnames")[[1]])
  else if(col.total=="bottom") row.order <- c(attr(t, "dimnames")[[1]], total.string)

  res <- bind_rows(res.numbers, 
                   res.row.percents, res.col.percents, res.percents, 
                   res.numbers.row.percents, res.numbers.col.percents, res.numbers.percents) %>%
    filter(type %in% structure) %>%
    select("row", "type", col.order) %>%
    arrange(factor(`row`, levels = row.order)) 
  
  # number=T, row.percent=T, col.pcercent=T, accuracy=0.1, join.percent=T, single.row=T,
  if (keep.type==F) res %<>% select(-c(type))
  if (row.total=="no") res %<>% select(-c(total.string))
  if (col.total=="no") res %<>% filter(row != total.string)
  
  #colnames(res)[1] <- row.var
  
  res
  
}



contingency.stats<-function(row.var, col.var, data, useNA="no",
                            p.n="pearson", v.n=T, p.2="fisher", rr.2=F, or.2=T, ci.2="asymptotic", phi.2=T, rr.dir="r2c") {
  t <- table(data[[row.var]], data[[col.var]], useNA = useNA)
  if (rr.dir=="r2c")
    t.rr <- t
  else { # rr.dir=="c2r"
    t.rr <- table(data[[col.var]], data[[row.var]], useNA = useNA)
  }
  
  ######### statistics
  if (length(attr(t, "dimnames")[[1]]) == 2 & length(attr(t, "dimnames")[[2]]) == 2) { 
    ########### 2x2
    
    a2 <- vcd::assocstats(t) #$ `contingency`, `phi`, `cramer` (V), `chisq_tests`[2,3], `table`
    Fisher <- fisher.test(t) #$ `p.value`, `estimate` (OR), `conf.int` [1:2] # CI the same as DescTools's method = "mle"
    RR = DescTools::RelRisk(t.rr, conf.level = 0.95) #$ 1=`rel. risk`, 2=`lwr.ci`, 3=`upr.ci`
    OR = DescTools::OddsRatio(t, conf.level = 0.95) #$ `odds ratio`, `lwr.ci`, `upr.ci`
    s2<-"" # result
    
    .or = NA 
    .or.ci = NA
    if (or.2 == T) { 
      .or <- OR[1]
      if ( ci.2 == "asymptotic" ) .or.ci = OR[2:3] # default
      else if ( ci.2 == "exact" ) .or.ci = DescTools::OddsRatio(t, conf.level = 0.95, method="mle")[2:3] # same as Fisher
      
      if( is.na(.or.ci) ) .or.ci=""
      else .or.ci = paste0(" [", scales::number(limit(.or.ci), 0.01) %>% paste0(collapse = ", "), "]")
    }
    if( is.na(.or) ) .or=NA # default
    else .or = paste0("OR=", scales::number(limit(.or), 0.01), .or.ci)
    
    
    
    .rr = NA
    .rr.ci = NA
    if (rr.2 == T) { # default
      .rr <- RR[1]
      if ( ci.2 == "asymptotic" ) .rr.ci = DescTools::RelRisk(t.rr, conf.level = 0.95, method="wald")[2:3]  # default
      else if ( ci.2 == "exact" ) .rr.ci = RR[2:3]
      
      if( is.na(.rr.ci) ) .rr.ci=""
      else .rr.ci = paste0(" [", scales::number(limit(.rr.ci), 0.01) %>% paste0(collapse = ", "), "]")
    }
    if( is.na(.rr) ) .rr=NA
    else .rr = paste0("RR=", scales::number(limit(.rr), 0.01), .rr.ci)  # default
    
    
    if (phi.2 == T) .phi = paste0("phi=", scales::pvalue(a2$phi, add_p = F, accuracy = 0.01) )  # default
    else .phi=NA
    
    
    .p = NA
    if (p.2 == "fisher") .p <- Fisher$p.value # default
    else if (p.2 == "chi2") .p <- chisq.test(t)$p.value
    else if (p.2 == "yates") .p <- chisq.test(t, correct = T)$p.value
    else if (p.2 == "pearson") .p <- a2$`chisq_tests`[2,3]
    else if (p.2 == "likelihood") .p <- a2$`chisq_tests`[1,3]
    
    if(or.2==F & rr.2==F & phi.2==F) 
      s2 <- paste0( scales::pvalue(.p, add_p = T), 
                    ifelse(p.2 == "chi2" & grepl("Yates", chisq.test(t)$method), "*", "") ) 
    else if( is.na(.p) ) .p=NA
    else {
      .p = paste0("(", scales::pvalue(.p, add_p = T), ifelse(p.2 == "chi2" & grepl("Yates", chisq.test(t)$method), "*", ""), ")")
      s2 <- paste(paste(c(.or, .rr, .phi) %>% na.omit(), collapse = ", "), .p, collapse = " ")
    }
    
    
    attr(s2, "OR") = OR
    attr(s2, "RR") = RR
    attr(s2, "fisher") = Fisher
    attr(s2, "asocs") = a2
    
    return (s2)
    
  } else { ################# MxN
    
    asocs <- vcd::assocstats(t)  
    res.string <- "" # result
    
    if (v.n == T) .v = paste0("V=", scales::number(asocs$cramer, accuracy = 0.001) )  # default
    else .v = NA
    
    if (v.n == T) .v = paste0("V=", scales::pvalue(asocs$cramer, add_p = F, accuracy = 0.01) )  # default
    else .v = NA
    
    .p = NA
    if (p.n == "pearson") {
      .p <- asocs$`chisq_tests`[2,3]
      if (is.na(.p)) .p <- asocs$`chisq_tests`[1,3]
    }
    else if (p.n == "likelihood") .p <- asocs$`chisq_tests`[1,3]
    #else if (p.n == "fisher") .p <- fisher.test(t)$p.value
    
    if(is.na(.p)) {
      assocs2 <- vcd::assocstats(t[rowSums(t)>0,colSums(t)>0])
      if (p.n == "pearson") {
        .p <- asocs$`chisq_tests`[2,3]
        if (is.na(.p)) .p <- asocs$`chisq_tests`[1,3]
      }
      else if (p.n == "likelihood") .p <- asocs$`chisq_tests`[1,3]
    }
    
    if(v.n==F) 
      res.string <- scales::pvalue(.p, add_p = T) 
    else if( is.na(.p) ) .p=NA
    else {
      .p = paste0("(", scales::pvalue(.p, add_p = T), ")")
      res.string <- paste(paste(c(.v) %>% na.omit(), collapse = ", "), .p, collapse = " ")
    }
    
    #attr(res.string, "fisher") = fisher.test(t)
    attr(res.string, "asocs") = asocs
    
    return (res.string)
    
  }
  
}





contingency.table <- function(row.var, col.var, data, caption=NULL, metadata=NULL,
                              font.face=NULL,
                              useNA="no", structure = c("#", "row%"), accuracy=0.1, 
                              row.total="right", col.total="bottom", total.string="(total)", keep.type=F, 
                              p.n="pearson", v.n=T, p.2="fisher", rr.2=F, rr.dir="r2c", or.2=T, ci.2="asymptotic", phi.2=T) {
  
  if (is.null(font.face)) {
    if (exists("MAIN.FONT")) { font.face = MAIN.FONT
    } else {font.face = "Arial"} }
  
  res <- proc_total.mxn(row.var, col.var, data, structure = structure, accuracy=accuracy, useNA = useNA,
                        row.total=row.total, col.total=col.total, total.string=total.string, keep.type=keep.type)
  
  t <- table(data[[row.var]], data[[col.var]], useNA = "no")
  stats <- contingency.stats(row.var, col.var, data, useNA = useNA,
                             p.n=p.n, v.n=v.n, p.2=p.2, rr.2=rr.2, rr.dir=rr.dir, or.2=or.2, ci.2=ci.2, phi.2=phi.2)
  
  
  res$row <- na_duplicates( res$row )
  names(res)[1] <- metadata.var_to_labels(row.var, metadata)
  header <- c("", col.var=length(levels(factor(data[[col.var]]))), "")
  names(header) <- c("", col.var, "")
  
  res %<>% mutate_all(str_remove_all, " \\(0\\.0%.*") %>%
    mutate_all(str_remove_all, " \\(NA / 0\\.0%.*") %>%
    mutate_all(str_replace_all, "100\\.0%", "100%")
  
  # (NA / 0.0%)
  if(!is.null(caption)) {
    if (is.na(caption)) {
      caption=paste0("Relația dintre ", metadata.var_to_labels(col.var, metadata) , " și ", metadata.var_to_labels(row.var, metadata), ".")
    } else if (is.function(caption)) {
      caption=caption(
        paste0("contingency.tab_"< col.var, "_", row.var), 
        paste0("Relația dintre ", metadata.var_to_labels(col.var, metadata) , " și ", metadata.var_to_labels(row.var, metadata), ".") )
      
    }
    cat(paste0("\n\n", caption, "\n\n"))
  }
  
  res2 <- res %>% flextable::flextable() %>% 
    flextable::bold(part="header")  %>%
    flextable::align(align="left", part = "all") %>%
    flextable::autofit(add_w = 0, add_h = 0) %>% 
    #flextable::height_all(0.05) %>%
    flextable::padding(padding = 0) %>%
    
    flextable::add_footer_lines(values=stats) %>%
    flextable::border(part="footer", border.bottom = officer::fp_border(color = "black", style = "solid", width = 1)) %>%
    flextable::italic(part = "footer") %>%
    
    flextable::add_header_lines(values=paste0("vs. ", metadata.var_to_labels(col.var, metadata))) %>% 
    flextable::border(part="header", i = 1, border.top = officer::fp_border(color = "black", style = "solid", width = 1)) %>%
    flextable::align(align="right", part = "header", i = 1) 

  if(col.total=="bottom")
    res2 %<>% flextable::hline(i = nrow(res)-length(structure), border=officer::fp_border(color = "black", style = "dashed", width = 1))
  else if(col.total=="top")
    res2 %<>% flextable::hline(i = length(structure), border=officer::fp_border(color = "black", style = "dashed", width = 1))

  res2 %<>% flextable::fix_border_issues() %>%
    flextable::height_all(0.2) %>% flextable::font(fontname = font.face, part="all")
  
  return (res2)

}




# proc_total.mxn("Sex", "Mediu de viata", baza_de_date, structure = c("#.row%"))
# contingency.stats("Sex", "Mediu de viata", baza_de_date, 
#                   p.n="pearson", v.n=T, p.2="fisher", rr.2=T, or.2=T, ci.2="asymptotic", phi.2=T)
# 
# 
# contingency.table ("Sex", "Mediu de viata", baza_de_date)
# contingency.table ("Sex", "Tipul tenului (fototip)", baza_de_date)
