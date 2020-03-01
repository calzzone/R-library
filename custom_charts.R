# helpers for more important functions

CreateAllFacet <- function(df, col, name = "facet", level = "all"){
  df$facet <- df[[col]]
  temp <- df
  temp$facet <- level
  merged <-rbind(temp, df)
  
  # ensure the facet value is a factor
  merged[[col]] <- as.factor(merged[[col]])
  
  merged[[name]] <- factor(merged$facet, levels=c(levels(df[[col]]), level))
  if(name != "facet") merged %<>% select(-c("facet"))
  return(merged)
}

# custom labeller: adds a custom prefix to facet labels (useful when the name of the column is different from the desired label)
label_prefix <- function (my_prefix="", sep="", wrap = 25, multi_line = TRUE ) 
{
  fun <- function(labels) {
    labels <- label_value(labels, multi_line = multi_line)
    lapply(labels, function(x) {
      x <- paste(my_prefix, x, sep=sep)
      #print(str(x))
      x <- strwrap(x, width = wrap, simplify = FALSE)
      x <- vapply(x, paste, character(1), collapse = "\n")
      
      x
    })
  }
  structure(fun, class = "labeller")
}


# Custom versions for common charts, complete with custom theming

my_scatter <- function(db, x, y, facet=NULL, color=NULL, fill=color, shape=NULL, 
                       metadata, 
                       R="spearman", stroke=0.1,
                       add.facet.name=F, max_size=3, alpha=0.5, wrap.legend=20, 
                       line.color = "black", line.se=F, 
                       log.correction=0.001,
                       perpendicular.labels.x.treshold=25) {
  # Facets get full independence. Colors just get different points. Specifing colors overrides size scale.
  
  data <- db %>% select(c(x, y)) %>% set_colnames(c("x", "y"))
  if (!is.null(facet)) data %<>% bind_cols(db %>% select(c(facet)) %>% set_colnames(c("facet")))
  if (!is.null(fill)) data %<>% bind_cols(db %>% select(c(fill)) %>% set_colnames(c("fill")))
  if (!is.null(shape)) data %<>% bind_cols(db %>% select(c(shape)) %>% set_colnames(c("shape")))
  
  
  min.x <- metadata$Min[metadata$Var==x]
  max.x <- metadata$Max[metadata$Var==x]
  w.x <- metadata$W[metadata$Var==x]
  breaks.x <- seq(min.x, max.x, w.x)
  
  if("Breaks" %in% names(metadata)) {  if(!is.na(metadata$Breaks[metadata$Var==x])) {
    breaks.x <- metadata$Breaks[metadata$Var==x] %>% str_split(";",simplify = T) %>% as.numeric()
    min.x <- breaks.x[1]
    max.x <- breaks.x[length(breaks.x)]
    w.x <- (max.x-min.x)/10
  }} 
  if(is.na(min.x)) min.x = min(data$x, na.rm = T)
  if(is.na(max.x)) max.x = max(data$x, na.rm = T)
  if(is.na(w.x)) w.x = 1
  labels.x <- breaks.x #%+% "\n" %+% log2(breaks.x)
  
  scale.x <- metadata$Scale[metadata$Var==x]
  if(is.null(scale.x)) scale.x <- "lin" 
  if(is.na(scale.x)) scale.x <- "lin" 
  if(scale.x == "log2") {
    data$x[data$x<0] <- NA
    data$x <- log2(data$x+log.correction)
    min.x <- log2(min.x+log.correction)
    max.x <- log2(max.x + log.correction)
    w.x <- log2(w.x)
    #if (breaks.x[1]==0) breaks.x[1] <- breaks.x[1] + log.correction
    breaks.x <- log2(breaks.x + log.correction)
  } else if(scale.x == "log10") {
    data$x[data$x<0] <- NA
    data$x <- log10(data$x+log.correction)
    min.x <- log10(min.x+log.correction)
    max.x <- log10(max.x + log.correction)
    w.x <- log10(w.x)
    #if (breaks.x[1]==0) breaks.x[1] <- breaks.x[1] + log.correction
    breaks.x <- log10(breaks.x + log.correction)
  }
  
  min.y <- metadata$Min[metadata$Var==y]
  max.y <- metadata$Max[metadata$Var==y]
  w.y <- metadata$W[metadata$Var==y]
  breaks.y <- seq(min.y, max.y, w.y)
  
  if("Breaks" %in% names(metadata)) {  if(!is.na(metadata$Breaks[metadata$Var==y])) {
    breaks.y <- metadata$Breaks[metadata$Var==y] %>% str_split(";",simplify = T) %>% as.numeric()
    min.y <- breaks.y[1]
    max.y <- breaks.y[length(breaks.y)]
    w.y <- (max.y-min.y)/10
  }} 
  if(is.na(min.y)) min.y = min(data$y, na.rm = T)
  if(is.na(max.y)) max.y = max(data$y, na.rm = T)
  if(is.na(w.y)) w.y = 1
  labels.y <- breaks.y
  
  scale.y <- metadata$Scale[metadata$Var==y]
  if(is.null(scale.y)) scale.y <- "lin" 
  if(is.na(scale.y)) scale.y <- "lin" 
  if(scale.y == "log2") {
    data$y[data$y<0] <- NA
    data$y <- log2(data$y+log.correction)
    min.y <- log2(min.y+log.correction)
    max.y <- log2(max.y + log.correction)
    w.y <- log2(w.y)
    #if (breaks.y[1]==0) breaks.y[1] <- breaks.y[1] + log.correction
    breaks.y <- log2(breaks.y + log.correction)
  } else if(scale.y == "log10") {
    data$y[data$y<0] <- NA
    data$y <- log10(data$y+log.correction)
    min.y <- log10(min.y+log.correction)
    max.y <- log10(max.y + log.correction)
    w.y <- log10(w.y)
    #if (breaks.y[1]==0) breaks.y[1] <- breaks.y[1] + log.correction
    breaks.y <- log10(breaks.y + log.correction)
  }
  
  
  
  CT <- cor.test(data[["x"]], data[["y"]], method = R, use="pairwise.complete.obs")
  CTS <- paste0("R = ", scales::number(CT$estimate, 0.001), " (", scales::pvalue(CT$p.value, add_p = T), ")")
  
  LM <- lm(`y`~`x`, data=data)
  y.smooth.lim <- sort(predict(LM, data.frame(x=c(min.x-max.x^5,  max.x ^ 5)) ))
  y.smooth.lim[1] = min(c(-100000, min.y, y.smooth.lim[1]))
  y.smooth.lim[2] = max(c(100000, max.y, y.smooth.lim[2]))
  x.smooth.lim <- sort(predict(lm(`x`~`y`, data=data), data.frame(y=c(min.y-max.y^2, max.y ^ 2)) ))
  x.smooth.lim[1] = min(c(-100000, min.x, x.smooth.lim[1]))
  x.smooth.lim[2] = max(c(100000, max.y, x.smooth.lim[2]))

  if(scale.x %in% c("log2", "log10"))
    x.smooth.lim = c(-50, 50000)
  
  if(scale.y %in% c("log2", "log10"))
    y.smooth.lim = c(-50, 50000)
  
  # x.smooth.lim = c(-50, 50)
  # y.smooth.lim = c(-5000, 50000)
  
  g <- data %>% 
    ggplot()+
    aes(x=`x`, y=`y`)
  
  if (!is.null(facet)) {
    if (add.facet.name==T) my_labeller = labeller(`facet` = label_prefix(my_prefix = metadata.var_to_labels(facet, metadata), sep = ": ", wrap=25))
    else my_labeller = labeller(`facet` = label_wrap_gen(25))
    g <- g + 
      facet_wrap(.~`facet`, labeller = my_labeller ) + 
      labs(subtitle = "General: " %+% CTS)
  }
  
  g <- g + geom_smooth(se=line.se, method = "lm", fullrange=T, color=line.color, linetype="solid", size=0.5, alpha=alpha/10)
  
  if (!is.null(fill)) {
    if (!is.null(shape))
      g <- g + geom_count(aes(fill=`fill`, shape=`shape`), alpha=alpha, guide = F, color=line.color, stroke=stroke)
    else 
      g <- g + geom_count(aes(fill=`fill`), alpha=alpha, pch=21, guide = F, color=line.color, stroke=stroke)
    
    if (is.factor(data$`fill`))
      g <- g + 
        geom_smooth(aes(color=fill), se=F, method = "lm", fullrange=T, linetype="dashed", size=0.33)+
        scale_fill_discrete(str_wrap(metadata.var_to_labels(fill, metadata), wrap.legend))+
        scale_color_discrete(str_wrap(metadata.var_to_labels(fill, metadata), wrap.legend))
    else
      g <- g + 
        #geom_smooth(aes(weight=fill), se=F, method = "lm", fullrange=T, linetype="dashed", size=0.33)+
        scale_fill_viridis_c(str_wrap(metadata.var_to_labels(fill, metadata), wrap.legend))
  } else {
    if (!is.null(shape))
      g <- g + geom_count(aes(fill=..n.., shape=`shape`), alpha=alpha, guide = F, color=line.color, stroke=stroke)
    else 
      g <- g + geom_count(aes(fill=..n..), alpha=alpha, pch=21, guide = F, color=line.color, stroke=stroke)
    
    g <- g + 
      # geom_count(aes(fill=..n..), alpha=alpha, guide = F, pch=21, color=line.color, stroke=0.1) +
      scale_fill_continuous("N", low="grey40", high="grey20", guide=F)
      #scale_fill_viridis_c("N", guide = F)#
  }
  
  g <- g +
    scale_size_area("N", max_size = max_size, breaks=seq(0, 100, 1)) +
    
    stat_cor(method = R, hjust=0, vjust=-0.5, size=3, fontface="italic",
             label.y = max.y, label.x = min.x,
             output.type = "text", # default is expression => strange things happen
             aes(label = paste0("R = ", scales::number(..r.., 0.001), " (", scales::pvalue(..p.., add_p = T), ")") ) ) +
    scale_x_continuous(str_wrap(metadata.var_to_labels(x, metadata), 30),
                       limits = x.smooth.lim,
                       breaks = breaks.x,
                       labels = labels.x) +
    scale_y_continuous(str_wrap(metadata.var_to_labels(y, metadata), 30),
                       #expand = expand_scale(mult=c(0.05, 0.1)),
                       limits = y.smooth.lim,
                       breaks = breaks.y,
                       labels = labels.y) +
    coord_cartesian(xlim=c(min.x, max.x),
                    ylim=c(min.y, max.y+(max.y-min.y)*0.1 )) +
    theme(plot.subtitle = element_text(face="italic", size=9), legend.position = "right")
  
  if(str_length(paste(breaks.x, collapse = "")) >= perpendicular.labels.x.treshold) g <- g + perpendicular.labels.x
  
  attr(g, "CT") = CT
  attr(g, "CTS") = CTS
  attr(g, "LM") = LM
  g
}

multiple_boxplot <- function(data, x, y, parse_factors=T, palette="auto", palette.direction="auto", metadata=NULL, strip.wrap=20, scales = "free_x", nrow = 1) {
  #x = c("cyl", "vs", "am", "gear")
  #y = "mpg"
  #data <- mtcars[, c(y, x)]
  
  if(is.null(strip.wrap)) if(is.na(strip.wrap)) if(strip.wrap <= 0) strip.wrap = 20
  
  if (length(palette) == 1) palette <- rep(palette, length(x))
  if (length(palette.direction) == 1) palette.direction <- rep(palette.direction, length(x))
  
  colors <- c()
  levels.list <- c()
  data <- data[, c(y, x)]
  for (v in 1+seq_along(x)) {
    if (parse_factors==T) data[v][[1]] <- factor(data[v][[1]])
    temp.levels <- data[v][[1]] %>% 
      na.omit() %>% 
      factor() %>% levels ()
    
    if (palette[v-1] == "random") P <- sample.int(18, 1)
    else if (palette[v-1] == "auto") {
      L <- temp.levels %>% sort() %>% tolower() %>% paste0(collapse="")
      if (L %in% c("danu", "noyes", "nonoui") ) P <- "Dark2"
      else if (L %in% c("ruralurban") ) P <- "Accent"
      else if (L %in% c("fm") ) P <- "Pastel1"
      else if (length(temp.levels)==2) P <- "Set3"
      else P <- "Spectral"
    }
    else P <- palette[v-1]
    
    if (palette.direction[v-1] == "random") D <- sample(c(1, -1))[1] #%>% as.numeric()
    else if (palette.direction[v-1] == "auto") {
      L <- temp.levels %>% sort() %>% tolower() %>% paste0(collapse="")
      if (L %in% c("danu", "noyes", "nonoui") ) D <- 1
      else if (L %in% c("ruralurban") ) D <- 1
      else if (L %in% c("fm") ) D <- 1
      else if (length(temp.levels)==2) D <- 1
      else D <- -1 # low degree (blue/green, good) to high degree (red, bad) of a disease
    }
    else D <- palette.direction[v-1]
    
    
    levels.list <- c(levels.list, temp.levels )
    colors <- c(colors, temp.levels %>% length() %>% 
                  #scales::brewer_pal(palette = sample.int(18, 1), direction = -1) () )
                  #scales::brewer_pal(palette = "Spectral", direction = 1) () )
                  scales::brewer_pal(palette = P, direction = D) () )
    #colors <- c(colors, temp.levels %>% length() %>% scales::hue_pal()())
    
  }
  
  #data <- reshape2::melt(data, id.vars=y) 
  data <- tidyr::gather(data, variable, value, -y) %>% na.omit() 
  colnames(data)[1] = "y_var"
  data$value <- factor(data$value, levels=unique(levels.list))
  
  data$variable <- factor(data$variable, levels=x)
  if(!is.null(metadata))
    if (x %in% metadata$Var)
      data$variable <- factor(metadata.var_to_labels(data$variable,metadata), 
                              levels=metadata.var_to_labels(x,metadata))

  
  g <- ggplot(data, aes(x=value, y=y_var))+ #theme_gray()+
    facet_wrap(~variable, scales = scales, nrow = nrow, labeller = label_wrap_gen(width=strip.wrap ))+
    #geom_boxplot(aes(fill=value))+
    #geom_boxplot(aes(fill=variable))+
    # stat_summary(fun.ymax = max, fun.ymin = min, na.rm=T, geom="linerange", color="red", size=0.45)+
    geom_boxplot(fill=colors, size=0.5, varwidth = F, coef=1.5, # errors if Inf
                 outlier.alpha = 0.5, outlier.stroke = 0.5, outlier.shape = 3, outlier.colour = "red")+
    stat_summary(fun.y = mean, na.rm=T, geom="point", pch=23, fill="white")+
    #geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3)+
    #geom_smooth(method = "lm", se=F, aes(group=1), fullrange=T) +
    labs(y=y)
  
  return (g)
}

multiple_boxplot_wrapper <- function(data, x, y, parse_factors=T, 
                                     palette="auto", palette.direction="auto", 
                                     metadata=NULL, trim_to_metadata = T,
                                     axis.text.x.orientation="vertical", strip.wrap=20, label.x.wrap=10, scales = "free_x", nrow = 1) {
  if(is.null(label.x.wrap)) if(is.na(label.x.wrap)) if(label.x.wrap <= 0) label.x.wrap = 10
  
  x = x[y != x]
  if(trim_to_metadata==T) # metadata has to exist for this one
    x = intersect(x, metadata$Var[metadata$Role %!in% c("exclude", "numeric", "id", "separate")])
  
  if (is.null(metadata)) metadata <- update_var_metadata(db, metadata, y)
  if (y %!in% metadata$Var) metadata <- update_var_metadata(db, metadata, y)
  
  require(grid)
  q <- data %>% 
    multiple_boxplot(x = x, y = y, parse_factors=parse_factors, 
                     palette=palette, palette.direction=palette.direction, metadata = metadata, strip.wrap=strip.wrap, scales = scales, nrow = nrow)+
    scale_x_discrete(labels=scales::wrap_format(label.x.wrap))+
    scale_y_continuous(metadata.var_to_labels(y, metadata), 
                       limits=c(metadata$Min[metadata$Var==y],metadata$Max[metadata$Var==y]),
                       breaks=seq(metadata$Min[metadata$Var==y],metadata$Max[metadata$Var==y],metadata$W[metadata$Var==y]))+
    theme (#panel.spacing = unit(0.25, "lines"), # join
      #strip.background = element_blank(), # hide variable name background
      #strip.placement = "outside", # make it look like x axis label
      strip.placement = "bottom",
      axis.title.x = element_blank(), # hide oerall x axis label
      #axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
      legend.position = "none") 
  
  if (axis.text.x.orientation=="vertical") q <- q + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
  
  
  # Get the ggplot grob
  gt = ggplotGrob(q)
  
  # Check for the widths - you need to change those that are set to 1 null
  #gt$widths
  
  for (v in seq_along(x)) {
    #if (parse_fators==T) data[v][[1]] <- factor(data[v][[1]])
    temp.levels <- data[,x[v] ][[1]] %>% na.omit() %>% factor() %>% levels () %>% length()
    
    #print(paste(" ", 1+4*v, " ", temp.levels))
    gt$widths[1+4*v] = unit(temp.levels, "null")
  }
  
  # Replace the default widths with relative widths:
  
  
  # Draw the plot
  grid.newpage()
  grid.draw(gt)
}

my_histo <- function(db, numerica, categ=NULL, metadata=NULL, small=ifelse(length(!is.na(db[[numerica]]))<50, T, F), 
                     stack.reverse=T, log.correction=0.001, different.bins=F, closed="left") {
  
  # if (is.null(metadata))
    # metadata = make_metadata(db, numerica, existing.metadata = metadata)
  
  if (numerica %!in% metadata$Var)
    metadata = make_metadata(db, numerica, existing.metadata = metadata)
  

  numerica.metadata <- get_limits_from_metadata_2(db=db, var = numerica, metadata=metadata, log.correction = log.correction)
  min.numerica <- numerica.metadata$Min
  max.numerica <- numerica.metadata$Max
  w.numerica <- numerica.metadata$W
  breaks.numerica <- numerica.metadata$Breaks
  scale.numerica <- numerica.metadata$Scale
  labels.numerica <- numerica.metadata$Labels
  if (closed=="right") labels.numerica[-1] <- labels.numerica[-1] %+% "]"
  metadata <- numerica.metadata$Metadata
  
  bins <- breaks.numerica
  if(different.bins == T) 
    bins <- metadata$Bins[metadata$Var==numerica] %>% str_split(";", simplify = T) %>% as.numeric() %>% unique() %>% sort()
  
  if(!is.null(categ)) {
    g <- db %>% select(c(numerica, categ)) %>%
      set_colnames(c("numerica", "categ")) %>%
      mutate(id=row.names(db)) %>%
      ggplot() + aes(x = numerica) +
      # stat_bin(aes(label=scales::percent(..count../sum(..count..), 0.1)%>% str_replace_all("^0.0%", "")),
      #            position=position_stack(reverse=stack.reverse),
      #            geom="text", angle=0, vjust=-0.1, hjust=0.5, closed=closed, size=3, 
      #            breaks=seq(metadata$Min[metadata$Var==numerica], metadata$Max[metadata$Var==numerica], metadata$W[metadata$Var==numerica])) +
      stat_bin(aes(fill=categ), color="white", alpha=0.75, closed=closed, size=0.1, 
               position=position_stack(reverse=stack.reverse),
               breaks=bins) + 
      scale_fill_discrete(metadata.var_to_labels(categ, metadata))
    
    g <- g +
      ggpubr::stat_central_tendency(aes(color=categ), show.legend = F, geom="point", pch="|",size=5) +
      ggpubr::stat_central_tendency(aes(color=categ), show.legend = F, geom="point", pch="^",size=5, type="median") +
      scale_color_discrete(metadata.var_to_labels(categ, metadata))
    
  } else { 
    g <- db %>% select(c(numerica)) %>%
      set_colnames(c("numerica")) %>%
      mutate(id=row.names(db)) %>%
      ggplot() + aes(x = numerica) +
      stat_bin(color="white", alpha=0.75, closed=closed, size=0.1, fill="grey50",
               position=position_stack(reverse=stack.reverse),
               breaks=bins)
  }
  # return(g)
  g <- g +
    stat_bin(aes(label=scales::percent(..count../sum(..count..), 0.1)%>% str_replace_all("^0.0%", "")),
             position=position_stack(reverse=stack.reverse),
             geom="text", angle=0, vjust=-0.1, hjust=0.5, closed=closed, size=3,
             breaks=bins)
  
  if(small==T) g <- g + stat_bin(aes(group=`id`), color="white", alpha=0.0, closed=closed, size=0.1, 
                                 position=position_stack(reverse=stack.reverse),
                                 breaks=bins)
  
  g <- g +
    #geom_vline(xintercept = mean(db[[numerica]], na.rm=T), linetype='solid', lwd=0.25)+
    geom_vline(xintercept = median(db[[numerica]], na.rm=T), linetype='dashed')+
    scale_y_continuous(breaks=seq(0,nrow(db), ifelse(small==T, 1, 10))) +
    #scale_fill_brewer(palette = "Set1")+
    labs(x = metadata.var_to_labels(numerica, metadata), y="Nr. pacienți") +
    theme(    legend.direction = "vertical",
              panel.grid.minor.x = element_blank(),
              plot.margin = margin(t=5, b = 5, r = 0, l = 0, unit = "pt"))
  
  
  if(scale.numerica %in% c("log2", "log10")) {
    g <- g + scale_x_log10(limits=c(min.numerica-log.correction, max.numerica+log.correction),
                           breaks=breaks.numerica, labels=labels.numerica) +
      geom_vline(xintercept = DescTools::Gmean(db[[numerica]], na.rm=T), linetype='solid', lwd=0.25)
  } else {
    g <- g + scale_x_continuous(limits=c(min.numerica, max.numerica),
                                breaks=breaks.numerica, labels=labels.numerica) +
      geom_vline(xintercept = mean(db[[numerica]], na.rm=T), linetype='solid', lwd=0.25)
  }
  
  
  g
}





GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

### 

my_box <- function(db, y, x, z=NULL, metadata=NULL, rug=F, vio=F, vio.trim=T,
                   box.width = 0.5, dodge=box.width, vio.dodge=0, vio.scale_factor=2.5,
                   log.correction=0.001, dot=F, draw_mean=T, draw_mean_sd = F,
                   varwidth = F, flip = F,
                   range=T, coef_treshold=1.5,
                   outlier.size = 2, outlier.alpha = 0.5, outlier.stroke = 0.5, outlier.shape = 3, outlier.colour = "red") {
  if(!is.null(z)) {
    db %<>% 
      select(c(y, x, z)) %>% set_colnames(c("y", "x", "z")) %>% 
      mutate(`ID` = 1:nrow(db)) %>%
      na.omit() 
  } else {
    db %<>% 
      select(c(y, x)) %>% set_colnames(c("y", "x")) %>%
      mutate(`ID` = 1:nrow(db)) %>%
      na.omit()
  }
  
  y.metadata <- get_limits_from_metadata_1(db=db, var = y, metadata=metadata, log.correction = log.correction)
  min.y <- y.metadata$Min
  max.y <- y.metadata$Max
  w.y <- y.metadata$W
  breaks.y <- y.metadata$Breaks
  scale.y <- y.metadata$Scale
  labels.y <- y.metadata$Labels
  metadata <- y.metadata$Metadata
  db$y <- y.metadata$Y


  
  if(!is.null(z)) {
    db %<>% 
      #select(c(y, x, z)) %>% set_colnames(c("y", "x", "z")) %>%
      #na.omit() %>% 
      group_by(x, z) %>%
      mutate(outlier.high = ifelse(y > quantile(y, .75) + coef_treshold*IQR(y), y, NA), 
             outlier.low = ifelse(y < quantile(y, .25) - coef_treshold*IQR(y), y, NA),
             outlier = ifelse(is.na(outlier.high), ifelse(is.na(outlier.low), NA, outlier.low), outlier.high)) %>% 
      ungroup() 
    
    g <- db %>% ggplot() + aes(y=`y`, x=`x`, fill=`z`, color=`z`) + 
      scale_fill_discrete(metadata.var_to_labels(z, metadata)) + 
      scale_color_discrete(metadata.var_to_labels(z, metadata))
    
    attr(g, "columns") <- length(levels(interaction(db[[x]], db[[z]], drop = F)))
    
  } else {
    db %<>%  
      #select(c(y, x)) %>% set_colnames(c("y", "x")) %>%
      #na.omit() %>%
      group_by(x) %>%
      mutate(outlier.high = ifelse(y > quantile(y, .75) + coef_treshold*IQR(y), y, NA), 
             outlier.low = ifelse(y < quantile(y, .25) - coef_treshold*IQR(y), y, NA),
             outlier = ifelse(is.na(outlier.high), ifelse(is.na(outlier.low), NA, outlier.low), outlier.high)) %>% 
      ungroup()  
    
    g <- db %>% ggplot() + aes(y=`y`, x=`x`, fill=`x`, color=`x`) + 
      scale_fill_discrete(metadata.var_to_labels(x, metadata)) + 
      scale_color_discrete(metadata.var_to_labels(x, metadata))
    
    attr(g, "columns") <- length(levels(db[[x]]))
  }
  
  if (vio==T)
    g <- g + geom_violin(position = position_dodge(vio.dodge), alpha=0.1, color="black", size=0.2, trim=vio.trim, width=box.width*vio.scale_factor)
  else if (vio=="split")
    g <- g + geom_split_violin(position = position_dodge(vio.dodge), alpha=0.1, color="black", size=0.2, trim=vio.trim, width=box.width*vio.scale_factor)
  
  if(range==T)
    g <- g + stat_summary(fun.ymax = max, fun.ymin = min, na.rm=T, geom="linerange", color=outlier.colour, size=0.45, 
                          position = position_dodge2(dodge))
  
  g <- g + geom_boxplot(color="black", size=0.5, 
                       varwidth = varwidth, width=box.width, position = position_dodge(dodge), 
                       coef=coef_treshold, # errors if Inf
                       outlier.size = 0, outlier.stroke = 0, outlier.alpha = 0 )
  
  if (sum(!is.na(db$outlier)) > 0 ) if (dot == F)
    g <- g + geom_point(aes(y=`outlier`), 
                        shape = outlier.shape, size = outlier.size, stroke = outlier.stroke, 
                        #fill = outlier.fill, 
                        colour = outlier.colour, alpha = outlier.alpha, 
                        position = position_jitterdodge(dodge.width = dodge, jitter.height = dodge/2)) 
  
  
  if (draw_mean==T)  {
    # if (draw_mean_sd==T)
    #   g <- g +
    #     stat_summary(fun.y = mean, na.rm=T, geom="point", pch=23, fill="white", size=2.5, position = position_dodge2(dodge)) +
    #     stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "linerange",
    #                  # position=position_nudge(x = 0.2),
    #                  position = position_dodge2(dodge+0.2),
    #                  width=0.1, color="black")
    # else
      g <- g + stat_summary(fun.y = mean, na.rm=T, geom="point", pch=23, fill="white", size=2.5, position = position_dodge2(dodge))
    
  }
  
  if(rug %in% c("l", "r")) g <- g + 
    geom_rug(sides=rug, outside=F, alpha=0.25) #+ geom_violin(alpha=0.1, size=0.25)
  if(dot == T) g <- g +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1, color="black", alpha=0.5, binwidth = 1/nrow(db), na.rm = T, show.legend = F)
  
  g <- g +
    scale_y_continuous(metadata.var_to_labels(y, metadata),
                     breaks=breaks.y,
                     labels = labels.y,
                     limits=c(min.y, max.y)) +
    scale_x_discrete(metadata.var_to_labels(x, metadata)) +
    theme(legend.key.size = unit(7, "mm"), 
          legend.direction = "horizontal", 
          plot.margin = margin(t=5, b = 5, r = 0, l = 0, unit = "pt"))
  
  if (flip == T) g <- g + coord_flip() + theme_reverse_grid
  
  g
}

my_pie <- function(data, var, facet=NULL, metadata=NULL, na.rm=T, p.fisher=F, 
                   line.color = "white", lwd=0.25,
                   angle=0, clockwise = T, 
                   two.lines.labels = T, add.facet.name=F, legend.height=1.2) {
  if(!is.null(facet)) {
    if (add.facet.name==T) my_labeller = labeller(`facet` = label_prefix(my_prefix = metadata.var_to_labels(facet, metadata), sep = ": ", wrap=25))
    else my_labeller = labeller(`facet` = label_wrap_gen(25))
    
    g <- data %>% 
      select(c(var, facet)) %>%
      set_colnames(c("var", "facet"))
    
    if (na.rm==T) g <- na.omit(g)
    
    g <- g %>% group_by(`facet`, `var`) %>%
      summarise(`Freq` = n()) %>%
      mutate(
        `Relaive Freq` = Freq/sum(Freq),
        `Freq%` = scales::percent(`Relaive Freq`, 0.1),
        `Label` = paste(`var`, ": ", `Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
      ggplot() + aes(x="", fill=`var`, y=`Relaive Freq`, label=`Label`) +
      facet_wrap(.~`facet`, labeller = my_labeller ) + 
      pie_theme() + 
      theme(legend.position = c(1, legend.height), # defalut: 1.25
            legend.justification = c(1, 1))
    
    if (p.fisher==T)
      g <- g + labs(caption = paste0("Fisher: ", scales::pvalue(fisher.test(table(data[[var]], data[[facet]]))$p.value, add_p = T)))
    
  } else {
    g <- data %>% 
      select(c(var)) %>%
      set_colnames("var") 
    
    if (na.rm==T) g <- na.omit(g)
    
    g <- g %>% group_by(`var`) %>%
      summarise(`Freq` = n()) %>%
      mutate(
        `Relaive Freq` = Freq/sum(Freq),
        `Freq%` = scales::percent(`Relaive Freq`, 0.1),
        `Label` = paste(`var`, ": ", `Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
      ggplot() + aes(x="", fill=`var`, y=`Relaive Freq`, label=`Label`) + 
      pie_theme() + 
      theme(legend.position = c(-0.05, 0.98)) # defalut: 1.25
    #+ theme(plot.background = theme_get()$panel.background)
  }
  
  g <- g + geom_bar(stat = "identity", color=line.color, alpha=0.75, lwd=lwd, 
                    position = position_stack(reverse = clockwise)) +
    geom_text(position = position_stack(vjust = 0.5, reverse = clockwise), size=3) +
    scale_y_continuous(breaks = 1:10 / 10) +
    labs(subtitle = metadata.var_to_labels(var, metadata)) +
    coord_polar("y", start = angle)# +    pie_theme 
  
  
  
  # if (!is.null(metadata))
  #   #g <- g + guides(fill=guide_legend(metadata.var_to_labels(var, metadata), nrow=2))
  #   g <- g + labs(subtitle = metadata.var_to_labels(var, metadata))
  # else
  #   #g <- g + guides(fill=guide_legend(var))
  #   g <- g + labs(subtitle = var)
  
  #guides(fill=guide_legend(metadata.var_to_labels(var, metadata)))+
  #scale_fill_brewer(metadata.var_to_labels("Nivelul fracturii", metadata), palette="Set2", direction = -1)+
  
  
  g
}

my_bar <- function(data, var, facet=NULL, metadata=NULL, fill=NULL, stack.reverse=F, fill.drop = T, x.drop=F, 
                   flip=T, wrap.labels = 10, wrap.title=80, add.facet.name=T, add.facet.name.to.facet = F, two.lines.labels=T,
                   p="fisher",
                   perpendicular.labels.x.treshold=25, line.color = "black", lwd=0) {
  
  # var
  # var, fill
  # var, facet
  # var, fill, facet
  
  if( is.null(fill) ) { fill <- "..NULL.." }
  else if (fill %in% c(names(data) ) ) {
    fill_var <- fill
    if (fill == var) { fill <- "..var.." }
    else if (!is.null(facet) && fill == facet) { fill <- "..facet.." }
    else { fill <- "..extra.." }
  } 
  else if (fill %in% c("..count..", "..prop..")) zzz <- 1 # do nothing
  else { # suppose fill is a color
    fill_color <- fill
    fill <- "..color.."
  }
  
  if (is.null(facet) & fill != "..extra..") { # case 1: only main var
    df <- data %>% 
      select(c(var)) %>% na.omit() %>%
      set_colnames("var") %>% 
      group_by(`var`) %>%
      summarise(`Freq` = n()) %>%
      mutate(
        `Relative Freq` = Freq/sum(Freq),
        `Freq%` = scales::percent(`Relative Freq`, 0.1),
        `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep=""))
    
    if (fill == "..NULL..")
      g <- ggplot(df) + aes(x=`var`, fill=`var`, y=`Relative Freq`, label=`Label`) +
        geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd) +
        no_legend
    else if (fill=="..var..")
      g <- ggplot(df) + aes(x=`var`, fill=`var`, y=`Relative Freq`, label=`Label`)+
        geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd) +
        no_legend
    # else if (fill=="..facet..")
    #   g <- ggplot(df) + aes(x=`var`, fill=`facet`, y=`Relative Freq`, label=`Label`)+
    #     geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd)
    else if (fill == "..count..")
      g <- ggplot(df) + aes(x=`var`, fill=`Freq`, y=`Relative Freq`, label=`Label`)+
        geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd)+
        scale_fill_continuous("N")
    else if (fill == "..prop..")
      g <- ggplot(df) + aes(x=`var`, fill=`Relative Freq`, y=`Relative Freq`, label=`Label`)+
        geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd) +
        scale_fill_continuous(name = NULL, breaks=seq(0, 1, 0.25), limits=c(0, 1), labels=scales::percent_format(1))
    else if(fill == "..color..")
      g <- ggplot(df) + aes(x=`var`, y=`Relative Freq`, label=`Label`)+
        geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd, fill=fill_color) +
        no_legend
    else
      g <- ggplot(df) + aes(x=`var`, y=`Relative Freq`, label=`Label`)+
        geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd) +
        no_legend
  }
  else if (is.null(facet) & fill == "..extra..") { # case 2: var and a fill variable
    df <- data %>% 
      select(c(var, fill_var)) %>% #na.omit() %>%
      set_colnames(c("var", "fill_var")) %>%
      filter(!is.na(`var`))
    
    db <- df
    if(fill.drop == T) {
      df <- filter(df, !is.na(`fill_var`))
      db <- droplevels(df)
    } 
    
    #db <- df
    
    # subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(table(var, fill_var)))$p.value, add_p=T)
    mat <- with(droplevels(db), table(var, fill_var))
    if (all.equal(dim(mat), c(2, 2)) == T) {
      if (p=="none") subtitle = ""
      else if (p=="fisher") subtitle = "Fisher " %+% scales::pvalue(with(droplevels(db), fisher.test(mat))$p.value, add_p=T)
      else if (p=="pearson") subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(mat, correct = F))$p.value, add_p=T)
      else if (p=="yates") subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(mat, correct = T))$p.value, add_p=T)
      else if (p=="chi2") subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(mat))$p.value, add_p=T)
      else subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(mat))$p.value, add_p=T)
    }
    else {
      if (p=="none") subtitle = ""
      else if (p=="pearson") subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(mat, correct = F))$p.value, add_p=T)
      else if (p=="yates") subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(mat, correct = T))$p.value, add_p=T)
      else if (p=="chi2") subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(mat))$p.value, add_p=T)
      else subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(mat))$p.value, add_p=T)
    }
    # subtitle = "Chi² " %+% scales::pvalue(with(droplevels(db), chisq.test(table(var, fill_var)))$p.value, add_p=T)
    
    g <- ggplot(df) + aes(x=`var`)+
      #geom_bar(stat = "identity", color="black", alpha=0.75, size=lwd, aes(fill=`fill_var`))+
      stat_count(aes(fill=`fill_var`, y=..count../sum(..count..)), color=line.color, alpha=0.75, size=lwd, 
                 position = position_stack(reverse = stack.reverse)) +
      scale_fill_discrete(str_wrap(metadata.var_to_labels(fill_var, metadata), 25)) +
      labs(subtitle = subtitle )
    #stat_count(aes(group=`id`), color="white", fill="transparent", size=0.1)+
    
    df_labels <- df %>% group_by(`var`) %>%
      summarise(`Freq` = n()) %>%
      mutate(
        `Relative Freq` = Freq/sum(Freq),
        `Freq%` = scales::percent(`Relative Freq`, 0.1),
        `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep=""))
    
    if (flip==T) {
      g <- g + #stat_count(aes(label=scales::percent(..count../sum(..count..))), geom="text", hjust=-0.1, size=3) +
        geom_label(aes(x = `var`, y=`Relative Freq`, label=`Label`), inherit.aes = F, data=df_labels,
                   size=3, hjust=0,
                   label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
      
    } else { # vertical columns
      g <- g + geom_label(aes(x = `var`, y=`Relative Freq`, label=`Label`), inherit.aes = F, data=df_labels,
                          size=3, vjust=0, hjust=0.5,
                          label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
    }
    
    
  }
  else if (!is.null(facet) & fill != "..extra..") { # case 3: main var and facet
    db <- data %>% 
      select(c(var, facet)) %>% na.omit() %>%
      set_colnames(c("var", "facet"))
    
    df <- db %>% 
      group_by(`facet`, `var`) %>%
      summarise(`Freq` = n()) %>%
      mutate(
        `Relative Freq` = Freq/sum(Freq),
        `Freq%` = scales::percent(`Relative Freq`, 0.1),
        `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) 
    
    db <- droplevels(db)
    subtitle = "Chi² " %+% scales::pvalue(chisq.test(table(db$var, db$facet))$p.value, add_p=T)
    if (add.facet.name == T) subtitle = "vs. " %+% facet %+% ": " %+% subtitle
    
    if (fill == "..NULL..")
      g <- ggplot(df) + aes(x=`var`, fill=`var`, y=`Relative Freq`, label=`Label`) +
      geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd) +
      no_legend
    else if (fill=="..var..")
      g <- ggplot(df) + aes(x=`var`, fill=`var`, y=`Relative Freq`, label=`Label`)+
      geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd) +
      no_legend
    else if (fill=="..facet..")
      g <- ggplot(df) + aes(x=`var`, fill=`facet`, y=`Relative Freq`, label=`Label`)+
      geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd) +
      no_legend
    else if (fill == "..count..")
      g <- ggplot(df) + aes(x=`var`, fill=`Freq`, y=`Relative Freq`, label=`Label`)+
      geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd)+
      scale_fill_continuous("N")
    else if (fill == "..prop..")
      g <- ggplot(df) + aes(x=`var`, fill=`Relative Freq`, y=`Relative Freq`, label=`Label`)+
      geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd)+
      scale_fill_continuous(name=NULL, breaks=seq(0, 1, 0.25), limits=c(0, 1), labels=scales::percent_format(1))
    else if(fill == "..color..")
      g <- ggplot(df) + aes(x=`var`, y=`Relative Freq`, label=`Label`)+
      geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd, fill=fill_color) +
      no_legend
    else
      g <- ggplot(df) + aes(x=`var`, y=`Relative Freq`, label=`Label`)+
      geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd) +
      no_legend
    
    if (add.facet.name.to.facet==T) my_labeller = labeller(`facet` = label_prefix(my_prefix = metadata.var_to_labels(facet, metadata), sep = ": ", wrap=25))
    else my_labeller = labeller(`facet` = label_wrap_gen(25))
    
    g <- g + facet_wrap(.~`facet`, labeller = my_labeller)+
      labs(subtitle = subtitle )+
      theme(panel.spacing = unit(15 , 'pt'))
    
  }  
  else if (!is.null(facet) & fill == "..extra..") { # case 4: var, facet, fill
    df <- data %>% 
      select(c(var, fill_var, facet)) %>% na.omit() %>%
      set_colnames(c("var", "fill_var", "facet")) #%>% 
    
    # `facet`, `fill_var`, `var`
    # `facet`, `var`, `fill_var`
    # `fill_var`, `facet`, `var`
    # `var`, `facet`, `fill_var`
    # `fill_var`, `var`, `facet`
    # `var`, `fill_var`, `facet`
    
    
    # df_data <- df %>% group_by( `var`, `fill_var`, `facet`) %>%
    #   summarise(`Freq` = n()) %>%
    #   mutate(
    #     `Relative Freq` = Freq/sum(Freq),
    #     `Freq%` = scales::percent(`Relative Freq`, 0.1),
    #     `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep=""))
    
    df_data <- df %>% group_by(`facet`) %>%
      dplyr::group_map( ~ .x %>% group_by(`fill_var`, `var`) %>%
                          summarise(`Freq` = n()) %>% ungroup() %>%
                          mutate(`Relative Freq` = Freq/sum(Freq) ) )
    df_data <- dplyr::bind_rows(df_data,.id = "facet") %>% 
      mutate(`facet` = as.factor(`facet`))
    levels(df_data$`facet`) <- levels(df$`facet`)
    
    # print(df_data)
    
    db <- droplevels(df)
    subtitle = "Chi² " %+% scales::pvalue(chisq.test(table(db$var, db$facet))$p.value, add_p=T)
    if (add.facet.name == T) subtitle = "vs. " %+% metadata.var_to_labels(facet, metadata) %+% ": " %+% subtitle
    
    if (add.facet.name.to.facet==T) my_labeller = labeller(`facet` = label_prefix(my_prefix = metadata.var_to_labels(facet, metadata), sep = ": ", wrap=25))
    else my_labeller = labeller(`facet` = label_wrap_gen(25))
    
    g <- ggplot(df_data) + aes(x=`var`, y=`Relative Freq`) + facet_wrap(.~`facet`, labeller = my_labeller)+
      geom_bar(stat = "identity", color=line.color, alpha=0.75, size=lwd, aes(fill=`fill_var`), 
               position = position_stack(reverse = stack.reverse))+
      #stat_count(aes(fill=`fill_var`, y=..count../sum(..count..)), color="black", alpha=0.75, size=lwd) +
      scale_fill_discrete(str_wrap(metadata.var_to_labels(fill_var, metadata), 25)) +
      labs(subtitle = subtitle )+
      theme(panel.spacing = unit(15 , 'pt')) #+
    #geom_label(aes(label=`Freq`, color=`fill_var`), size=2, position=position_stack())
    #stat_count(aes(group=`id`), color="white", fill="transparent", size=0.1)+
    
    df_labels <- df %>% group_by(`facet`, `var`) %>%
      summarise(`Freq` = n()) %>%
      mutate(
        `Relative Freq` = Freq/sum(Freq),
        `Freq%` = scales::percent(`Relative Freq`, 0.1),
        `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) 
    
    if (flip==T) {
      g <- g + #stat_count(aes(label=scales::percent(..count../sum(..count..))), geom="text", hjust=-0.1, size=3) +
        geom_label(aes(x = `var`, y=`Relative Freq`, label=`Label`), inherit.aes = F, data=df_labels,
                   size=3, hjust=0,
                   abel.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
      
    } else { # vertical columns
      g <- g + geom_label(aes(x = `var`, y=`Relative Freq`, label=`Label`), inherit.aes = F, data=df_labels,
                          size=3, vjust=0, hjust=0.5,
                          label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
    }
    
  }
  
  
  
  
  
  
  if (flip==T) g <- g + coord_flip() + theme_reverse_grid
  
  if (fill != "..extra..") { 
    if (flip==T) {
      g <- g + geom_label(aes(y=`Relative Freq`), 
                          size=3, hjust=0,
                          label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
      
    } else { # vertical columns
      g <- g + geom_label(aes(y=`Relative Freq`), 
                          size=3, vjust=0, hjust=0.5, 
                          label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
    }
  }
  
  g <- g + #geom_bar(stat = "identity", color="black", alpha=0.75, size=lwd)+
    labs(x=str_trunc(str_wrap(metadata.var_to_labels(var, metadata), wrap.title), wrap.title*3.9),
         y="Cazuri")+
    scale_x_discrete(labels=scales::wrap_format(wrap.labels), drop = x.drop) + 
    scale_y_continuous(breaks=seq(0, 1, 0.1), 
                       labels = scales::percent_format(1),
                       expand = expand_scale(mult=c(0.01, 0.25)))
  #no_legend
  
  # if(str_length(paste(breaks.x, collapse = "")) > perpendicular.labels.x.treshold) g <- g + perpendicular.labels.x
  
  
  g
}

my_bar_multivar2 <- function(data, vars, facet=NULL, metadata=NULL, 
                            keep=c("da", "yes", "oui"), remove="##########", 
                            sort.bars="no", fill = "..count..",  stack.reverse = T,
                            percent.from="valid", label.percent.from=F, axis.limit = NA,
                            wrap.labels = 10, add.facet.name=F, facet.orientation = "horizontal",
                            two.lines.labels=F, 
                            line.color = "black", lwd=0, 
                            flip=T, lang="ro") {
  
  # sort.bars: "no", "asc.count", "desc.count", "asc.alpha", "desc.alpha", "reverse"
  
  # var
  # var, fill
  # var, facet
  # var, fill, facet
  
  fill_color = 'grey'
  # fill_var = NULL
  if( is.null(fill) ) { fill <- "..NULL.." }
  else if (fill %in% c(names(data) ) ) { # fill by variable
    fill_var <- fill
    if (!is.null(facet) && fill == facet) { fill <- "..facet.." }
    else { fill <- "..extra.." }
  } 
  else if (fill %in% c("..var..")) zzz <- 1 # fill by main vars, do nothing
  else if (fill %in% c("..count..", "..prop..")) zzz <- 1 # fill by computed numbers, do nothing
  else { # fill by predefiend color, suppose fill is a color
    fill_color <- fill
    fill <- "..color.."
  }
  
  # fill can be: 
    # ..NULL.. (no fill), 
    # ..color.. (fill_color = preset color), 
    # ..var.., 
    # ..facet.., ..extra.. (fill_var = facet, ?), 
    # ..count.., ..prop..
  
  if (is.null(facet) & fill != "..extra..") { # case 1: only main var
    df <- data %>% 
      select(c(vars)) %>% na.omit() %>%
      #set_colnames(paste0("var.", 1:length(vars))) %>% 
      tidyr::gather(key="VAR", value="danu", factor_key=F) %>%
      filter(!is.na(`danu`)) %>%
      mutate(`VAR` = str_remove_all(metadata.var_to_labels(`VAR`, metadata), remove) ) %>%
      mutate(`VAR` = factor(`VAR`, levels=str_remove_all(metadata.var_to_labels(vars, metadata), remove)  )) %>%
      group_by(`VAR`, `danu`) %>%
      summarise(`Freq` = n()) %>%
      mutate(`Relative Freq` = Freq/ifelse(percent.from=="valid", sum(Freq),
                                           ifelse(percent.from=="all", nrow(data),
                                                  ifelse(is.numeric(percent.from), percent.from,
                                                         0))),
             `Freq%` = scales::percent(`Relative Freq`, 0.1),
             `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
      filter(danu %in% keep) %>% ungroup() 
    
    df_labels = df
    
    if(sort.bars == "asc.count") {
      df <- mutate (df, `VAR` = as.character(`VAR`))
      df2 <- df %>% group_by(`VAR`) %>% arrange(`Freq`) 
      df <- df %>% mutate(`VAR` = factor(`VAR`, levels = df2$`VAR`))
    }
    else if(sort.bars == "desc.count") {
      df <- mutate (df, `VAR` = as.character(`VAR`))
      df2 <- df %>% group_by(`VAR`) %>% arrange(desc (`Freq`) )
      df <- df %>% mutate(`VAR` = factor(`VAR`, levels = df2$`VAR`))
    }
    else if(sort.bars == "asc.alpha") {
      df <- df %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`))))
    }
    else if(sort.bars == "desc.alpha") {
      df <- df %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`), decreasing = T)))
    }
    else if(sort.bars == "reverse") {
      df <- df %>% mutate(`VAR` = forcats::fct_rev(`VAR`))
    }
    
    
    g <- ggplot(df, aes(x=`VAR`, y=`Relative Freq`))
  }
  else if (is.null(facet) & fill == "..extra..") { # case 2: var and a fill variable
    df <- data %>% 
      select(c(vars, fill_var)) %>% #na.omit() %>%
      set_colnames(c(vars, "FILL_VAR")) %>% 
      tidyr::gather(key="VAR", value="danu", factor_key=F, -c("FILL_VAR")) %>%
      filter(!is.na(`danu`))  %>%
      mutate(`VAR` = str_remove_all(metadata.var_to_labels(`VAR`, metadata), remove) ) %>%
      mutate(`VAR` = factor(`VAR`, levels=str_remove_all(metadata.var_to_labels(vars, metadata), remove)  )) 
    
    df_data <- df %>%
      group_by(`FILL_VAR`, `VAR`, `danu`) %>%
      summarise(`Freq` = n()) %>% ungroup() 
    
    df_data$`sf` <- rep(NA, nrow(df_data))
    
    for (fv in as.character(levels(df_data$`VAR`))) {
      df_data$`sf`[df_data$`VAR` == fv] <- df[df$VAR == fv,] %>% na.omit() %>% nrow()
      #data %>% select(c(fv, fill_var)) %>% na.omit() %>% nrow
    }
    
    df_data %<>% 
      mutate(`Relative Freq` = Freq/ifelse(percent.from=="valid", `sf`,
                                           ifelse(percent.from=="all", nrow(data),
                                                  ifelse(is.numeric(percent.from), percent.from, 0))),
             `Freq%` = scales::percent(`Relative Freq`, 0.1),
             `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
      filter(danu %in% keep)  
    
    # df_data <- df %>%
    #   group_by(`FILL_VAR`, `VAR`, `danu`) %>%
    #   summarise(`Freq` = n()) %>%
    #   mutate(`Relative Freq` = Freq/ifelse(percent.from=="valid", sum(Freq),
    #                                        ifelse(percent.from=="all", nrow(data),
    #                                               ifelse(is.numeric(percent.from), percent.from, 0))),
    #          `Freq%` = scales::percent(`Relative Freq`, 0.1),
    #          `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
    #   filter(danu %in% keep) %>% ungroup() 
    
    df_labels <- df %>%
      group_by(`VAR`, `danu`) %>%
      summarise(`Freq` = n()) %>%
      mutate(`Relative Freq` = Freq/ifelse(percent.from=="valid", sum(Freq),
                                           ifelse(percent.from=="all", nrow(data),
                                                  ifelse(is.numeric(percent.from), percent.from, 0))),
             `Freq%` = scales::percent(`Relative Freq`, 0.1),
             `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
      filter(danu %in% keep) %>% ungroup() 
    
    
    
    if(sort.bars == "asc.count") {
      df_data <- mutate (df_data, `VAR` = as.character(`VAR`))
      df_data2 <- df_data %>% group_by(`VAR`) %>% summarise(x = sum(`Freq`)) %>% arrange(x) 
      df_data <- df_data %>% mutate(`VAR` = factor(`VAR`, levels = df_data2$`VAR`))
    }
    else if(sort.bars == "desc.count") {
      df_data <- mutate (df_data, `VAR` = as.character(`VAR`))
      df_data2 <- df_data %>% group_by(`VAR`) %>% summarise(x = sum(`Freq`)) %>% arrange(desc(x)) 
      df_data <- df_data %>% mutate(`VAR` = factor(`VAR`, levels = df_data2$`VAR`))
    }
    else if(sort.bars == "asc.alpha") {
      #df_data <- mutate (df_data, `VAR` = as.character(`VAR`))
      df_data <- df_data %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`))))
    }
    else if(sort.bars == "desc.alpha") {
      #df_data <- mutate (df_data, `VAR` = as.character(`VAR`))
      df_data <- df_data %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`), decreasing = T)))
    }
    else if(sort.bars == "reverse") {
      df_data <- df_data %>% mutate(`VAR` = forcats::fct_rev(`VAR`))
    }
    
    # df <- df_data
    # for (fv in df_labels$VAR) df$`Label`[df$VAR == fv] <- df_labels$`Label`[df_labels$VAR == fv]
    for (fv in df_labels$VAR) {
      # df$`Label`[df$VAR == fv] <- df_labels$`Label`[df_labels$VAR == fv]
      df_labels$`Relative Freq`[df_labels$VAR == fv] <- sum(df_data$`Relative Freq`[df_data$VAR == fv])
    }
    
    g <- ggplot(df_data, aes(x=`VAR`, y=`Relative Freq` ))+ #, label=`Label`
      theme(panel.spacing = unit(15 , 'pt'))
  }
  else if (!is.null(facet) & fill != "..extra.." ) { # case 3: main var and facet
    df <- data %>% 
      select(c(vars, facet)) %>% #na.omit() %>%
      set_colnames(c(vars, "facet")) %>% 
      tidyr::gather(key="VAR", value="danu", factor_key=F, -c("facet")) %>%
      filter(!is.na(`danu`))  %>%
      mutate(`VAR` = str_remove_all(metadata.var_to_labels(`VAR`, metadata), remove) ) %>%
      mutate(`VAR` = factor(`VAR`, levels=str_remove_all(metadata.var_to_labels(vars, metadata), remove)  )) %>%
      group_by(`facet`, `VAR`, `danu`) %>%
      summarise(`Freq` = n()) %>%
      mutate(`Relative Freq` = Freq/ifelse(percent.from=="valid", sum(Freq),
                                           ifelse(percent.from=="all", nrow(data),
                                                  ifelse(is.numeric(percent.from), percent.from,
                                                         0))),
             `Freq%` = scales::percent(`Relative Freq`, 0.1),
             `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
      filter(danu %in% keep) %>% ungroup() 
    
    df_labels = df
    
    if(sort.bars == "asc.count") {
      df <- mutate (df, `VAR` = as.character(`VAR`))
      df2 <- df %>% group_by(`VAR`) %>% summarise(x = sum(`Freq`)) %>% arrange(x) 
      df <- df %>% mutate(`VAR` = factor(`VAR`, levels = df2$`VAR`))
    }
    else if(sort.bars == "desc.count") {
      df <- mutate (df, `VAR` = as.character(`VAR`))
      df2 <- df %>% group_by(`VAR`) %>% summarise(x = sum(`Freq`)) %>% arrange(desc(x)) 
      df <- df %>% mutate(`VAR` = factor(`VAR`, levels = df2$`VAR`))
    }
    else if(sort.bars == "asc.alpha") {
      #df <- mutate (df, `VAR` = as.character(`VAR`))
      df <- df %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`))))
    }
    else if(sort.bars == "desc.alpha") {
      #df <- mutate (df, `VAR` = as.character(`VAR`))
      df <- df %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`), decreasing = T)))
    }
    else if(sort.bars == "reverse") {
      df <- df %>% mutate(`VAR` = forcats::fct_rev(`VAR`))
    }
    
    if (add.facet.name==T) my_labeller = labeller(`facet` = label_prefix(my_prefix = metadata.var_to_labels(facet, metadata), sep = ": ", wrap=25))
    else my_labeller = labeller(`facet` = label_wrap_gen(25))
    
    #g <- ggplot(df, aes(x=`VAR`, y=`Relative Freq`, label=ifelse(two.lines.labels==T, `Label2`, `Label`) ))+
    g <- ggplot(df, aes(x=`VAR`, y=`Relative Freq`))+
      theme(panel.spacing = unit(15 , 'pt'))
    
    if (facet.orientation == "horizontal")
      g <- g + facet_grid(.~`facet`, labeller = my_labeller )
    else
      g <- g + facet_grid(`facet`~., labeller = my_labeller )
  }  
  else if (!is.null(facet) & fill == "..extra.." ) { # case 4: var, facet, fill
    
  }
  
  
  
  if (fill == "..count..")
    g <- g + geom_bar(stat = "identity", alpha=0.75, aes(fill=`Freq`), color=line.color, size=lwd)
  else if (fill == "..prop..")
    g <- g + geom_bar(stat = "identity", alpha=0.75, aes(fill=`Relative Freq`), color=line.color, size=lwd)
  else if (fill == "..facet..") 
    g <- g + geom_bar(stat = "identity", alpha=0.75, mapping = aes(fill=`facet`), color=line.color, size=lwd)
  else if (fill == "..var..") 
    g <- g + geom_bar(stat = "identity", alpha=0.75, mapping = aes(fill=`VAR`), color=line.color, size=lwd)
  else if (fill == "..extra..") 
    g <- g + geom_bar(stat = "identity", alpha=0.75, 
                      mapping = aes(fill=`FILL_VAR`), 
                      position=position_stack(reverse=stack.reverse), 
                      color=line.color, size=lwd)
  else if (fill == "..color..")
    g <- g + geom_bar(stat = "identity", alpha=0.75, fill=fill_color, color=line.color, size=lwd)
  else g <- g + geom_bar(stat = "identity", alpha=0.75, fill=fill_color, color=line.color, size=lwd)
  
  if (flip==T) g <- g + coord_flip() + theme_reverse_grid +
      geom_label(aes(y=`Relative Freq`, label=`Label`), data=df_labels,
                 size=3, hjust=0,
                 label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
  else g <- g + 
      geom_label(aes(y=`Relative Freq`, label=`Label`), data=df_labels,
                 size=3, vjust=0, hjust=0.5, 
                 label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
  
  g <- g + 
    labs(x=NULL, y=switch(lang, 
                          "en" = "Cases" %+% ifelse (label.percent.from==T, 
                                                     ifelse(is.na(percent.from), "",
                                                            ifelse(percent.from == "all", " (% of all data)", 
                                                                   ifelse(percent.from == "valid", " (% of valid data)",
                                                                          ifelse(is.numeric(percent.from), " (% of " %+% percent.from  %+% " cases)",
                                                                                 "")))),  ""),
                          "ro" = "Cazuri" %+% ifelse (label.percent.from==T, 
                                                      ifelse(is.na(percent.from), "",
                                                             ifelse(percent.from == "all", " (% toate datele)", 
                                                                    ifelse(percent.from == "valid", " (% din datele disponibile)",
                                                                           ifelse(is.numeric(percent.from), " (% din " %+% percent.from  %+% " cazuri)",
                                                                                  "")))), ""),
                          "fr" = "Cases" %+% ifelse (label.percent.from==T, 
                                                     ifelse(is.na(percent.from), "",
                                                            ifelse(percent.from == "all", " (% de toute data)", 
                                                                   ifelse(percent.from == "valid", " (% de valid data)",
                                                                          ifelse(is.numeric(percent.from), " (% de " %+% percent.from  %+% " cases)",
                                                                                 "")))),  ""),
                          "Cases" ))
  
  if (is.na(axis.limit))
    g <- g + scale_y_continuous(breaks=seq(0, 1, ifelse(max(df$`Relative Freq`)<0.1, 0.01, 
                                                        ifelse(max(df$`Relative Freq`)<0.8, 0.1, 0.25))), 
                                labels = scales::percent_format(1),
                                expand = expand_scale(mult=c(0.01, 0.25)))
  else 
    g <- g + scale_y_continuous(breaks=seq(0, 1, ifelse(axis.limit<0.1, 0.01, 
                                                        ifelse(axis.limit<0.8, 0.1, 0.25))), 
                                limits=c(0, axis.limit),
                                labels = scales::percent_format(1),
                                expand = expand_scale(mult=c(0.01, 0.01)))
  
  if(!is.na(wrap.labels)) g <- g + scale_x_discrete(labels=scales::wrap_format(width = wrap.labels))
  
  g + no_legend
  
}

my_bar_multivar <- function(data, vars, facet=NULL, metadata=NULL, 
                            keep=c("da", "yes", "oui"), remove="##########", 
                            sort.bars="no", fill = "..count..", 
                            percent.from="valid", label.percent.from=F, axis.limit = NA,
                            wrap.labels = 10, add.facet.name=F, facet.orientation = "horizontal",
                            two.lines.labels=F, 
                            line.color = "black", lwd=0, 
                            flip=T, lang="ro") {
  
  # sort.bars: "no", "asc.count", "desc.count", "asc.alpha", "desc.alpha", "reverse"
  
  if (!is.null(facet)) {
    df <- data %>% 
      select(c(vars, facet)) %>% #na.omit() %>%
      set_colnames(c(vars, "facet")) %>% 
      tidyr::gather(key="VAR", value="danu", factor_key=F, -c("facet")) %>%
      filter(!is.na(`danu`))  %>%
      mutate(`VAR` = str_remove_all(metadata.var_to_labels(`VAR`, metadata), remove) ) %>%
      mutate(`VAR` = factor(`VAR`, levels=str_remove_all(metadata.var_to_labels(vars, metadata), remove)  )) %>%
      group_by(`facet`, `VAR`, `danu`) %>%
      summarise(`Freq` = n()) %>%
      mutate(`Relative Freq` = Freq/ifelse(percent.from=="valid", sum(Freq),
                                           ifelse(percent.from=="all", nrow(data),
                                                  ifelse(is.numeric(percent.from), percent.from,
                                                  0))),
             `Freq%` = scales::percent(`Relative Freq`, 0.1),
             `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
      filter(danu %in% keep) %>% ungroup() 
    
    
    if(sort.bars == "asc.count") {
      df <- mutate (df, `VAR` = as.character(`VAR`))
      df2 <- df %>% group_by(`VAR`) %>% summarise(x = sum(`Freq`)) %>% arrange(x) 
      df <- df %>% mutate(`VAR` = factor(`VAR`, levels = df2$`VAR`))
    }
    else if(sort.bars == "desc.count") {
      df <- mutate (df, `VAR` = as.character(`VAR`))
      df2 <- df %>% group_by(`VAR`) %>% summarise(x = sum(`Freq`)) %>% arrange(desc(x)) 
      df <- df %>% mutate(`VAR` = factor(`VAR`, levels = df2$`VAR`))
    }
    else if(sort.bars == "asc.alpha") {
      #df <- mutate (df, `VAR` = as.character(`VAR`))
      df <- df %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`))))
    }
    else if(sort.bars == "desc.alpha") {
      #df <- mutate (df, `VAR` = as.character(`VAR`))
      df <- df %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`), decreasing = T)))
    }
    else if(sort.bars == "reverse") {
      df <- df %>% mutate(`VAR` = forcats::fct_rev(`VAR`))
    }
    
    if (add.facet.name==T) my_labeller = labeller(`facet` = label_prefix(my_prefix = metadata.var_to_labels(facet, metadata), sep = ": ", wrap=25))
    else my_labeller = labeller(`facet` = label_wrap_gen(25))
    
    #g <- ggplot(df, aes(x=`VAR`, y=`Relative Freq`, label=ifelse(two.lines.labels==T, `Label2`, `Label`) ))+
    g <- ggplot(df, aes(x=`VAR`, y=`Relative Freq`, label=`Label` ))+
      theme(panel.spacing = unit(15 , 'pt'))
    
    if (facet.orientation == "horizontal")
      g <- g + facet_grid(.~`facet`, labeller = my_labeller )
    else
      g <- g + facet_grid(`facet`~., labeller = my_labeller )
    
  } else {
    df <- data %>% 
      select(c(vars)) %>% na.omit() %>%
      #set_colnames(paste0("var.", 1:length(vars))) %>% 
      tidyr::gather(key="VAR", value="danu", factor_key=F) %>%
      filter(!is.na(`danu`)) %>%
      mutate(`VAR` = str_remove_all(metadata.var_to_labels(`VAR`, metadata), remove) ) %>%
      mutate(`VAR` = factor(`VAR`, levels=str_remove_all(metadata.var_to_labels(vars, metadata), remove)  )) %>%
      group_by(`VAR`, `danu`) %>%
      summarise(`Freq` = n()) %>%
      mutate(`Relative Freq` = Freq/ifelse(percent.from=="valid", sum(Freq),
                                           ifelse(percent.from=="all", nrow(data),
                                                  ifelse(is.numeric(percent.from), percent.from,
                                                         0))),
             `Freq%` = scales::percent(`Relative Freq`, 0.1),
             `Label` = paste(`Freq`, ifelse(two.lines.labels==T, "\n(", " ("), `Freq%`, ")", sep="")) %>%
      filter(danu %in% keep) %>% ungroup() 
    
    if(sort.bars == "asc.count") {
      df <- mutate (df, `VAR` = as.character(`VAR`))
      df2 <- df %>% group_by(`VAR`) %>% arrange(`Freq`) 
      df <- df %>% mutate(`VAR` = factor(`VAR`, levels = df2$`VAR`))
    }
    else if(sort.bars == "desc.count") {
      df <- mutate (df, `VAR` = as.character(`VAR`))
      df2 <- df %>% group_by(`VAR`) %>% arrange(desc (`Freq`) )
      df <- df %>% mutate(`VAR` = factor(`VAR`, levels = df2$`VAR`))
    }
    else if(sort.bars == "asc.alpha") {
      df <- df %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`))))
    }
    else if(sort.bars == "desc.alpha") {
      df <- df %>% mutate(`VAR` = factor(as.character(`VAR`), levels = sort(levels(`VAR`), decreasing = T)))
    }
    else if(sort.bars == "reverse") {
      df <- df %>% mutate(`VAR` = forcats::fct_rev(`VAR`))
    }
    
    
    g <- ggplot(df, aes(x=`VAR`, y=`Relative Freq`, label=`Label`))
    
  }
  
  if (fill == "..count..")
    g <- g + geom_bar(stat = "identity", alpha=0.75, aes(fill=`Freq`), color=line.color, size=lwd)
  else if (fill == "..prop..")
    g <- g + geom_bar(stat = "identity", alpha=0.75, aes(fill=`Relative Freq`), color=line.color, size=lwd)
  else #if (fill == "..prop..")
    g <- g + geom_bar(stat = "identity", alpha=0.75, fill="grey", color=line.color, size=lwd)
  
  if (flip==T) g <- g + coord_flip() + theme_reverse_grid +
      geom_label(aes(y=`Relative Freq`), 
                 size=3, hjust=0,
                 label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
  else g <- g + 
      #geom_bar(stat = "identity", alpha=0.75, aes(fill=`Freq`), color="black", size=lwd)+
      geom_label(aes(y=`Relative Freq`), 
                 size=3, vjust=0, hjust=0.5, 
                 label.padding=unit(1, "mm"), label.size = unit(0, "mm"), fill="transparent")
  
  g <- g + 
    labs(x=NULL, y=switch(lang, 
            "en" = "Cases" %+% ifelse (label.percent.from==T, 
              ifelse(is.na(percent.from), "",
                     ifelse(percent.from == "all", " (% of all data)", 
                             ifelse(percent.from == "valid", " (% of valid data)",
                                    ifelse(is.numeric(percent.from), " (% of " %+% percent.from  %+% " cases)",
                                           "")))),  ""),
            "ro" = "Cazuri" %+% ifelse (label.percent.from==T, 
              ifelse(is.na(percent.from), "",
                     ifelse(percent.from == "all", " (% toate datele)", 
                            ifelse(percent.from == "valid", " (% din datele disponibile)",
                                   ifelse(is.numeric(percent.from), " (% din " %+% percent.from  %+% " cazuri)",
                                          "")))), ""),
            "fr" = "Cases" %+% ifelse (label.percent.from==T, 
              ifelse(is.na(percent.from), "",
                     ifelse(percent.from == "all", " (% de toute data)", 
                            ifelse(percent.from == "valid", " (% de valid data)",
                                   ifelse(is.numeric(percent.from), " (% de " %+% percent.from  %+% " cases)",
                                          "")))),  ""),
            "Cases" ))
  
  if (is.na(axis.limit))
    g <- g + scale_y_continuous(breaks=seq(0, 1, ifelse(max(df$`Relative Freq`)<0.1, 0.01, 
                                               ifelse(max(df$`Relative Freq`)<0.8, 0.1, 0.25))), 
                       labels = scales::percent_format(1),
                       expand = expand_scale(mult=c(0.01, 0.25)))
  else 
    g <- g + scale_y_continuous(breaks=seq(0, 1, ifelse(axis.limit<0.1, 0.01, 
                                                        ifelse(axis.limit<0.8, 0.1, 0.25))), 
                                limits=c(0, axis.limit),
                                labels = scales::percent_format(1),
                                expand = expand_scale(mult=c(0.01, 0.01)))
  
  if(!is.na(wrap.labels)) g <- g + scale_x_discrete(labels=scales::wrap_format(width = wrap.labels))
  
  g + no_legend
  
}



# Deming regression
Deming <- function(x, y){
  df  <- data.frame(y, x)
  pca <- prcomp(~x+y, df)
  slp <- with(pca, rotation[2,1] / rotation[1,1])
  int <- with(pca, center[2] - slp*center[1])
  
  c(int, slp)
}
# Define functions to pass to stat_smooth - see mnel's answer at link for details
# Defined the Deming model output as class Deming to define the predict method
# I only used the intercept and slope for predictions - is this correct?
deming <- function(formula,data,...){
  M <- model.frame(formula, data)
  d <- Deming(x =M[,2],y =M[,1])
  class(d) <- "Deming"
  d  
}
# an s3 method for predictdf (called within stat_smooth)
predictdf.Deming <- function(model, xseq, se, level) {
  pred <- model %*% t(cbind(1, xseq) )
  d <- data.frame(x = xseq, y = c(pred))
  d
}
# geom_smooth(fullrange=T, se=F, method = deming, size=0.5, color="black")


my_cor_pmat <- function (x, ...) 
{
  mat <- as.matrix(x)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1))  if (! all(is.na(mat[,i]))) {
    for (j in (i + 1):n)  if (sum(complete.cases(mat[,c(i,j)])) > 1) {
      # print(colnames(mat)[i] %+% " x " %+% colnames(mat)[j])
      tmp <- stats::cor.test(mat[, i], mat[, j], ...)
      # tmp <- stats::cor.test(mat[, i], mat[, j], method = "spearman", use="pairwise.complete.obs")
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

multicor <- function(db, x, y, metadata, method = "spearman", R2 = F, 
                     check_metadata=T,
                     wrap.x = 15, wrap.y = 30, legend.key.height = 7,
                     lab = T, lab_size = 2.75,
                     low = "blue", mid = "white", high="red", na.value = "grey",
                     breaks=seq(-1, 1, 0.25), limits=c(ifelse(R2==T, 0, -1), 1),
                     chart.flip=T,
                     font.face=NULL, t.2_lines=T, p.style = "p") {
  
  if(check_metadata==T) {
    x <- intersect(x, metadata$Var[metadata$Role=="numeric"])
    y <- intersect(y, metadata$Var[metadata$Role=="numeric"])
  }
  
  CORR <- cor(x = db[x], y=db[y],  method = method, use="pairwise.complete.obs")
  if(R2==T) CORR <- CORR^2
  # PMAT <- matrix(ggcorrplot::cor_pmat(db[c(x, y)],  method = method, use="pairwise.complete.obs")[x, y], nrow = length(x))
  PMAT <- matrix(my_cor_pmat(db[c(x, y)],  method = method, use="pairwise.complete.obs")[x, y], nrow = length(x))
  row.names(PMAT) <- x
  colnames(PMAT) <- y
  
  for (i in x) for (j in y) if (i==j) {
    CORR[i, j] = Inf
    PMAT[i, j] = 0
  }
  
  xlabs <- metadata.var_to_labels(x, metadata)
  ylabs <- metadata.var_to_labels(y, metadata)
  
  # part 1: table
  
  if (p.style == "p") 
    t <- (scales::number(CORR, 0.001) %+% ifelse(t.2_lines==T, "\n(", " (") %+% scales::pvalue(PMAT, add_p = T)  %+% ")" )
  else if (p.style == "pstars")
    t <- (scales::number(CORR, 0.001) %+% pstars(PMAT) )
  else
    t <- scales::number(CORR, 0.001)
  
  t %<>% 
    matrix(nrow=length(x)) %>% 
    as.data.frame.matrix() %>%
    mutate_all(str_remove_all, "Inf.*") %>%
    mutate_all(str_replace_all, "NA.*", "NA") %>%
    mutate(`R` = xlabs) %>%
    set_colnames(c(ylabs, "VS. (" %+% capitalize(method) %+% " R)") ) %>%
    select(c("VS. (" %+% capitalize(method) %+% " R)", ylabs))  %>% 
    flextable::flextable() %>%
    flextable::theme_booktabs(10)
  
  if (p.style == "pstars") {
    t %<>% flextable::add_footer_lines("p-value = ⁺: <0.10, < *: <0.05*, **: < 0.01, ***: <0.001") %>%
      flextable::border(part = "footer", border.bottom = officer::fp_border(color = "black", style = "solid", width = 1)) %>%
      flextable::italic(part = "footer")
    }
  
  if (is.null(font.face)) {
    if (exists("MAIN.FONT")) { font.face = MAIN.FONT
    } else {font.face = "Arial"} }
  
  t %<>% flextable::font(fontname = font.face, part="all") %>%
    flextable::bold(part="header") %>%
    flextable::align(align="left", part = "all") %>%
    flextable::height_all(0.2) %>%
    flextable::padding(padding = 0) %>%
    flextable::autofit(add_w = 0, add_h = 0) %>%
    flextable::fix_border_issues() 
  
 # part 2: chart
  
  CORR[is.na(CORR)] <- Inf
  PMAT[is.na(PMAT)] <- 1
  CORR <- CORR[rev(row.names(CORR)),]
  PMAT <- PMAT[rev(row.names(PMAT)),]
  ylabs <- rev(ylabs)
  
  if (chart.flip==T) {
    CORR <- t(CORR)
    PMAT <- t(PMAT)
    xlabs <- metadata.var_to_labels(y, metadata)
    ylabs <- rev(metadata.var_to_labels(x, metadata))
  }
  
  p <- ggcorrplot::ggcorrplot(corr = CORR, p.mat = PMAT,
                              lab = lab, lab_size = lab_size, 
                              outline.color = "black",
                              legend.title = "R\n" %+% capitalize(method),
                              ggtheme = ggpubr::theme_pubclean(10, "Arial")) + 
    scale_x_discrete(labels = str_wrap(xlabs, wrap.x))+ 
    scale_y_discrete(labels = str_wrap(ylabs, wrap.y))+  
    scale_fill_gradient2("R\n" %+% capitalize(method), breaks=breaks, limits=limits, 
                         low = low, mid = mid, high=high, na.value = na.value)+
    my_theme() + theme(
      text = element_text(color="black"),
      #title = element_text(size = base_size*0.9, face="italic", color="black"),
      #plot.subtitle = element_text(size = base_size*0.9, face="italic", color="black"),
      #plot.caption = element_text(size = base_size*0.9, face="italic", color="black"),
      
      #axis.text = element_text(size = base_size*0.9, color="black"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      
      legend.background = element_blank(), 
      legend.key.height = unit(legend.key.height, "mm"), 
      legend.key = element_blank(), 
      legend.position = "right" ) +
    #legend.bottom_left
    perpendicular.labels.x
  
  return (list(p = p, t = t))
}




