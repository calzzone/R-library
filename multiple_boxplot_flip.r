library(ggplot2)
#library(tidyr)
#library(reshape2)
library(dplyr)
library(scales)
library(grid)

multiple_boxplot_flip <- function(data, x, y, parse_factors=T, tufte=F) {
  #x = c("cyl", "vs", "am", "gear")
  #y = "mpg"
  #data <- mtcars[, c(y, x)]
  colors <- c()
  levels.list <- c()
  data <- data[, c(y, x)]
  for (v in 1+seq_along(x)) {
    if (parse_factors==T) data[v][[1]] <- factor(data[v][[1]])
    temp.levels <- data[v][[1]] %>% na.omit() %>% factor() %>% levels ()
    levels.list <- c(levels.list, temp.levels )
    #colors <- c(colors, temp.levels %>% length() %>% scales::brewer_pal(palette = "Greens")())
    colors <- c(colors, temp.levels %>% length() %>% scales::hue_pal()())
  }
  #data <- reshape2::melt(data, id.vars=y) 
  data <- tidyr::gather(data, variable, value, -y) %>% na.omit() 
  colnames(data)[1] = "y_var"
  data$value <- factor(data$value, levels=unique(levels.list))
  data$variable <- factor(data$variable, levels=x)
  
  g <- ggplot(data, aes(x=value,y=y_var)) + theme_gray()+
    coord_flip()
  
  if (tufte==T) {
    g <- g + facet_wrap(~variable, scales = "free_y", ncol = 1, strip.position = "top") + 
      geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3, color=colors)
  } else {
    g <- g + facet_wrap(~variable, scales = "free_y", ncol = 1, strip.position = "left", labeller = label_wrap_gen(width=10 )) +
      geom_boxplot(fill=colors, outlier.alpha = 0.5, outlier.stroke = 0.5, size=0.5, varwidth = F) 
  }
    
  g <- g + labs(y=y) + # bottom title
    theme(axis.title.y = element_blank()) # left title
  
  return (g)
}


x = c("Sex", "Mediu", "Fumat", "ADO", "ADI", "Insulina","RD", "ND")
y = "HbA1c (%)" 
db <- baza_de_date %>% rename(RD=`Retinopatie diabetica`, ND=`Neuropatie diabetica`)
tufte = F
g <- multiple_boxplot_flip(db, x, y, parse_factors=F, tufte=tufte) +
  theme (#panel.spacing = unit(0.25, "lines"), # join
    #strip.background = element_blank(), # hide variable name background
    #strip.placement = "left", # like "outside"
    legend.position = "none") 


gt = ggplotGrob(g)
#gt$heights
for (v in seq_along(x)) {
  temp.levels <- db[,x[v] ][[1]] %>% na.omit() %>% factor() %>% levels () %>% length()
  if (tufte==T) {
    # settings for tufte:
    #print(paste(" ", 3+5*v, " ", temp.levels))
    gt$heights[3+5*v] = unit(temp.levels, "null")
  } else {
    # settings for box:
    #print(paste(" ", 3+4*v, " ", temp.levels))
    gt$heights[3+4*v] = unit(temp.levels, "null")
  }  
}

# Draw the plot
grid.newpage()
grid.draw(gt)