library(ggplot2)
#library(tidyr)
#library(reshape2)
library(dplyr)
library(scales)
library(grid)

multiple_boxplot <- function(data, x, y, parse_factors=T) {
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
  
  g <- ggplot(data, aes(x=value,y=y_var))+ theme_gray()+
    facet_wrap(~variable, scales = "free_x", nrow = 1, labeller = label_wrap_gen(width=10 ))+
    #geom_boxplot(aes(fill=value))+
    #geom_boxplot(aes(fill=variable))+
    geom_boxplot(fill=colors, outlier.alpha = 0.5, outlier.stroke = 0.5, size=0.5, varwidth = F)+
    #geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3)+
    #geom_smooth(method = "lm", se=F, aes(group=1), fullrange=T) +
    labs(y=y)
  
  return (g)
}

x = c("Sex", "Mediu", "Fumat", "ADO", "ADI", "Insulina","RD", "ND")
y = "HbA1c (%)" 
db <- baza_de_date %>% rename(RD=`Retinopatie diabetica`, ND=`Neuropatie diabetica`)
#multiple_boxplot(mtcars, x=c("cyl", "vs", "am", "gear"), y="mpg") + 
#multiple_boxplot(baza_de_date, x=c("AT1 Polymorphism", "ECG", "Sex", "HBP", "Obesity"), y="BMI (kg/m²)", parse_fators = F) + 
q <- multiple_boxplot(db, x, y, parse_factors=F ) +
  #ggpubr::theme_pubclean()+ 
  theme_grey()+
  theme (#panel.spacing = unit(0.25, "lines"), # join
    #strip.background = element_blank(), # hide variable name background
    #strip.placement = "outside", # make it look like x axis label
    strip.placement = "bottom",
    axis.title.x = element_blank(), # hide oerall x axis label
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
    legend.position = "none") 


# Get the ggplot grob
gt = ggplotGrob(q)

# Check for the widths - you need to change those that are set to 1 null
#gt$widths

for (v in seq_along(x)) {
  #if (parse_fators==T) data[v][[1]] <- factor(data[v][[1]])
  temp.levels <- db[,x[v] ][[1]] %>% na.omit() %>% factor() %>% levels () %>% length()
  #print(paste(" ", 1+4*v, " ", temp.levels))
  gt$widths[1+4*v] = unit(temp.levels, "null")
}

# Replace the default widths with relative widths:


# Draw the plot
grid.newpage()
grid.draw(gt)


