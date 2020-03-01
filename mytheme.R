# some theming shortcuts
no_legend = theme(legend.position = "none")
legend.none = theme(legend.position = "none")
legend.top = theme(legend.position = "top", legend.justification = "center")
legend.bottom = theme(legend.position = "bottom", legend.justification = "center")
legend.right = theme(legend.position = "right", legend.justification = "center")
legend.left = theme(legend.position = "left", legend.justification = "center")
legend.top_right = theme(legend.position = c(1, 1), legend.justification = c(1, 1))
legend.top_left = theme(legend.position = c(0, 1), legend.justification = c(0, 1))
legend.bottom_right = theme(legend.position = c(1, 0), legend.justification = c(1, 0))
legend.bottom_left = theme(legend.position = c(0, 0), legend.justification = c(0, 0))
no_title.legend = theme(legend.title = element_blank())
legend.no_title = theme(legend.title = element_blank())

perpendicular.labels.x = theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
no.title.x = theme(axis.title.x = element_blank())
no.title.y = theme(axis.title.y = element_blank())
no.labels.x = theme(axis.text.x = element_blank())
no.labels.y = theme(axis.text.y = element_blank())
no.ticks.x = theme(axis.ticks.x = element_blank())
no.ticks.y = theme(axis.ticks.y = element_blank())
no.grid.x <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
no.grid.y <- theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
no.minor_grid.x <- theme(panel.grid.minor.x = element_blank())
no.minor_grid.y <- theme(panel.grid.minor.y = element_blank())

if (exists("MAIN.FONT")) { font.face = MAIN.FONT
} else {font.face = "Arial"}
update_geom_defaults("text", list(family = ifelse(startsWith(font.face, "Times"), ifelse(startsWith(font.face, "Arial"), "Arial", "Times New Roman"), font.face )))
update_geom_defaults("label", list(family = ifelse(startsWith(font.face, "Times"), ifelse(startsWith(font.face, "Arial"), "Arial", "Times New Roman"), font.face )))

library(ggplot2)
theme_spss <- function(base_family = font.face, base_size=12) {
  theme_minimal()+ theme(text = element_text(family = base_family, face = "bold"),
                         axis.ticks = element_line(colour = "black", size = 1),
                         panel.background = element_rect(colour = "black",
                                                         fill = "gray90", size = 1.5),
                         panel.grid = element_blank())
}

# My main theme is based on ggpubr::theme_pubclean

my_theme <- function(base_family= font.face, base_size=10, base_theme = theme_grey) {
  base_theme(base_size = base_size, base_family = base_family) + 
    theme(
      text = element_text(color="black"),
      title = element_text(size = base_size*0.9, face="italic", color="black"),
      plot.subtitle = element_text(size = base_size*0.9, face="italic", color="black"),
      plot.caption = element_text(size = base_size*0.9, face="italic", color="black"),
      
      axis.text = element_text(size = base_size*0.9, color="black"),
      axis.title.x = element_text(size=base_size*0.9, hjust=0, face="italic"),
      axis.title.y = element_text(size=base_size*0.9, hjust=0, face="italic"),
      
      legend.background = element_blank(), 
      legend.key.size = unit(base_size*0.3, "mm"), 
      legend.key = element_blank(), 
      legend.text = element_text(size=base_size*0.9, face="italic"), 
      legend.title = element_text(size=base_size*0.9, hjust=0, face="italic"), 
      legend.position = c(1, 1), legend.justification = c(1, 1) ) 
}

ggplot2::theme_set(my_theme())
line <- theme_get()$panel.grid.major#.x

# Based on the same ggpubr::theme_pubclean(), reverse the grid when flpping the axes
# theme_reverse_grid = theme(panel.grid.major.x = element_line(color="grey", linetype = "dotted"),
#                            panel.grid.major.y = element_blank())
theme_reverse_grid = theme(panel.grid.major.x = line,
                           panel.grid.major.y = element_blank())


# A clean theme for pie charts, based on the same ggpubr::theme_pubclean()

pie_theme <- function(base_family= font.face, base_size=10, base_theme = theme_grey) {
  pt <- my_theme(base_size = base_size, base_family = base_family, base_theme = base_theme)
  line <- pt$panel.grid.major#.x
  pt <- pt + theme(
      text=element_text(family=base_family, size = base_size, color = "black"),
      
      #plot.caption = element_text(size=base_size*0.9, face="italic"),
      plot.subtitle = element_text(size=base_size, face="italic"),
      #plot.title = element_text(size=base_size*0.9, face="italic"),
      
      plot.background = element_blank(), panel.background = element_blank(),
      
      axis.text.x=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title=element_blank(),
      
      #legend.background = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0, 1), #top left
      legend.direction = "horizontal", 
      legend.justification = c(0, 0.5),
      #legend.text = element_text(size=base_size*0.9, face="italic"), 
      legend.key.height = unit(base_size*0.3, "pt"),
      legend.key.width = unit(base_size*0.6, "pt"))
  
  
  #pt <- pt + theme(panel.grid.major.y = element_line(color = "grey", linetype = "dotted"))
  pt <- pt + theme(panel.grid.major.y = line)
  
  pt
}


#qplot(rnorm(100))+pie_theme(base_theme = theme_grey)
#qplot(rnorm(100))+pie_theme(base_theme = theme_pubclean)


