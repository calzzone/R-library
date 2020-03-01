p1 <- iris %>% ggplot() + theme_grey() + aes(x=Species, y=Sepal.Length, fill=Species) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0, 8), breaks=seq(0, 8, 1))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit="cm"))


p2 <- iris %>% ggplot() + theme_grey() + aes(x=Species, y=Sepal.Width, fill=Species) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0, 8), breaks=seq(0, 8, 1))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit="cm"))

p3 <- iris %>% ggplot() + theme_grey() + aes(x=Species, y=Petal.Length, fill=Species) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0, 8), breaks=seq(0, 8, 1))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit="cm"))

p4 <- iris %>% ggplot() + theme_grey() + aes(x=Species, y=Petal.Width, fill=Species) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0, 8), breaks=seq(0, 8, 1))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit="cm"))

p_all <- plot_grid(p1, p2, p3, p4,
                   #align="v", axis="l", 
                   nrow=1, 
                   labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"),
                   label_size = 8, label_x = 0, hjust = 0
                   ) %>% 
  add_sub(label="Secies", x = 0.5, hjust = 0.5, y = 0.5) 

legend <- get_legend(p1 + theme(legend.position = "top"))
p_all <- plot_grid(legend, p_all, nrow=2, rel_heights = c(0.1, 1))

p_all
#legend <- get_legend(p1 + theme(legend.position = "bottom"))
#p_all <- plot_grid(ggmatrix_gtable(p_all), legend, nrows=2, ncol = 1, rel_heights = c(1, .2)) 

#ggdraw(p_all)
