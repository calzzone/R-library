setwd("~/Desktop/")
library(ggplot2)
library(ggExtra)
library(cowplot)
library(ggpubr)
library(dplyr)
library(readxl)
library(Hmisc) #rcor
library(stringr)

Untitled_1 <- read_csv("Untitled 1.csv") %>%
  mutate(Profit = as.numeric(Profit)) %>%
  filter(Profit != 0) %>%  filter(Profit < 2000) 

ggplot(Untitled_1) + aes(x=Profit,color=Symbol) +
  facet_wrap(~Volume) +
  theme_grey() +
  stat_bin(color="white", alpha=0.5, breaks = seq(-100,100,10)) +
  geom_rug(sides="b")
