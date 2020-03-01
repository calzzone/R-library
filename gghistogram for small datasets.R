set.seed(1)
data <- rnorm(50, 35, 15)
data2 <- data.frame(id=factor(1:50), myvals = round(data, 0))
hist(data2$myvals)

ggplot(data2, aes(x=myvals) ) +
  geom_histogram(aes(group=id), fill="grey75", color="white", size=0.25, breaks=seq(0, 60, 10))+
  geom_histogram(color="black", size=0.5, breaks=seq(0, 60, 10), , fill="transparent")+
  stat_bin(aes(
    label=scales::percent(..count../50) 
    #label=paste(..count.., scales::percent(..count../50), sep=":\n")
    ),
    geom="text", vjust=-1, breaks = seq(0, 60, 10)) +
  expand_limits(y=c(0, 20))+
  theme_grey()
