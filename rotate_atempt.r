test <- data.frame(start1=c(1,1,1,1,2,2,2,3,3,4),
                   start2=c(1,2,3,4,2,3,4,3,4,4),
                   logFC=c(5,5,1,0,8,0,5,2,4,3))

gg1 <- ggplot(test, aes(start1, start2)) + theme_gray()+
  geom_tile(aes(fill = logFC), colour = "gray", size=0.05) +
  scale_fill_gradientn(colours=c("#0000FF","white","#FF0000"), na.value="#DAD7D3")

library(grid)
plot(gg1, vp = viewport(angle = 45, clip = "off"))


rotate <- function(df, degree) {
  dfr <- df
  degree <- pi * degree / 180
  l <- sqrt(df$start1^2 + df$start2^2)
  teta <- atan(df$start2 / df$start1)
  dfr$start1 <- round(l * cos(teta - degree))
  dfr$start2 <- round(l * sin(teta - degree))
  return(dfr)
}
test2 <- rotate(test, 45)
gg2 <- ggplot(test2, aes(start1, start2)) + theme_pubclean()+
  geom_tile(aes(fill = logFC), colour = "gray", size=0.05) +
  scale_fill_gradientn(colours=c("#0000FF","white","#FF0000"), na.value="#DAD7D3")
plot(gg2)


plotTriMatrix <- function(x) {
  ## clear lower triangle
  x[lower.tri(x)] <- NA
  
  ## calculate diag
  nr <- nrow(x)
  nc <- ncol(x)
  d <- sqrt(nr^2 + nc^2)
  d2 <- 0.5 * d
  
  ## empty plot area
  plot(NA, type="n", xlim=c(0, d), ylim=c(0, d), xlab="", ylab="", asp=1)
  
  ## plot matrix and rotate 45
  rasterImage(as.raster(x),
              xleft=d2, xright=d2+nc, ybottom=-d2, ytop=-d2+nr,
              interpolate=FALSE, angle=45)
}
set.seed(123)
m <- matrix(runif(100), 10, 10)

plotTriMatrix(m)



