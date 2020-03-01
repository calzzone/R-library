t <- table(round(rnorm(100, mean(rnorm(3, 2, 3)), 5)))
#plot(t)
s <- t[c("0", "1", "2", "3", "4")]
plot(s)

id = 1:10
df <- data.frame(id)

for (x in 1:6) {
  t <- round(rtruncnorm(10, mean(rnorm(3, 2, 3)), 5,0,4))
  #s <- t[c("0", "1", "2", "3", "4")]
  plot(table(t))
  df[,x+1] <- t
}
View(df)


rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  if (a > b) stop('Error: Truncation range is empty');
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd));
  qnorm(U, mean, sd); }

plot(rtruncnorm( 100, 2.5, 2, 0, 5))
