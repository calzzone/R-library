# Download data online. This is a simulated dataset for this post.
#myData <- read.csv('http://static.lib.virginia.edu/statlab/materials/data/mediationData.csv')

model.0 <- lm(Y ~ X, myData)
summary(model.0)

model.M <- lm(M ~ X, myData)
summary(model.M)

model.Y <- lm(Y ~ X + M, myData)
summary(model.Y)


library(mediation)
results <- mediate(model.M, model.Y, treat='X', mediator='M', boot=TRUE, sims=500)
summary(results)
