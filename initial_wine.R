wine_file = 'winequality-red.csv'
wine.data = read.csv(wine_file)
#attach(wine.data)
initial_fit = lm(quality ~., data=wine.data)
summary(initial_fit)

#see range of quality values
range(quality)
table(quality)
# histogram of quality
hist(quality)
# see number of observations and dimensions
dim(wine.data)
n = dim(wine.data)[1]
# calculate # of observations that are 5, 6 or 7
(681 + 638+199)/n

par(mfrow = c(3,4))
sapply(wine.data,hist)


# variable selection
source('functions.r')
alpha.in <- 0.05
alpha.out <- 0.05
resp.name <- "quality"
data.name = 'wine.data'
reg.names = names(wine.data[1:(length(names(wine.data))-1)])

wine.lm.forward = forward.selection(alpha.in, resp.name, reg.names, data.name)
summary(wine.lm.forward)

wine.lm.backward = backward.elimination(alpha.out, resp.name, reg.names, data.name)
summary(wine.lm.backward)

wine.lm.stepwise = stepwise.selection(alpha.in, alpha.out, resp.name, reg.names, data.name)
summary(wine.lm.stepwise)

#checking for multicollinearity
#check pairwise correlation
library(dplyr)
X.mat = cbind(select(wine.data, volatile.acidity, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, pH, sulphates, alcohol))
cor(X.mat)
# seems to be some correlation between free sulfur dioxide and total sulfur dioxide with corr .668
#plot:
plot(free.sulfur.dioxide, total.sulfur.dioxide)
summary(lm(free.sulfur.dioxide ~ total.sulfur.dioxide, data=wine.data))
# F statistic says there is some relationship, but perhaps not enough to call it multicollinearity
#lets look at vif

X.mat.scl = scale(X.mat)
XpX.mat.scl = t(X.mat.scl) %*% X.mat.scl
XpX.inv.scl = solve(XpX.mat.scl)
vif = diag(XpX.inv.scl)
vif

#Looking for quadratic relationship
plot(volatile.acidity, quality)
plot(chlorides, quality, data=wine.data)
plot(alcohol, quality, data=wine.data)
plot(sulphates, quality, data=wine.data)
plot(total.sulfur.dioxide, quality, data=wine.data)
plot(free.sulfur.dioxide, quality, data=wine.data)
plot(pH, quality, data=wine.data)



#plot(quality, resid(wine.lm.step))



both_model = lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol)
free_model = lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + pH + sulphates + alcohol)
total_model = lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol)

summary(free_model)
summary(total_model)
summary(initial_fit)
PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  sum(pr^2)
}



# plot(fixed.acidity, alcohol)
# plot(volatile.acidity, alcohol)
# plot(citric.acid, alcohol)
# plot(residual.sugar, alcohol)
# plot(chlorides, alcohol)
# plot(free.sulfur.dioxide, alcohol)
# plot(density, alcohol)
# plot(pH, alcohol)
# plot(sulphates, alcohol)
# plot(alcohol, quality)