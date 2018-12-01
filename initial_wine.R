wine_file = 'winequality-red.csv'
wine.data = read.csv(wine_file)
#attach(wine.data)
initial_fit = lm(quality ~., data=wine.data)
summary(initial_fit)

#quality <- wine.data$quality

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

# Test for normality of response variable using Shapiro-Wilk normality test
shapiro.test(x = wine.data$quality)
# W = 0.85759, p-value < 2.2e-16

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

# Train models for all possible regressor combinations and compare

library(leaps)

#n <- dim(wine.data)[1]
k <- dim(wine.data)[2]
p <- k + 1
wine.vs <- regsubsets(quality ~ ., data=wine.data, nbest=5)
summary(wine.vs)


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



# Use a proportional-odds logistic regression model since the response is ordinal

library(MASS)
library(dplyr)

wine.data.polr <- wine.data
wine.data.polr$quality <- factor(wine.data.polr$quality, ordered = T)

# Train model using variables selected using stepwise selection procedure
mPOLR <- polr(quality ~ volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data = wine.data.polr)
summary(mPOLR)

wine.col.subset <- wine.data %>% select(volatile.acidity,chlorides,free.sulfur.dioxide,total.sulfur.dioxide,pH,sulphates,alcohol)

# Plot marginal probabilities of quality rating for each regressor
par(mfrow = c(1,1))
for (i in 1:length(coefficients(mPOLR))){
  xran <- range(wine.col.subset[[i]])
  xvals <- seq(xran[1], xran[2], (xran[2]-xran[1])/50)
  xbeta <- xvals*(coefficients(mPOLR)[i])
  
  p3 <- logistic_cdf( mPOLR$zeta[1] - xbeta )
  p4 <- logistic_cdf( mPOLR$zeta[2] - xbeta ) - logistic_cdf( mPOLR$zeta[1] - xbeta )
  p5 <- logistic_cdf( mPOLR$zeta[3] - xbeta ) - logistic_cdf( mPOLR$zeta[2] - xbeta )
  p6 <- logistic_cdf( mPOLR$zeta[4] - xbeta ) - logistic_cdf( mPOLR$zeta[3] - xbeta )
  p7 <- logistic_cdf( mPOLR$zeta[5] - xbeta ) - logistic_cdf( mPOLR$zeta[4] - xbeta )
  p8 <- 1 - logistic_cdf( mPOLR$zeta[5] - xbeta )
  
  varname <- names(mPOLR$coefficients)[i]
  plot(xvals, p3, type='l', ylab='Prob', ylim=c(0,1), xlab=varname, main = paste0("Marginal probability plot for regressor:",varname))
  lines(xvals, p4, col='red')
  lines(xvals, p5, col='blue')
  lines(xvals, p6, col='green')
  lines(xvals, p7, col='purple')
  lines(xvals, p8, col='brown')
  legend("topleft", lty=1, col=c("black", "red", "blue", "green", "purple", "brown"), 
         legend=c("quality = 3", "quality = 4", "quality = 5", "quality = 6", "quality = 7", "quality = 8"))
}

