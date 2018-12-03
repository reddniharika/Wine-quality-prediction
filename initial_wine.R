wine_file = 'winequality-red.csv'
wine.data = read.csv(wine_file)

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
X.mat = cbind(dplyr::select(wine.data, volatile.acidity, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, pH, sulphates, alcohol))
cor(X.mat)
# seems to be some correlation between free sulfur dioxide and total sulfur dioxide with corr .668
#plot:
plot(free.sulfur.dioxide, total.sulfur.dioxide)
summary(lm(free.sulfur.dioxide ~ total.sulfur.dioxide, data=wine.data))
# F statistic says there is some relationship, but perhaps not enough to call it multicollinearity
#lets look at vif

library(car)
full.lm <- lm(quality ~ ., data = wine.data)
final.lm <- lm(quality ~ volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data = wine.data)
vif(full.lm)
vif(final.lm)

wine.data$sulphates <- wine.data$sulphates^(-2)
final.lm2 <- lm(quality ~ volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data = wine.data)

#Looking for quadratic relationship
plot(x = wine.data$volatile.acidity, y = wine.data$quality)
plot((wine.data$chlorides)^(-0.5), y = wine.data$quality)
plot(wine.data$alcohol, y = wine.data$quality)
plot(wine.data$sulphate, y = wine.data$quality)
plot(wine.data$total.sulfur.dioxide, y = wine.data$quality)
plot(wine.data$free.sulfur.dioxide, y = wine.data$quality)
plot(wine.data$free.sulfur.dioxide , y = wine.data$quality)

bt <- boxTidwell(quality ~ volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, 
                 data = wine.data,
                 max.iter = 50)

bt

# The boxTidwell test indicates that sulphates and free.sulfur.dioxide should be transformed
par(mfrow = c(1,2))
plot(wine.data$sulphates, y = wine.data$quality)
plot(wine.data$sulphates^(-2), y = wine.data$quality)

plot(wine.data$free.sulfur.dioxide, y = wine.data$quality)
plot(wine.data$free.sulfur.dioxide^(-0.5), y = wine.data$quality)

# Transform using results of Box-Tidwell procedure
wine.data.transfm <- wine.data %>% dplyr::select(-quality)
wine.data.transfm$sulphates <- wine.data$sulphates^(-2)
wine.data.transfm$free.sulfur.dioxide <- wine.data$free.sulfur.dioxide^(-0.5)

# Center data before creating interactions to avoid multicollinearity problems
wine.data.transfm <- scale(wine.data.transfm, center = T, scale = F)
centers <- attr(wine.data.transfm, which = "scaled:center")
wine.data.transfm <- data.frame(wine.data.transfm)
wine.data.transfm$quality <- wine.data$quality


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

# both_model = lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = wine.data)
# free_model = lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + pH + sulphates + alcohol, data = wine.data)
# total_model = lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol, data = wine.data)
# 
# 
# summary(free_model)
# summary(total_model)
# summary(initial_fit)
# PRESS <- function(linear.model) {
#   pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
#   sum(pr^2)
# }

# Use stepwise selection to find significant base regressors
wine.lm.stepwise.centered = stepwise.selection(alpha.in, alpha.out, resp.name, reg.names, "wine.data.transfm")
summary(wine.lm.stepwise.centered)

# look for significant interaction terms, keeping only vars found significant by stepwise selection
total_model_interact = lm(quality ~ (volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol)^2, data = wine.data.transfm)
summary(total_model_interact)

# test significance of interaction terms with full set of base vars
total_model_plus = lm(quality ~ volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol + chlorides*free.sulfur.dioxide + free.sulfur.dioxide*pH + chlorides*pH + chlorides*sulphates + chlorides*alcohol + volatile.acidity*total.sulfur.dioxide + total.sulfur.dioxide*sulphates + sulphates*alcohol, data = wine.data.transfm)
summary(total_model_plus)

# Create interaction terms explicitly for inclusion in later models
wine.data.transfm.interact <- wine.data.transfm
wine.data.transfm.interact$volatile.acidity..total.sulfur.dioxide <- wine.data.transfm.interact$volatile.acidity*wine.data.transfm.interact$total.sulfur.dioxide
wine.data.transfm.interact$chlorides..free.sulfur.dioxide <- wine.data.transfm.interact$chlorides*wine.data.transfm.interact$free.sulfur.dioxide
wine.data.transfm.interact$chlorides..pH <- wine.data.transfm.interact$chlorides*wine.data.transfm.interact$pH
wine.data.transfm.interact$chlorides..sulphates <- wine.data.transfm.interact$chlorides*wine.data.transfm.interact$sulphates
wine.data.transfm.interact$chlorides..alcohol <- wine.data.transfm.interact$chlorides*wine.data.transfm.interact$alcohol
wine.data.transfm.interact$total.sulfur.dioxide..sulphates <- wine.data.transfm.interact$sulphates*wine.data.transfm.interact$total.sulfur.dioxide
wine.data.transfm.interact$sulphates..alcohol <- wine.data.transfm.interact$sulphates*wine.data.transfm.interact$alcohol

# Do stepwise selection again to determine which base + interaction regressors to keep
reg.names <- setdiff(names(wine.data.transfm.interact), "quality")
data.name <- "wine.data.transfm.interact"

wine.lm.stepwise.interact = stepwise.selection(alpha.in, alpha.out, resp.name, reg.names, data.name)
summary(wine.lm.stepwise.interact)
all.useful.vars <- names(coefficients(wine.lm.stepwise.interact))[-1]


# Use a proportional-odds logistic regression model since the response is ordinal

library(MASS)
library(dplyr)


# wine.col.subset <- wine.data %>% select(volatile.acidity,chlorides,free.sulfur.dioxide,total.sulfur.dioxide,pH,sulphates,alcohol)
# xmat.polr <- scale(wine.col.subset, center = T, scale = F)
# wine.data.polr <- data.frame(xmat.polr, quality = factor(wine.data$quality, ordered = T))
# 
# # Train model using variables selected using stepwise selection procedure
# mPOLR <- polr(quality ~ volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol, data = wine.data.polr)
# summary(mPOLR)
# unname(round(mPOLR$coefficients, 3))
# unname(round(mPOLR$zeta, 3))
# confint(mPOLR)
# 
# # Plot marginal probabilities of quality rating for each regressor
# par(mfrow = c(3,3))
# for (i in 1:length(coefficients(mPOLR))){
#   varname <- names(mPOLR$coefficients)[i]
#   xran <- range(wine.data.polr[[i]])
#   xvals <- seq(xran[1], xran[2], (xran[2]-xran[1])/50)
#   xvals.orig <- attr(x = xmat.polr, which = "scaled:center")[varname]+xvals
#   xbeta <- xvals*(coefficients(mPOLR)[i])
#   
#   p3 <- logistic_cdf( mPOLR$zeta[1] - xbeta )
#   p4 <- logistic_cdf( mPOLR$zeta[2] - xbeta ) - logistic_cdf( mPOLR$zeta[1] - xbeta )
#   p5 <- logistic_cdf( mPOLR$zeta[3] - xbeta ) - logistic_cdf( mPOLR$zeta[2] - xbeta )
#   p6 <- logistic_cdf( mPOLR$zeta[4] - xbeta ) - logistic_cdf( mPOLR$zeta[3] - xbeta )
#   p7 <- logistic_cdf( mPOLR$zeta[5] - xbeta ) - logistic_cdf( mPOLR$zeta[4] - xbeta )
#   p8 <- 1 - logistic_cdf( mPOLR$zeta[5] - xbeta )
#   
#   plot(xvals.orig, p3, type='l', ylab='Prob', ylim=c(0,1), xlab=varname, main = paste0("Marginal probability plot for regressor:",varname))
#   lines(xvals.orig, p4, col='blue')
#   lines(xvals.orig, p5, col='green')
#   lines(xvals.orig, p6, col='brown')
#   lines(xvals.orig, p7, col='purple')
#   lines(xvals.orig, p8, col='red')
# #  legend("topleft", lty=1, col=c("black", "blue", "green", "brown", "purple", "red"), 
# #         legend=c("quality = 3", "quality = 4", "quality = 5", "quality = 6", "quality = 7", "quality = 8"))
# }


# 2nd proportional-odds logistic regression model - including interaction terms

wine.col.subset.all <- wine.data.transfm.interact %>% dplyr::select(all.useful.vars)
wine.data.polr.all <- data.frame(wine.col.subset.all, quality = factor(wine.data$quality, ordered = T))

# Train model using variables selected using stepwise selection procedure
vars.to.use <- all.useful.vars  #setdiff(all.useful.vars, c("chlorides..alcohol", "total.sulfur.dioxide..sulphates"))#, "volatile.acidity..total.sulfur.dioxide"))
mPOLR2 <- polr(as.formula(paste0("quality ~ ", paste(sort(vars.to.use), collapse = " + "))), data = wine.data.polr.all)
summary(mPOLR2)
unname(round(mPOLR2$coefficients, 3))
unname(round(mPOLR2$zeta, 3))
confint(mPOLR2)

# fill out centering modifiers for interaction terms kept in model
centers <- c(centers, centers["total.sulfur.dioxide"]*centers["sulphates"], centers["total.sulfur.dioxide"]*centers["volatile.acidity"], centers["free.sulfur.dioxide"]*centers["chlorides"])
names(centers) <- c(names(centers)[1:(length(centers)-3)], "total.sulfur.dioxide..sulphates", "volatile.acidity..total.sulfur.dioxide", "chlorides..free.sulfur.dioxide")

# Plot marginal probabilities of quality rating for each regressor
par(mfrow = c(3,4))
for (i in 1:length(coefficients(mPOLR2))){
  varname <- names(mPOLR2$coefficients)[i]
  xran <- range(wine.data.polr.all[[varname]])
  xvals <- seq(xran[1], xran[2], (xran[2]-xran[1])/50)
  xvals.orig <- centers[varname]+xvals
  xbeta <- xvals*(coefficients(mPOLR2)[i])
  
  p3 <- logistic_cdf( mPOLR2$zeta[1] - xbeta )
  p4 <- logistic_cdf( mPOLR2$zeta[2] - xbeta ) - logistic_cdf( mPOLR2$zeta[1] - xbeta )
  p5 <- logistic_cdf( mPOLR2$zeta[3] - xbeta ) - logistic_cdf( mPOLR2$zeta[2] - xbeta )
  p6 <- logistic_cdf( mPOLR2$zeta[4] - xbeta ) - logistic_cdf( mPOLR2$zeta[3] - xbeta )
  p7 <- logistic_cdf( mPOLR2$zeta[5] - xbeta ) - logistic_cdf( mPOLR2$zeta[4] - xbeta )
  p8 <- 1 - logistic_cdf( mPOLR2$zeta[5] - xbeta )
  
  plot(xvals.orig, p3, type='l', ylab='Marginal Probability ', ylim=c(0,1), xlab=varname, main = paste0("Regressor:",varname))
  lines(xvals.orig, p4, col='blue')
  lines(xvals.orig, p5, col='green')
  lines(xvals.orig, p6, col='brown')
  lines(xvals.orig, p7, col='purple')
  lines(xvals.orig, p8, col='red')
#  legend("topleft", lty=1, col=c("black", "blue", "green", "brown", "purple", "red"), 
 #        legend=c("quality = 3", "quality = 4", "quality = 5", "quality = 6", "quality = 7", "quality = 8"))
}


# Testing effect of interactions on multicollinearity
tempdata <- data.frame(wine.col.subset.all, quality = wine.data$quality)
mtemp <- lm(quality ~ ., data = tempdata)
vif(mtemp)
summary(mtemp)



#Model adequacy checking
library(sure)
library(PResiduals)

# wine$quality<-as.factor(wine$quality)
# fit.logit<-polr(quality ~(volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol)^2,data=wine, method ="logistic")
# summary(fit.logit)

autoplot(mPOLR2, nsim = 5, what = "fitted", alpha = 0.5)
autoplot(mPOLR2, nsim = 5, what = "qq")

autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$volatile.acidity, xlab = "volatile.acidity")
autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$chlorides, xlab = "chlorides")
autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$free.sulfur.dioxide, xlab = "free.sulfur.dioxide")
autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$total.sulfur.dioxide, xlab = "total.sulfur.dioxide")
#autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$sulphates, xlab = "sulphates")
autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$alcohol, xlab = "alcohol")
autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$pH, xlab = "pH")
autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$ volatile.acidity..total.sulfur.dioxide, xlab = " volatile.acidity..total.sulfur.dioxide")
autoplot(mPOLR2, nsim = 5, what = "covariate",x = wine.data.polr.all$ chlorides..free.sulfur.dioxide, xlab = " chlorides..free.sulfur.dioxide")



# boxTidwell(quality ~total.sulfur.dioxide ,data=wine)
# # Total sulphur dioxide should be raise to the power 0.67
# 
# boxTidwell(quality ~sulphates , data=wine)
# # Sulphates should be raise to the power -2.17
# 
# boxTidwell(quality ~free.sulfur.dioxide , data=wine)
# # Free sulphur dioxide should be raise to the power 0.57
# 
# boxTidwell(quality ~chlorides , data=wine)
# # Chlorides should be raise to the power -0.66
# 
# 
# wine$sulphates1<-wine$sulphates^-2.17
# wine$total.sulfur.dioxide1<-wine$total.sulfur.dioxide^0.67
# 
# wine$free.sulfur.dioxide1<-wine$free.sulfur.dioxide^0.57
# wine$chlorides1<-wine$chlorides^-0.66
# 
# fit.logit.trans<-polr(quality ~chlorides1+free.sulfur.dioxide1+sulphates1+total.sulfur.dioxide1+volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol,data=wine, method ="logistic")
# autoplot(fit.logit.trans, nsim = 5, what = "covariate",x = wine$volatile.acidity, xlab = "volatile.acidity")
# autoplot(fit.logit.trans, nsim = 5, what = "covariate",x = wine$chlorides1, xlab = "chlorides")
# autoplot(fit.logit.trans, nsim = 5, what = "covariate",x = wine$free.sulfur.dioxide1, xlab = "free.sulfur.dioxide")
# autoplot(fit.logit.trans, nsim = 5, what = "covariate",x = wine$total.sulfur.dioxide1, xlab = "total.sulfur.dioxide")
# autoplot(fit.logit.trans, nsim = 5, what = "covariate",x = wine$sulphates1, xlab = "sulphates")
# autoplot(fit.logit.trans, nsim = 5, what = "covariate",x = wine$alcohol, xlab = "alcohol")
# autoplot(fit.logit.trans, nsim = 5, what = "covariate",x = wine$pH, xlab = "pH")
# 
# summary(fit.logit.trans)


"
DEviance tests can only be done to compare models before and after transformation

"

# The deviance statistics of these two models are        |

fit.logit$deviance
fit.logit.trans$deviance

# The difference-in-deviance statistic is                |

diff.dev <- fit.logit$deviance - fit.logit.trans$deviance
diff.dev

# Since the transformed model includes just 4 more parameters  |
# than the original model, the associated degrees of      |
# freedom is                                             |

df.diff.dev <- 4

# The chi-square critical value is                       |


# The estimate and standard error of the parameter under |
# test are                                               |

b2.chlorides1 <- summary(fit.logit.trans)$coefficients[1,1]
b2.chlorides1

se.b2.hat <- summary(fit.logit.trans)$coefficients[1,2]
se.b2.hat

#Sulphates transformed
b2.sulphates1 <- summary(fit.logit.trans)$coefficients[3,1]
b2.sulphates1

se.b2.hat <- summary(fit.logit.trans)$coefficients[3,2]
se.b2.hat

# The corresponding z statistic is formed as             |

z0 <- b2.sulphates1 / se.b2.hat
z0

# The standard-normal critical value is                  |
alpha <- 0.05
z.crit <- qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE)
z.crit


# The P-value is                                         |

p.val <- 2*pnorm(abs(z0), mean=0, sd=1, lower.tail=FALSE)
p.val


# #install.packages("survey")
# 
# # library(survey)
# # 
# # regressors<-c(chlorides1+free.sulfur.dioxide1+sulphates1+total.sulfur.dioxide1+volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+pH+sulphates+alcohol)
# # regTermTest(fit.logit.trans, "chlorides1")
# # regTermTest(fit.logit.trans, "free.sulfur.dioxide1")
# # regTermTest(fit.logit.trans, "sulphates1")
# # regTermTest(fit.logit.trans, "total.sulfur.dioxide1")
# # regTermTest(fit.logit.trans, "volatile.acidity")
# # regTermTest(fit.logit.trans, "chlorides")
# # regTermTest(fit.logit.trans, "free.sulfur.dioxide")
# # regTermTest(fit.logit.trans, "total.sulfur.dioxide")
# # regTermTest(fit.logit.trans, "pH")
# # regTermTest(fit.logit.trans, "sulphates")
# # regTermTest(fit.logit.trans, "alcohol")
# 
# 
install.packages("pscl")
library(pscl)
pR2(mPOLR2)


install.packages("MKmisc")
library(MKmisc)
HLgof.test(fit = fitted(mPOLR2), obs = wine.data.polr.all$quality)


#Multinom model
# fit.multinom <- multinom(quality ~ ., data=wine.data)
# summary(fit.multinom)
# 
# logLik(fit.multinom)

deviance(mPOLR2)
deviance(update(mPOLR2,.~.-chlorides)) - deviance(mPOLR2)
#0
#Shows that presence of chlorides has not had any effect on deviance
deviance(update(mPOLR2,.~.-chlorides..free.sulfur.dioxide)) - deviance(mPOLR2)
#9.803717

deviance(update(mPOLR2,.~.-free.sulfur.dioxide)) - deviance(mPOLR2)
#18.33998

deviance(update(mPOLR2,.~.-pH)) - deviance(mPOLR2)
#14.46254

deviance(update(mPOLR2,.~.-alcohol)) - deviance(mPOLR2)
#302.9163
#Alcohol has the most influence on quality

deviance(update(mPOLR2,.~.-volatile.acidity..total.sulfur.dioxide)) - deviance(mPOLR2)
#20.21373

deviance(update(mPOLR2,.~.-volatile.acidity)) - deviance(mPOLR2)
#134.8794

deviance(update(mPOLR2,.~.-total.sulfur.dioxide)) - deviance(mPOLR2)
#39.80138






####################################
### Logistic regression


wine.data$good <- wine.data$quality >= 6
head(wine.data)
summary(wine.data)



source('functions.r')
alpha.in <- 0.05
alpha.out <- 0.05
resp.name <- "good"
data.name = 'wine.data'
reg.names = c("fixed.acidity"  ,      "volatile.acidity"  ,   "citric.acid"    ,      "residual.sugar" ,     
              "chlorides"    ,        "free.sulfur.dioxide" , "total.sulfur.dioxide", "density"  ,           
              "pH"   ,                "sulphates"    ,        "alcohol" )

wine.lm.forward = forward.selection(alpha.in, resp.name, reg.names, data.name)
summary(wine.lm.forward)

wine.lm.backward = backward.elimination(alpha.out, resp.name, reg.names, data.name)
summary(wine.lm.backward)

wine.lm.stepwise = stepwise.selection(alpha.in, alpha.out, resp.name, reg.names, data.name)
summary(wine.lm.stepwise)

log1 = glm(good~volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=wine.data, family=binomial(link="logit"))
summary(log1)

qqnorm(rstudent(log1), datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals")
qqline(rstudent(log1))

nullmod <- glm(good~1, data=wine.data,family="binomial")
1-logLik(log1)/logLik(nullmod)

install.packages("pscl")
library(pscl)
pR2(log1)
###############################################