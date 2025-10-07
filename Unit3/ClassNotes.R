# read in the data
water <- read.table('AgricultureWater.txt',header=T)

###########################
# EDA
###########################

# Values within expected range
summary(water)

# SWC is non-normal
hist(water$swc)
# CWSI is pretty uniform
hist(water$cwsi)

mod <- lm(swc ~ ., data = water)
# Not a linear relationship
plot(mod)

# Try a polynomial. Need I() function to make it handle poly
poly <- lm(swc ~ cwsi + I(cwsi^2), data = water)
# Much better residuals
plot(poly)

# Plot the data vs. poly model line (uses prediction values to get Y's)
plot(water$cwsi, water$swc)
x.out = seq(0, 1, .01)
newdata = data.frame(cwsi = x.out)
y.out <- predict(poly, newdata = newdata)
lines(x.out, y.out, col = 'mediumorchid3')
# Given description (CWSI -> 1 means high stress and water needs to be added)
# This would say that as stress increases, water needs to be added, but also it has more water available

###########################
# Non-linear options: 
###########################
# Can do transformation on Y
log_mod <- lm(log(swc) ~ cwsi, data = water)
plot(log_mod)

# Can do a polynomial regression
poly_mod <- lm(swc ~ cwsi + I(cwsi^2) + I(cwsi^3) + I(cwsi^4), data = water)
plot(poly_mod)
# Can cause issues on edges, especially beyond range of data
# Need to choose order of polynomials (squared, cubed, etc.)
# Can use variable selection as usual
# Colinearity is an issue (higher order, more colinearity)
# Scale can be important (cube big numbers is bad)

# Basis functions: y = beta0 + beta1 * b1(x) + beta2 * b2(x) + ...
# Can do stepwise/piecemeal (different functions within different ranges of x)
# Stepwise won't be continunous at knots (boundaries of regions)
# Splines: Stepwise polynomial, but enforces continuity

###########################
# Splines
###########################
# Polynomial regression
library(splines)

## Splines (and basis function expansions)
# To use splines, you will need the `splines` library in R.  
# All basis function expansions are done within R formulas (so I hope you are comfortable with them by now).  
# For example, to fit a B-spline you would specify the formula `lm(y~bs(x, df=))` where `x` is the 
# variable you want a spline in.

# - `poly(var_name, degree=)` - creates a `degree` polynomial in `var_name`
# - `bs(var_name, df=, degree=, knots=)` - creates a `degree` B-spline in the variable `var_name` with `df` degrees of freedom.  Altneratively, you can use the `knots` argument to specify where the knots are put.  If you only give the `df` then it will put the knots equally spaced.
# - `ns(var_name, df=, degree=, knots=)` - create a `degree` natural spline in the variable `var_name` with `df` degrees of freedom or `knots`.
# - `I()` - this is for special use within formulas where you can put any function you want.  For example, if I put `y~I(x>10)` it will set up a step function at the point 10.  Likewise, `y~I(x^2)` will put a squared term for `x` (although `poly()` above is a better way to do polynomial regression).  So, anytime you alter a variable within a formula, make sure you use `I()`.
mod_spl <- lm(swc ~ bs(water$cwsi, knots = seq(0, 1, .05)), data = water)
# Having too many knots can 'overfit' the data
plot(water$cwsi, water$swc)
lines(water$cwsi[order(water$cwsi)], mod_spl$fitted.values[order(water$cwsi)])

mod_spl <- lm(swc ~ bs(water$cwsi, knots = c(0.5)), data = water)
plot(water$cwsi, water$swc)
lines(water$cwsi[order(water$cwsi)], mod_spl$fitted.values[order(water$cwsi)])

mod_spl <- lm(swc ~ bs(water$cwsi, knots = c(.5, .6)), data = water)
plot(water$cwsi, water$swc)
lines(water$cwsi[order(water$cwsi)], mod_spl$fitted.values[order(water$cwsi)])

mod_spl <- lm(swc ~ bs(water$cwsi, knots = c(.6, .7, .75, .8, .9)), data = water)
plot(water$cwsi, water$swc)
lines(water$cwsi[order(water$cwsi)], mod_spl$fitted.values[order(water$cwsi)])

# Cubic splines behave poorly outside range of data: can use natural splines (ns())
# Natural splines force linearity outside of last knots
# Number of knots: has a large impact on performance
# Degrees of freedom:
# ns(): num.knots = df - 1
# bs(): num.knots = df - degree (order)
# Location of knots: Usually evenly spaced over domain

# Testing
samples <- sample(1:78, 15)
train <- water[-samples, ]
test <- water[samples,]

mod_ns_3 <- lm(swc ~ ns(train$cwsi, df = 3), data = train)
plot(train$cwsi, train$swc)
lines(train$cwsi[order(train$cwsi)], mod_ns_3$fitted.values[order(train$cwsi)])
yhat <- predict(mod_ns_3, newx = test)
RMSE = 

mod_lin_3 <- lm(swc ~ bs(train$cwsi, degree = 1, df = 3), data = train)
plot(train$cwsi, train$swc)
lines(train$cwsi[order(train$cwsi)], mod_lin_3$fitted.values[order(train$cwsi)])


###########################
# Kernel Smoothing
###########################
# Kernel: a 'functional' distribution (ie. oh, this matches the exponential, etc.)
# Kernel smoothing: Take points within range and find average (point)
# Nearest Neighbor: Take average of nearby points (within a bandwidth)
### Similar to K-nearest neighbors (But KNN picks # points on each side, not width)
# Epanechnikov Kernel: Apply normal (no tails) distributed weights to nearest points
# There are a bunch of kernels. Usually, want to 0 out things far away 
# How to choose width: Cross-validate. Usually best to only report Out of Sample RMSE
# On boundaries (not as much data) variance of predictions increases, becomes biased at tails

# KNN Example
# Get training/test data
ts = sample(1:nrow(water),20)
train = water[-ts,]
test = water[ts,]
# Plot train vs. test
plot(train$cwsi,train$swc,ylim=c(22,30))
points(test$cwsi,test$swc,pch=19)

# KNN in caret
library(caret)
model = caret::knnreg(swc~cwsi,data=train,k = 10)
yhat = predict(model,newdata = test)
# Just plotting as is is out of order
lines(test$cwsi,yhat,type='o',col=2)
# Order the test x's:
plot(train$cwsi,train$swc,ylim=c(22,30))
points(test$cwsi,test$swc,pch=19)
lines(test$cwsi[order(test$cwsi)],yhat[order(test$cwsi)],type='o',col=2)

# But which k? ---note that we can also see the bias/variance tradeoff here! 
k = 1:nrow(train)
MSE = numeric(length(k)) # empty vector of the same length as klist. 

# recall MSE = bias^2 + var + sigma2
# loop through all suggested k, then plot
for(i in 1:length(k)){
  model = caret::knnreg(swc~cwsi,data=train,k = k[i])
  yhat = predict(model,newdata = test)
  MSE[i] = (mean((test$swc - yhat)^2))
}
plot(k,MSE,ylim=c(0,max(MSE)))

###########################
# Local Regression (LOESS)
###########################
# Uses a kernal function to weight, then fits a poly model within that range
# y = b0 + bx + bx^2

?loess
# loess(formula=, data=, degree=, span=) - 
# this uses a tricubic weighting function with smoothing span span to fit a local 
# regression of degree degree. If span<1 then only a fraction of the data is weighted. 
# IMPORTANT: this function can only handle up to 4 numeric predictors. For higher dimensions, 
# local regression usually doesn’t work well due to the curse of dimensionality.

# Degree: Which ceofficients are used: setting to 0 = average (kernel smoothing)



# Test some examples
# Get training/test data
ts = sample(1:nrow(water),20)
train = water[-ts,]
test = water[ts,]
# first, the default settings of quadratic (degree=2) and span=0.75
model = loess(swc~cwsi,data=train)
oo = order(train$cwsi)
plot(train$cwsi,train$swc,ylim=c(22,30))
points(test$cwsi,test$swc,pch=19)
lines(train$cwsi[oo],model$fitted[oo],col=2)

# Now increase the flexibility by using more local weights with span=0.15
model2 = loess(swc~cwsi,data=train,span=.15)
lines(train$cwsi[oo],model2$fitted[oo],col=3)

model3 = loess(swc~cwsi,data=train,span=.5, degree = 1)
lines(train$cwsi[oo],model3$fitted[oo],col=4)

# Model 1
yhat = predict(model,newdata = test)
(mean((test$swc - yhat)^2))
# Model 2
yhat2 = predict(model2,newdata = test)
(mean((test$swc - yhat2)^2))
# Model 3
yhat3 = predict(model3,newdata = test)
(mean((test$swc - yhat3)^2))

# What happens as span changes?
x <- c(1:10)
span_mse <- numeric(length(x))
spans <- numeric(length(x))
for (i in 1:10) {
  span = 1/i
  span_model <- loess(swc~cwsi,data=train,span=span)
  span_yhat <- predict(span_model, newdata = test)
  span_mse[i] <- (mean((test$swc - span_yhat)^2))
  spans[i] <- span
}
plot(spans, span_mse)

(test$swc - span_yhat)

###########################
# Smoothing Splines
###########################
# Want small changes in second derivative (penalize how fast the derivative changes)
# We don't need to choose knots, but choose lambda via cross-validation

# gam or mgcv libraries
# Note the mgcv library because it has an option for calculating standard errors while gam doesn’t.
library(mgcv)

# gam(y~s(x), data=) - while with other basis function expansions you can just use the lm() function, 
# smoothing splines are a different in that you have to use the gam function (becase, recall from class, 
# that smoothing splines are LASSO-ed, splines with a knot at every point). The s(x) specifies that you 
# want a smoothing spline in x. When you specify the model this way gam will do cross-validation for you to 
# choose the correct penalty parameter.
model1 = gam(swc ~ s(cwsi),data=water)
plot(water$cwsi,water$swc)
lines(water$cwsi[order(water$cwsi)],model1$fitted.values[order(water$cwsi)],col='green')
# add two standard errors
p = predict(model1,se.fit=T)
lines(water$cwsi[order(water$cwsi)],(p$fit+2*p$se.fit)[order(water$cwsi)],col='orange',lty=2)
lines(water$cwsi[order(water$cwsi)],(p$fit-2*p$se.fit)[order(water$cwsi)],col='orange',lty=2)
# There's also a smooth.spline() function if you want to choose df manually

###########################
# Generalized Additive Models (GAMs)
###########################
# Basically, fit a value (function) for each factor, then iterate through until all functions converge
# Selection in GAMs
# Up to me to determine whether or not a variable should or should not be included in the model
# Can involve looking at individual relationships to see which ones appear to be related, which should be linear, etc.



###########################
# Testing
###########################
water <- read.table('AgricultureWater.txt',header=T)

ts = sample(1:nrow(water),20)
train = water[-ts,]
test = water[ts,]
model1 = gam(swc ~ s(cwsi),data=train)
p = predict(model1,newdata = test, se.fit=T)
# Predictive capability
sqrt(mean(p$se.fit))
# Water needed to add to get to 29
29- predict(model1, newdata = data.frame(cwsi = .5))


plot(model1)
library(gratia)

