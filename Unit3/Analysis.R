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


