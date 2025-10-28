# read in the data
water = read.table('~/Dropbox/Teaching/Stat 536/3 - Nonlinear/AgricultureWater.txt',header=T)

# Smoothing Splines

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
lines(water$cwsi[order(water$cwsi)],model1$fitted.values[order(water$cwsi)],col='orange')

# add two standard errors
p = predict(model1,se.fit=T)
lines(water$cwsi[order(water$cwsi)],(p$fit+2*p$se.fit)[order(water$cwsi)],col='orange',lty=2)
lines(water$cwsi[order(water$cwsi)],(p$fit-2*p$se.fit)[order(water$cwsi)],col='orange',lty=2)

# There's also a smooth.spline() function if you want to choose df manually