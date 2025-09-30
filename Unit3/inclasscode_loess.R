# read in the data
water = read.table('~/Dropbox/Teaching/Stat 536/3 - Nonlinear/AgricultureWater.txt',header=T)

ts = sample(1:nrow(water),20)
train = water[-ts,]
test = water[ts,]

plot(train$cwsi,train$swc,ylim=c(22,30))
points(test$cwsi,test$swc,pch=19)


# Local Regression

# Because local regression is typically a bit more stable than kernel smoothing, 
# only local regression is highlighted here
# Of course, you can try kernel smoothing if you want!

?loess
# loess(formula=, data=, degree=, span=) - 
# this uses a tricubic weighting function with smoothing span span to fit a local 
# regression of degree degree. If span<1 then only a fraction of the data is weighted. 
# IMPORTANT: this function can only handle up to 4 numeric predictors. For higher dimensions, 
# local regression usually doesn’t work well due to the curse of dimensionality.



# first, the default settings of quadratic (degree=2) and span=0.75
model = loess(swc~cwsi,data=train)
oo = order(train$cwsi)
lines(train$cwsi[oo],model$fitted[oo],col=2)

# Now increase the flexibility by using more local weights with span=0.15
model2 = loess(swc~cwsi,data=train,span=.15)
lines(train$cwsi[oo],model2$fitted[oo],col=3)
