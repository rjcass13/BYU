library(splines)
library(mgcv)
water = read.table('~/Dropbox/Teaching/Stat 536/3 - Nonlinear/AgricultureWater.txt',header=T)
water = water[order(water$cwsi),]
plot(swc~cwsi,data = water,pch=16)

y = water$swc
x = water$cwsi

###
### Guided explanation of these methods for your gut intuition. 
###



# For plotting
xx = seq(0,1,length.out=1001)

### 
### Polynomial Regression fit
### 

model = lm(y~poly(x,2))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col=2,lwd=2)

### 
### B splines/cubic
### 

model = lm(y~bs(x,df = 8,degree = 3))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col=3,lwd=2)

### 
### Natural splines
### 

model = lm(y~ns(x,df = 2))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col=4,lwd=2)
 

###
### Loess
###
 
# Kernel smoothing (no local regression)
 
  model = loess(y~x,degree = 0,span = .15)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=5,lwd=2)
 
# Local linear loess
 
  model = loess(y~x,degree = 1,span = .225)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=6,lwd=2)
 
# Local quadratic (default) loess
 
  model = loess(y~x,span = .75,degree=1)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=4,lwd=2)
 
# KNN
  
  model = caret::knnreg(swc~cwsi,data=water,k = 5)
  yhat = predict(model,newdata = data.frame(cwsi=xx))
  lines(xx,yhat,col=8,lwd=2)
  
###
### GAM, i.e. smoothing splines
###
 
model = gam(y ~ s(x))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col=1,lwd=4)
# There's lots of settings you can change for a smoothing spline, but they're pretty good out of the box. 
 
###
### monotonic? We could try the (postive/increasing only) isotonic regression step function: 
###
plot(x,y)
iso = isoreg(x,-y)
lines(x,-iso$yf,col='firebrick',lwd=2)
 







