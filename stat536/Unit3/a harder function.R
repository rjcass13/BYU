# Play around with this code! Note that even for crazy functions, 

library(splines)

set.seed(2)
f = function(x){
  -6+x^3+16*abs(x)-5*sin(x*4)
}
curve(f,-2,2,n=1000)

n = 100
x = sort(runif(n,-2,2))
y = f(x)+rnorm(n,0,2)

plot(x,y)
curve(f,-2,2,add=T)




###
### Guided explanation of these methods for your gut intuition. 
###



# For plotting
xx = seq(-2,2,length.out=1001)

### 
### Polynomial Regression fit
### 

plot(x,y)
curve(f,-2,2,add=T)

for(i in 1:7){
  model = lm(y~poly(x,i))
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=2,lty=i)
}
for(i in 8:17){
  model = lm(y~poly(x,i))
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=3,lty=i)
}
# note that 8th and higher degree polynomials seem to fit pretty close, but have highly varying behavior in the tails

### 
### B splines/cubic
### 
plot(x,y)
curve(f,-2,2,add=T)

for(i in 3:7){
  model = lm(y~bs(x,df = i))
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=2,lty=i)
}
for(i in 8:17){
  model = lm(y~bs(x,df=i))
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=3,lty=i)
}
# just a little shout out to df=7
 
model7 = lm(y~bs(x,df = 7))
yyhat = predict(model7,newdata = data.frame(x=xx))
lines(xx,yyhat,col='blue',lwd=2)

model7 = lm(y~bs(x,degree=3, knots =   quantile(x,c(.2,.4,.6,.8))))
yyhat = predict(model7,newdata = data.frame(x=xx))
lines(xx,yyhat,col='orange',lwd=2)

# Custom knots: can we do better? i.e. fewer knots? It's hard. 

model = lm(y~bs(x,degree=3,knots = c(-.7,0,1.5)))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col='purple',lwd=4)

# Note the tail behavior of these is crazy too! 



### 
### Natural splines
### 
plot(x,y)
curve(f,-2,2,add=T)

for(i in 1:7){
  model = lm(y~ns(x,df = i))
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=2,lty=i)
}
for(i in 8:12){
  model = lm(y~ns(x,df=i))
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=3,lty=i)
}
# just a little shout out to df=5

model5 = lm(y~ns(x,df = 5))
yyhat = predict(model7,newdata = data.frame(x=xx))
lines(xx,yyhat,col='blue',lwd=2)

model5 = lm(y~ns(x, knots =   quantile(x,c(.2,.4,.6,.8))))
yyhat = predict(model7,newdata = data.frame(x=xx))
lines(xx,yyhat,col='orange',lwd=2)

# Here, df=5 has the same knots!

# Custom knots: can we do better? i.e. fewer knots? It's hard when the nonlinear features of the function are about equally spaced ;)

model = lm(y~ns(x,knots = c(-.7,0,1.5)))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col='purple',lwd=4)

###
### Loess
###

plot(x,y)
curve(f,-2,2,add=T)

# Kernel smoothing (no local regression)
for(i in 1:20){
  model = loess(y~x,degree = 0,span = i/20)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=2,lty=i)
}
# Local linear loess
for(i in 1:20){
  model = loess(y~x,degree = 1,span = i/20)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=3,lty=i)
}
# Local quadratic (default) loess
for(i in 1:20){
  model = loess(y~x,span = i/20)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=4,lty=i)
}
# Note: this figure effectively uses all possible spans. 
# Too large of spans create those flat lines, in red (degree=0) especially
# Too small creates the wiggly spikes, in blue (degree=2) especially. 
# So should we just use green/degree=1? But degree=2 is the default. 
# Note that many of the green curves are not good (but not terrible), whereas most of the blue curves are good, though some are crazy. 


# We'd never actually use many of the spans shown in the above plot, so let's plot a reduced set of those that are actually plausible (i.e. lie within the range of y|x)
plot(x,y)
curve(f,-2,2,add=T)
# 
# Kernel smoothing (no local regression)
for(i in 2:4){
  model = loess(y~x,degree = 0,span = i/20)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=2,lty=i)
}
# Local linear loess
for(i in 2:7){
  model = loess(y~x,degree = 1,span = i/20)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=3,lty=i)
}
# Local quadratic (default) loess
for(i in 2:12){
  model = loess(y~x,span = i/20)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=4,lty=i)
}
# Now, notice the much larger range of spans that are "plausible" for degree=2 in blue! 


###
### GAM, i.e. smoothing splines
###

plot(x,y)
curve(f,-2,2,add=T)
model = gam(y ~ s(x))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col=4)

model = gam(y ~ s(x,bs="cr"))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col=5)
# There's lots of settings you can change for a smoothing spline, but they're pretty good out of the box. 





###
### Compare tail behavior
###
# The perk of natural splines is supposed to be better tail behavior, right? Do we see that? 
# Red = polynomials
# Blue = b-splines
# Green = natural splines
plot(x,y)
curve(f,-2,2,add=T,lwd=5)


# for(i in 3:17){
#   model = lm(y~poly(x,i))
#   yyhat = predict(model,newdata = data.frame(x=xx))
#   lines(xx,yyhat,col=2)
# }
# for(i in 3:17){
#   model = lm(y~bs(x,df = i))
#   yyhat = predict(model,newdata = data.frame(x=xx))
#   lines(xx,yyhat,col=4)
# }
for(i in 3:17){
  model = lm(y~ns(x,df=i))
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=3)
}

# Note that all three methods have the flexibility to fit the middle of the data well if df is high enough. 
# However, we see that b-splines have better tail behavior than polynomials, and natural splines are better than both. 

# now add loess and gam:
# Kernel smoothing (no local regression)
# for(i in 2:15){
#   model = loess(y~x,degree = 0,span = i/20)
#   yyhat = predict(model,newdata = data.frame(x=xx))
#   lines(xx,yyhat,col=5)
# }
# Local linear loess
# for(i in 2:15){
#   model = loess(y~x,degree = 1,span = i/20)
#   yyhat = predict(model,newdata = data.frame(x=xx))
#   lines(xx,yyhat,col=6)
# }
# Local quadratic (default) loess
for(i in 2:15){
  model = loess(y~x,span = i/20)
  yyhat = predict(model,newdata = data.frame(x=xx))
  lines(xx,yyhat,col=7)
}
# GAM

model = gam(y ~ s(x))
yyhat = predict(model,newdata = data.frame(x=xx))
lines(xx,yyhat,col='firebrick',lwd=4)









