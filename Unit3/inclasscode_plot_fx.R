# Play around with this code! Note that even for crazy functions, gam will get pretty close 

library(splines)

set.seed(2)
f = function(x){
  -6+x^3+16*abs(x)-5*sin(x*4)
}
g = function(z){
  ifelse(z<0,-10-5*z,-3*z^2)
}

curve(f,-2,2,n=1000)
curve(g,-2,2,n=1000)

n = 1000
x1 = sort(runif(n,-2,2))
x2 = runif(n,-2,2)
y = f(x1)+g(x2)+rnorm(n,0,2)


 

library(plotly)
plot_ly(x = x1, y = x2, z = y,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 4))

library(mgcv)
mod = gam(y ~ s(x1)+s(x2))
plot(mod, pages = 1, shade = TRUE, seWithMean = TRUE, scale = 0)


# install.packages("gratia")
library(gratia)
# all smooths, auto layout
draw(mod)
 

### Manually: 
xx = seq(-2,2,.01)
x.out = data.frame(x1 = xx, x2 = 0)
plot(xx, predict(mod,newdata = x.out),type='l')
curve(f,-2,2,col=2,add=T)
# Question, why is it shifted up/down? 

# plot g
x.out = data.frame(x1 = 0, x2 = xx)
plot(xx, predict(mod,newdata = x.out),type='l', ylim=c(-16, 5))
curve(g(x),-2,2,col=2,add=T)

# GAM lines will be biased: the job of the intercept is to bring it to the same level 
# of the original function