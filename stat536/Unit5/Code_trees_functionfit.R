# Challenging single variable function
f = function(x){
ifelse(x>.5,
       x^-.2,
       log(x))
}

curve(f,n = 9999)

# Generate data
nn = 100
xx = sort(runif(nn))
yy = f(xx) + rnorm(nn,sd=.1)
data = data.frame(y=yy,x=xx)

plot(xx,yy,pch=19)
curve(f,add=T)


# This will ease plotting later:
oos.seq = seq(from=0.000001,to=1,length.out = 9000)
oos.df = data.frame(x = oos.seq)



### Regression Tree
library(rpart)
library(rpart.plot)
tree <- rpart(y~x,data=data,  control=list(cp=0))
rpart.plot(tree)
plotcp(tree)
min.cp <- tree$cptable[which.min(tree$cptable[,'xerror']),'CP']
tree.pruned <- tree # prune(tree, cp=min.cp)
rpart.plot(tree.pruned)
yhat.tree = predict(tree.pruned, newdata=oos.df)


# plot predictions
plot(xx,yy,pch=19)
curve(f,add=T)
lines(oos.seq,yhat.tree,col=2,lwd=3)


### Bagging... and Random Forests
# Note: bagging is just random forests with m=p. In this case p=1. 
library(ranger)
forest <- ranger(y~x,data=data
                 # ,num.trees = 2
                 # ,min.node.size = 20
                 ) 
forest$prediction.error # out-of-bag MSE

# plot the predictions
yhat.forest = predict(forest,data=oos.df)
plot(xx,yy,pch=19)
curve(f,add=T)
lines(oos.seq,yhat.forest$predictions,col=2,lwd=3)
# go back and adjust number of trees, etc.



### Boosting
library(gbm)
boost <- gbm(formula = y~x, data = data, distribution = "gaussian"
             # ,n.trees = 2000
             # ,shrinkage = .01
             # ,interaction.depth = 3
             )
yhat.boost = predict(boost,newdata = oos.df) 

# plot the predictions
plot(xx,yy,pch=19)
curve(f,add=T)
lines(oos.seq,yhat.boost,col=2,lwd=3)
# What happened??

# go back and adjust several things :)



### BART
library(BART) #we'll use gbart or wbart functions. (I'm pretty sure gbart just uses wbart when you put in a continuous outcome)
bart = gbart(x.train = data$x, y.train = data$y, x.test = oos.seq)
# bart$yhat.test has many values... because it is composed of draws from a distribution!

# plot(xx,yy,pch=19)
# curve(f,add=T)
# lines(oos.seq,bart$yhat.test.mean,col=2,lwd=3)
# lines(oos.seq,apply(bart$yhat.test,2,quantile,.975),col=2,lwd=2,lty=2)
# lines(oos.seq,apply(bart$yhat.test,2,quantile,.025),col=2,lwd=2,lty=2)
## or 
plot(xx,yy,pch=19)
curve(f,add=T)
lines(oos.seq,bart$yhat.test.mean,col=2,lwd=3)
polygon(
  x = c(oos.seq,rev(oos.seq)),
  y = c(apply(bart$yhat.test,2,quantile,.975), rev(apply(bart$yhat.test,2,quantile,.025))),
  col = rgb(1,0,0,alpha = .3),
  border = NA
)



### now go back and increase the error variance and see how this changes. 
