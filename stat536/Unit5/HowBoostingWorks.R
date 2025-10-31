rm(list=ls())
library(rpart)
# Challenging single variable function
f = function(x){
  ifelse(x>.5,
         x^-.2,
         log(x))
}


# Generate data
nn = 100
xx = sort(runif(nn))
yy = f(xx) + rnorm(nn,sd=.01)
data = data.frame(y=yy,x=xx)

par(mfrow=c(1,1)) # single plot

plot(xx,yy,pch=19)
curve(f,add=T,n = 9999)


# This will ease plotting later:
oos.seq = seq(from=0.000001,to=1,length.out = 9000)
oos.df = data.frame(x = oos.seq)



###
### BOOSTING, using trees_functionfit's CART code
###
lambda = 1                                   # !!!!!!!! CHOOSE !!!!!!!!!!


par(mfrow=c(1,3)) # side-by-side plots
current.f.hat_test = numeric(length(oos.seq))
current.f.hat_train = numeric(length(yy))
current.residuals = yy
iteration.number = 0

###
### iterate 
###
iteration.number = iteration.number+1
updated.data = data.frame(y = current.residuals, x = xx)
tree <- rpart(y~x,data=updated.data,  control=list(cp=0))  # !!!!!!!! CHOOSE !!!!!!!!!!
f.hat.b_test = predict(tree, newdata=oos.df)
f.hat.b_train = predict(tree)
# update f.hat
current.f.hat_train = current.f.hat_train + lambda*f.hat.b_train
current.f.hat_test = current.f.hat_test + lambda*f.hat.b_test

# Plot fit of f.hat.b
plot(xx,current.residuals,pch=17,main="This iteration's fit")
lines(oos.seq,f.hat.b_test,col=2,lwd=3)

# plot current f.hat's fit on original data
plot(xx,yy,pch=19,main='Overall Fit')
curve(f,add=T)
lines(oos.seq,current.f.hat_test,col=4,lwd=3)

# update current.residuals
current.residuals = current.residuals - lambda*f.hat.b_train # you could just take original y's minus current f.hat

# plot current residuals to see what we'd fit next: 
plot(xx,current.residuals,pch=2,main='New Residuals')


print(iteration.number)

#preview: what if we let lambda change/adapt? 
