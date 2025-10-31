rm(list=ls())

ad = read.csv('~/Dropbox/Teaching/Stat 536/6 - Binary ML/Admissions.csv',stringsAsFactors = T)
head(ad)

plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)







## JDF hacks for plotting cool stuff
x.len = 101
y.len = 101
x.test.seq = seq(290,340,length.out = x.len)
y.test.seq = seq(min(ad$CGPA),4,length.out = y.len)
x.test = rep(x.test.seq,y.len)
y.test = rep(y.test.seq,each=x.len)
X.test = cbind(x.test,y.test)
points(X.test,col=4,cex=.2)
X.test.df = data.frame(CGPA = y.test, GRE.Score = x.test)


Y.train = ifelse(ad$Status=='Admitted',1,0)
X.train = model.matrix(Status~.-1,data=ad)


#################################
########### new tree methods!
#################################

### Random Forests/Bagging
library(ranger)
rf <- ranger(Status~CGPA+GRE.Score,data=ad
              ,num.trees = 200
              # , mtry = 4
              , importance = "impurity"
             , probability = T)  
p = predict(rf,data = X.test.df)
plot.these = which(p$predictions[,1]<.6 & p$predictions[,1]>.4)
plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
points(x.test[plot.these],y.test[plot.these],col=5,pch=20)
importance(rf)



### Boosting
library(gbm)
boost <- gbm(Y.train~CGPA+GRE.Score,data=ad
             # , distribution = "bernoulli"
             # , n.trees = 100
             )
# yhat.boost = predict(boost,newdata = ??)
summary(boost)
p = predict(boost,newdata = X.test.df,type='response')
plot.these = which(p<.6 & p>.4)
plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
points(x.test[plot.these],y.test[plot.these],col=5,pch=20)
 
### BART
library(BART) 
#multinomial bart
bart = lbart(x.train = ad[,c(5,1)], y.train = Y.train
              , x.test = X.test.df
              # , ntree = 30
              # , nskip = 1000
              # , ndpost=1000
              # , keepevery = 1
)
dim(bart$yhat.test)


# Aside: See Jensen's inequality:
hist(plogis(bart$yhat.test))
a = apply(plogis(bart$yhat.test),2,mean)
p = plogis(apply(bart$yhat.test,2,mean))
plot(a,p)
abline(0,1,col=2,lwd=4)


# Plot: 
plot.these = which(p<.55 & p>.45)
plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
points(x.test[plot.these],y.test[plot.these],col=5,pch=20)

################################
### PARTIAL DEPENDENCE PLOTS ###
################################
library(pdp)
library(ggplot2)

# For Random Forests
rf <- ranger(Status~.,data=ad
             ,num.trees = 200
             # , mtry = 4
             , importance = "impurity"
             , probability = T)  
p = partial(rf,"CGPA")
autoplot(p, contour = TRUE)
p = partial(rf,"GRE.Score")
autoplot(p, contour = TRUE)
p = partial(rf,"SOP")
autoplot(p, contour = TRUE)

# For Boosting:
boost <- gbm(Y.train~GRE.Score + University.Rating + SOP + LOR + CGPA + Research,data=ad
             # , distribution = "bernoulli"
             # , n.trees = 100
             )
summary(boost)
# yhat.boost = predict(boost,newdata = ??)
plot(boost,"GRE.Score")
plot(boost,"CGPA")
plot(boost,"SOP")


# BART, need a different package:
library(bartMachine)

##Build another BART regression model
bart_machine = bartMachine(X,y, num_trees = 200, num_burn_in = 500,
                           num_iterations_after_burn_in = 1000)
#partial dependence plot  
pd_plot(bart_machine, "CGPA")
pd_plot(bart_machine, "SOP")










