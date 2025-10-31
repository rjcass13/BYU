rm(list=ls())

ad = read.csv('Admissions.csv',stringsAsFactors = T)
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


#################################
########### Logistic regression
#################################

model.logit = glm(Status~CGPA+GRE.Score,data=ad,family='binomial')
p = predict(model.logit,newdata = X.test.df,type='response')

# easier to see which points are close to 0.5 instead of back-calculating to find the exact function/curve
plot.these = which(p<.55 & p>.45)
plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
points(x.test[plot.these],y.test[plot.these],col=5,pch=20)






#################################
########### Logistic regression + splines (basically logistic )
#################################
library(mgcv)
model.logit.gam = gam(Status~s(CGPA)+s(GRE.Score),data=ad,family='binomial')
p = predict(model.logit,newdata = X.test.df,type='response')

# easier to see which points are close to 0.5 instead of back-calculating to find the exact function/curve
plot.these = which(p<.55 & p>.45)
plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
points(x.test[plot.these],y.test[plot.these],col=5,pch=20)




### !!! YES, we could look at an interaction or decorrelating the variables, but if we have a lot of variables, wouldn't it be nice for the model to investigate interactions for us?????







#################################
########### KNN
#################################


# X <- fastDummies::dummy_cols(ad[,1:6])  # faster than model.matrix, but doesn't like this dataset because there are no categorical variables
X.train = model.matrix(Status~GRE.Score+CGPA,data=ad)
X.combined = rbind(X.train[,-1],X.test)
# indices of train, test
train = 1:nrow(X.train)
test = (nrow(X.train)+1):nrow(X.combined)

# Center and Scale (standardize)
X.scaled <- scale(X.combined)


library(class)
model1 = knn(train = X.scaled[train,], test=X.scaled[test,], cl = ad$Status, k = 3)
plot(x.test,y.test,col=as.numeric(model1),pch=19)


# Decorrelate
XX <- X.scaled%*%solve(chol(cov(X.scaled)))

model2 = knn(train = XX[train,], test=XX[test,], cl = ad$Status, k = 3)
plot(x.test,y.test,col=as.numeric(model2),pch=19)


# larger k

model3 = knn(train = XX[train,], test=XX[test,], cl = ad$Status, k = 7)
plot(x.test,y.test,col=as.numeric(model3),pch=19)
# nonlinear form is appearing!






#################################
########### Discriminant Analysis
#################################

adm = ad[ad$Status == 'Admitted',c(1,5)]
rej = ad[ad$Status == 'Rejected',c(1,5)]

plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16,cex=.6)

# Admitted
# thanks https://www.statology.org/bivariate-normal-distribution-in-r/
mu    <- colMeans(adm)
sigma <- cov(adm)
f     <- function(x, y) mnormt::dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x.test.seq, y.test.seq, f)
#add contour plot
contour(x.test.seq, y.test.seq, z,add=T)
points(mean(adm$GRE.Score),mean(adm$CGPA),cex=2,pch=19)

# Rejected
mu    <- colMeans(rej)
sigma <- cov(rej)
f     <- function(x, y) mnormt::dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x.test.seq, y.test.seq, f)
#add contour plot
contour(x.test.seq, y.test.seq, z,add=T,col=2)
points(mean(rej$GRE.Score),mean(rej$CGPA),cex=2,pch=19,col=2)



### LDA
library(MASS)
model4 = lda(Status ~ GRE.Score + CGPA, data=ad)
p = predict(model4,newdata = X.test.df)

plot.these = which(p$posterior[,1]<.52 & p$posterior[,1]>.48)
# plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
points(x.test[plot.these],y.test[plot.these],col=5,pch=20)



### QDA
model5 = qda(Status ~ GRE.Score + CGPA, data=ad)
p = predict(model5,newdata = X.test.df)

plot.these = which(p$posterior[,1]<.52 & p$posterior[,1]>.48)
# plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
points(x.test[plot.these],y.test[plot.these],col=5,pch=20)

  







#################################
########### SVM
#################################

plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)



### SVM
library(e1071)
model.svm = svm(Status ~ GRE.Score + CGPA, data=ad,kernel = "radial", probability = TRUE) 

p = predict(model.svm, newdata = X.test.df,probability = T)
pp = attr(p,"probabilities")

plot.these = which(pp[,1]<.55 & pp[,1]>.45)
# plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
points(x.test[plot.these],y.test[plot.these],col=5,pch=20)

plot(x.test,y.test,col=as.numeric(p),pch=19)




#################################
########### Neural Nets
#################################

# Neural nets in R don't work the best... These may not converge. 
# The best option probably is the keras package (which just runs python haha)

# fit neural network
library(neuralnet)
nn=neuralnet(Status ~ GRE.Score +CGPA,data=ad, hidden=c(2),act.fct = "logistic", stepmax=999999) #,threshold = .001
p = predict(nn,newdata = X.test.df,type='class')
plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
plot.these = which(p[,1]<.55 & p[,1]>.45)
points(x.test[plot.these],y.test[plot.these],col=5,pch=1)

# nnet
library(nnet)
nn = nnet(Status ~ GRE.Score + CGPA ,data=ad, size=3)
p = predict(nn,newdata = X.test.df)
plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)
plot.these = which(p[,1]<.55 & p[,1]>.45)
points(x.test[plot.these],y.test[plot.these],col=5,pch=1)



