# read in the data
water = read.table('~/Dropbox/Teaching/Stat 536/3 - Nonlinear/AgricultureWater.txt',header=T)

ts = sample(1:nrow(water),20)

train = water[-ts,]
test = water[ts,]

plot(train$cwsi,train$swc,ylim=c(22,30))
points(test$cwsi,test$swc,pch=19)



library(caret)
model = caret::knnreg(swc~cwsi,data=train,k = 10)
yhat = predict(model,newdata = test)

lines(test$cwsi,yhat,type='o',col=2)

# now order the test x's:
plot(train$cwsi,train$swc,ylim=c(22,30))
points(test$cwsi,test$swc,pch=19)
lines(test$cwsi[order(test$cwsi)],yhat[order(test$cwsi)],type='o',col=2)


# But which k? ---note that we can also see the bias/variance tradeoff here! 
k = 1:nrow(train)

MSE = numeric(length(k)) # empty vector of the same length as klist. 
# recall MSE = bias^2 + var + sigma2

# loop through all suggested k, then plot
for(i in 1:length(k)){
  model = caret::knnreg(swc~cwsi,data=train,k = k[i])
  yhat = predict(model,newdata = test)
  MSE[i] = (mean((test$swc - yhat)^2))
}

plot(k,MSE,ylim=c(0,max(MSE)))
# 