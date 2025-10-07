water = read.table('AgricultureWater.txt',header=T)
water = water[order(water$cwsi),]

# Setup values we want to explore over
poly.max.df = 7
poly.test.errors = matrix(NA, ncol=poly.max.df, nrow=nrow(water))

bs.max.df = 10
bs.test.errors = matrix(NA, ncol=bs.max.df, nrow=nrow(water))

ns.max.df = 10
ns.test.errors = matrix(NA, ncol=ns.max.df, nrow=nrow(water))

loess.max.span = 10
loess.test.errors = matrix(NA, ncol=loess.max.span, nrow=nrow(water))

gam.test.errors = numeric(nrow(water))

# LOOCV: Leave One Out Cross Validation
# For LOOCV, we loop through each observation i as the test set. 
for(i in 1:nrow(water)){
  train = water[-i,]
  test = water[i,]

  ### Polynomial

  for(j in 1:poly.max.df){
    model = lm(swc~poly(cwsi,degree = j),data=train)
    yhat = predict(model,newdata = test)
    poly.test.errors[i,j] = test$swc - yhat
  }
  
  
  ### B splines

  for(j in 1:bs.max.df){
    model = lm(swc~bs(cwsi,df=j),data=train)
    yhat = predict(model,newdata = test)
    bs.test.errors[i,j] = test$swc - yhat
  }
  
  ### Natural splines

  for(j in 1:ns.max.df){
    model = lm(swc~ns(cwsi,df=j),data=train)
    yhat = predict(model,newdata = test)
    ns.test.errors[i,j] = test$swc - yhat
  }
  
  
  
  ### LOESS
  # fit loess with chosen span
  for(j in 1:loess.max.span){
    model = loess(swc~cwsi,data=train,span = i/10*.75)
    yhat = predict(model,newdata = test)
    loess.test.errors[i,j] = test$swc - yhat
  }
   

  ### GAM
  # gam will cv itself, but we want to compare OOS MSE
  model = gam(swc~cwsi,data=train)
  yhat = predict(model,newdata = test)
  gam.test.errors[i] = test$swc - yhat
}


#####
# We have the errors, now to get the OOS MSE. I'm going with RMSE, as it's more interpretable. 
#####

sqrt(colMeans(poly.test.errors^2))
sqrt(colMeans(bs.test.errors^2))
sqrt(colMeans(ns.test.errors^2))
sqrt(colMeans(loess.test.errors^2,na.rm=T))
sqrt(mean(gam.test.errors^2))
