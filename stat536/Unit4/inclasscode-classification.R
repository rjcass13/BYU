n = 1000
x = runif(n,-2,2)
p = pnorm(x)
y = rbinom(n,1,p)
plot(x,jitter(y))
lines(x[order(x)],p[order(x)],col=2,lwd=4)

model = glm(y~x,family=binomial)


Threshold = seq(0,1,by=0.001)
false.negative.rate <- false.positive.rate <- true.positive.rate <- error.rate <- (0*Threshold-99)

for(i in 1:length(Threshold)){
  y.hat = ifelse(model$fitted.values < Threshold[i],0,1)
  error.rate[i] = mean(y!=y.hat)
  TP = sum(y==1 & y.hat==1)
  TN = sum(y==0 & y.hat==0)
  FP = sum(y==0 & y.hat==1)
  FN = sum(y==1 & y.hat==0)
  false.negative.rate[i] = FN/(FN+TP) # 1-sensitivity
  false.positive.rate[i] = FP/(FP+TN) # 1-specificity
}

## Errors plot from slide
plot(Threshold,error.rate,ylim=c(0,1),ylab='Error Rate',type='l',lwd=2)
lines(Threshold,false.negative.rate,col=4,lty=2,lwd=2)
lines(Threshold,false.positive.rate,col=2,lty=3,lwd=2)

## ROC
plot(false.positive.rate,1 - false.negative.rate
     ,xlab = "False Positive Rate (1-Specificity)",ylab = "True Positive Rate (Sensitivity)"
     ,col=4,type='l',lwd=2
     )
abline(0,1,lty=3)

# Get an AUC
library(pROC)
roc(y,model$fitted.values)


### Out of sample: use predict instead of model$fitted
predict(model,newdata = data.frame(x=1.234),type='response')
