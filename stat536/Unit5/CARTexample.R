ad = read.csv('~/Dropbox/Teaching/Stat 536/6 - Binary ML/Admissions.csv',stringsAsFactors = T)
head(ad)

plot(ad$GRE.Score,ad$CGPA, col = as.numeric(ad$Status), pch=16)



## 90/10 Train/test split
train <- sample(nrow(ad),round(.9*nrow(ad)))

## Using rpart
library(rpart)
tree.model <- rpart(Status~.,data=ad,subset=train,method="class",control=rpart.control(cp=0))
plot(tree.model)
text(tree.model)
plotcp(tree.model)
final.rpart <- prune(tree.model, cp=tree.model$cptable[which.min(tree.model$cptable[,"xerror"]),"CP"])
plot(final.rpart)
text(final.rpart)
