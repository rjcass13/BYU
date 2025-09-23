library(dplyr)
library(car)
library(glmnet)
river = read.csv("Rivers.csv", header = TRUE)
pairs(river[,1:7])


#using only a subset so we can visualize
train_range = sample(1:102,80)
train_data = river[train_range,]
test_data = river[-train_range,]

lasso.model = glmnet(x=river, y=river$Metric, alpha = 1)
avPlots(lasso.model)
