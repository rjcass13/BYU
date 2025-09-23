library(dplyr)
library(car)
library(glmnet)
library(pls)
river = read.csv("Rivers.csv", header = TRUE)
pairs(river[,1:7])


#using only a subset so we can visualize
train_range = sample(1:102,80)
train_data = river[train_range,]
test_data = river[-train_range,]

# LASSO
lasso.model = glmnet(x=river, y=river$Metric, alpha = 1)
avPlots(lasso.model)
# Ridge
ridge.model = glmnet(x=river, y=river$Metric, alpha = 0)
# Elastic Net
elastic.model = glmnet(x=river, y=river$Metric, alpha = .5)
# PCR
X = model.matrix(Metric ~ ., data = river)[,-1]
model.cv = pcr(Metric ~ X, data=river, validation="CV", scale=T, ncomp = 40)
# PLS
