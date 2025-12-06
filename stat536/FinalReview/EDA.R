dt <- read.csv('oregon.csv')
dt[-c(3, 16)] <- lapply(dt[-c(3, 16)], factor)

library(ranger)
library(vip)
library(pdp)
library(ggplot2)
set.seed(1337)
ind <- sample(1:nrow(dt), round(nrow(dt) * .1))
train <- dt[-ind, ]
test <- dt[ind, ]
rf <- ranger(enrolled ~ ., data = train, num.trees = 200
  , mtry = 3
  , importance = "impurity", probability = T)  
p = predict(rf, data = test)
# Different Importance outputs
vip(rf, num_features = 20)

pred_in <- predict(rf, data = train)
pred_stat_in <- cut(pred$predictions[, 2], breaks = c(-1, .5, 1), labels = c(0, 1))
length(which(pred_stat_in == train$enrolled))/length(train$enrolled)

pred_stat_out <- cut(p$predictions[, 2], breaks = c(-1, .5, 1), labels = c(0, 1))
length(which(pred_stat_out == test$enrolled))/length(test$enrolled)


p1 = partial(rf,"income")
autoplot(p1, contour = TRUE)
p2 = partial(rf,"invited")
autoplot(p2, contour = TRUE)


# Invited/Not Invited Split
set.seed(1337)
inv <- dt[which(dt$invited == 1), ]
ninv <- dt[-which(dt$invited == 1), ]
ind_inv <- sample(1:nrow(inv), round(nrow(inv) * .1))
ind_ninv <- sample(1:nrow(ninv), round(nrow(ninv) * .1))
train_inv <- dt[-ind_inv, ]
test_inv <- dt[ind_inv, ]
train_ninv <- dt[-ind_ninv, ]
test_ninv <- dt[ind_ninv, ]
# Inv
rf_inv <- ranger(enrolled ~ ., data = train, num.trees = 200
  , mtry = 3
  , importance = "impurity", probability = T)  
# Different Importance outputs
vip(rf_inv, num_features = 20)
p_out_inv = predict(rf_inv, data = test_inv)
p_in_inv = predict(rf_inv, data = train_inv)
pred_stat_inv_in <- cut(p_in_inv$predictions[, 2], breaks = c(-1, .5, 1), labels = c(0, 1))
length(which(pred_stat_inv_in == train_inv$enrolled))/length(train_inv$enrolled)
pred_stat_inv_out <- cut(p_out_inv$predictions[, 2], breaks = c(-1, .5, 1), labels = c(0, 1))
length(which(pred_stat_inv_out == test_inv$enrolled))/length(test_inv$enrolled)
# Ninv
rf_ninv <- ranger(enrolled ~ ., data = train, num.trees = 200
  , mtry = 3
  , importance = "impurity", probability = T)  
# Different Importance outputs
vip(rf_ninv, num_features = 20)
p_out_ninv = predict(rf_ninv, data = test_ninv)
p_in_ninv = predict(rf_ninv, data = train_ninv)
pred_stat_ninv_in <- cut(p_in_ninv$predictions[, 2], breaks = c(-1, .5, 1), labels = c(0, 1))
length(which(pred_stat_ninv_in == train_ninv$enrolled))/length(train_ninv$enrolled)
pred_stat_ninv_out <- cut(p_out_ninv$predictions[, 2], breaks = c(-1, .5, 1), labels = c(0, 1))
length(which(pred_stat_ninv_out == test_ninv$enrolled))/length(test_ninv$enrolled)




# Binomial
library(MASS)
library(pROC)
set.seed(1337)
ind <- sample(1:nrow(dt), round(nrow(dt) * .1))
train <- dt[-ind, ]
test <- dt[ind, ]
mod <- glm(enrolled ~ ., data = train, family = binomial)
fin_mod <- stepAIC(mod, direction = "both")
coef(fin_mod)
# Calculate AUC
predicted_probabilities <- predict(fin_mod, type = "response")
auc_value <- auc(train$enrolled, predicted_probabilities)
print(auc_value)
plot(roc(train$enrolled, fin_mod$fitted.values))
