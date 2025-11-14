library(mgcv)
cc <- read.csv("CCFraud.csv", stringsAsFactors = TRUE)
cc <- cc[, -1] # Get rid of the 'X' column which is just the row indices

######################
# Monotonicity
######################
# Sort of Monotonic: within the heavy region on the right it is
plot(cc$V1,jitter(cc$Class), main = 'Fraudulent vs. V1', xlab = 'V1', ylab = 'Fraudulent (yes/no)')
smooth_fit <- gam(cc$Class ~ s(cc$V1))
lines(cc$V1[order(cc$V1)], predict(smooth_fit)[order(cc$V1)], col = "red", lwd = 2)

# Monotonic
plot(cc$V2,jitter(cc$Class), main = 'Fraudulent vs. V2', xlab = 'V2', ylab = 'Fraudulent (yes/no)')
smooth_fit <- gam(cc$Class ~ s(cc$V2))
lines(cc$V2[order(cc$V2)], predict(smooth_fit)[order(cc$V2)], col = "red", lwd = 2)

# Monotonic
plot(cc$V3,jitter(cc$Class), main = 'Fraudulent vs. V3', xlab = 'V3', ylab = 'Fraudulent (yes/no)')
smooth_fit <- gam(cc$Class ~ s(cc$V3))
lines(cc$V3[order(cc$V3)], predict(smooth_fit)[order(cc$V3)], col = "red", lwd = 2)

# Monotonic
plot(cc$V4,jitter(cc$Class), main = 'Fraudulent vs. V4', xlab = 'V4', ylab = 'Fraudulent (yes/no)')
smooth_fit <- gam(cc$Class ~ s(cc$V4))
lines(cc$V4[order(cc$V4)], predict(smooth_fit)[order(cc$V4)], col = "red", lwd = 2)

# Once there are rejections, yes, it's monotonic
plot(cc$V5,jitter(cc$Class), main = 'Fraudulent vs. V5', xlab = 'V5', ylab = 'Fraudulent (yes/no)')
smooth_fit <- gam(cc$Class ~ s(cc$V5))
lines(cc$V5[order(cc$V5)], predict(smooth_fit)[order(cc$V5)], col = "red", lwd = 2)

# Iffy
plot(cc$V6,jitter(cc$Class), main = 'Fraudulent vs. V6', xlab = 'V6', ylab = 'Fraudulent (yes/no)')
smooth_fit <- gam(cc$Class ~ s(cc$V6))
lines(cc$V6[order(cc$V6)], predict(smooth_fit)[order(cc$V6)], col = "red", lwd = 2)

# Monotonic
plot(cc$V7,jitter(cc$Class), main = 'Fraudulent vs. V7', xlab = 'V7', ylab = 'Fraudulent (yes/no)')
smooth_fit <- gam(cc$Class ~ s(cc$V7))
lines(cc$V7[order(cc$V7)], predict(smooth_fit)[order(cc$V7)], col = "red", lwd = 2)



#####################
# Bagging
#####################
library(ranger)
library(vip)
library(pdp)
library(ggplot2)
set.seed(1337)
non_fraud <- cc[which(cc$Class == 0), ]
fraud <- cc[which(cc$Class == 1), ]
n_fraud <- nrow(fraud)
n_non_fraud <- nrow(non_fraud)

B <- 1
forests <- vector("list", length = B)
for (i in 1:B) {
  # One Bagging sample
  up_fraud <- fraud[sample(1:n_fraud, 1000, replace = TRUE), ]
  down_non_fraud <- non_fraud[sample(1:n_non_fraud, 10000, replace = TRUE), ]

  train <- rbind(up_fraud, down_non_fraud)
  y_train <- train$Class
  x_train <- train[, colnames(train) != 'Class']
  x_test <- fraud[, colnames(train) != 'Class']

  forests[[i]] <- ranger(Class ~ ., data=cc, num.trees = 200
    #, mtry = 3
    , importance = "impurity", probability = T)  
}

f <- forests[[1]]
vip(f, num_features = 29)
