library(mgcv)
library(xtable)
cc <- read.csv("CCFraud.csv", stringsAsFactors = TRUE)
cc <- cc[, -1] # Get rid of the 'X' column which is just the row indices

######################
# Monotonicity
######################
# I tested a bunch and found these to be non-monotonic-ish
par(mfrow = c(1, 3))
plot(cc$V1,jitter(cc$Class), main = 'Fraudulent vs. V1', xlab = 'V1', ylab = 'Fraudulent (yes/no)', cex.main = 2, cex.lab = 1.5)
smooth_fit <- gam(cc$Class ~ s(cc$V1))
lines(cc$V1[order(cc$V1)], predict(smooth_fit)[order(cc$V1)], col = "red", lwd = 4)

plot(cc$V6,jitter(cc$Class), main = 'Fraudulent vs. V6', xlab = 'V6', ylab = 'Fraudulent (yes/no)', cex.main = 2, cex.lab = 1.5)
smooth_fit <- gam(cc$Class ~ s(cc$V6))
lines(cc$V6[order(cc$V6)], predict(smooth_fit)[order(cc$V6)], col = "red", lwd = 4)

plot(cc$V8,jitter(cc$Class), main = 'Fraudulent vs. V8', xlab = 'V8', ylab = 'Fraudulent (yes/no)', cex.main = 2, cex.lab = 1.5)
smooth_fit <- gam(cc$Class ~ s(cc$V8))
lines(cc$V8[order(cc$V8)], predict(smooth_fit)[order(cc$V8)], col = "red", lwd = 4)
par(mfrow = c(1, 1))

#####################
# Bagging
#####################
library(ranger)
library(vip)
library(pdp)
library(ggplot2)
set.seed(1337)
# Define the dimensions of each partition
non_fraud <- cc[which(cc$Class == 0), ]
fraud <- cc[which(cc$Class == 1), ]
n_fraud <- nrow(fraud)
n_non_fraud <- nrow(non_fraud)

# We want the following rations of up/downsampling: 20:1, 10:1, 5:1, 2:1, 1:1
B <- 5
non_fraud_counts <- c(60000, 60000, 60000, 60000, 60000)
fraud_counts <- c(1000, 1500, 2000, 2500, 3000)

# Initialize my vectors
forests <- vector("list", length = B)
sensitivity <- numeric(B)
precision <- numeric(B)
true_fraud <- numeric(B)

# Generate a random forest for each of the test scenarios and calculate performance metrics to find ideal up/downsampling
for (i in 1:B) {
  # Get sampled indices from the fraudulent and non-fraudulent partitions
  fraud_indices <- sample(1:n_fraud, fraud_counts[i], replace = TRUE)
  non_fraud_indices <- sample(1:n_non_fraud, non_fraud_counts[i], replace = TRUE)
  # Using the indices, get the corresponding training and testing sets
  fraud_samp <- fraud[fraud_indices, ]
  non_fraud_samp <- non_fraud[non_fraud_indices, ]
  fraud_test <- fraud[-fraud_indices, ]
  non_fraud_test <- non_fraud[non_fraud_indices, ]

  train <- rbind(fraud_samp, non_fraud_samp)


  rf <- ranger(Class ~ ., data = train, num.trees = 200
    #, mtry = 3
    , importance = "impurity", probability = T)  
  
  if (i == 5) {
    pred <- predict(rf, data = train)
    pred_pos <- pred$predictions[, 1] >= .5 
    pred_neg <- pred$predictions[, 1] < .5
    act_pos <- train$Class == 1
    act_neg <- train$Class == 0
    tp <- sum(pred_pos & act_pos)
    fp <- sum(pred_pos & act_neg)

    print(tp / fraud_counts[i])
    print(tp / (tp + fp))  
  }
  
  # Values calculated using the Confuscion matrix here: https://en.wikipedia.org/wiki/Sensitivity_and_specificity
  pred <- predict(rf, data = cc)
  pred_pos <- pred$predictions[, 1] >= .5 
  pred_neg <- pred$predictions[, 1] < .5
  act_pos <- cc$Class == 1
  act_neg <- cc$Class == 0
  tp <- sum(pred_pos & act_pos)
  fp <- sum(pred_pos & act_neg)

  # Caclulate and store the values
  sensitivity[i] <- tp / n_fraud
  precision[i] <- tp / (tp + fp)
  forests[[i]] <- rf
}

f <- forests[[2]]
p <- vip(f, num_features = 10) + 
  labs(title = 'Variable Importance Plot', y = 'Importance', x = 'Variable') +
  theme(plot.title = element_text(hjust = 0.5))
print(p)

vals <-vi(f)
table <- xtable(vals, type = 'latex')
print(table, include.rownames = FALSE)
