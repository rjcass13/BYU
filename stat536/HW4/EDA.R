library(MASS)
library(pROC)
####################
## Load/Prep Data ##
####################
mark <- read.table("TargetedMarketing.csv", sep = ';', header = TRUE, stringsAsFactors = TRUE)
mark$y <- ifelse(mark$y == "no", 0, 1)

# Through testing performed below, I found that really only the '999' or 'None'
# factor of pdays was significant
mark$pdays <- ifelse(mark$pdays == "999", 0, 1)
mark$pdays <- as.factor(mark$pdays)

####################
### Jitter Plots ###
####################
# Jitter Plot - age
plot(mark$age,jitter(mark$y))
smooth_fit <- loess(mark$y ~ mark$age)
lines(mark$age[order(mark$age)], predict(smooth_fit)[order(mark$age)], col = "red", lwd = 2)

# Jitter Plot - pdays
pday_df <- mark[mark$pdays != 999, ] 
plot(pday_df$pdays,jitter(pday_df$y))
smooth_fit <- loess(pday_df$y ~ pday_df$pdays)
lines(pday_df$pdays[order(pday_df$pdays)], predict(smooth_fit)[order(pday_df$pdays)], col = "red", lwd = 2)

# Jitter Plot - campaign
plot(mark$campaign,jitter(mark$y))
smooth_fit <- loess(mark$y ~ mark$campaign)
lines(mark$campaign[order(mark$campaign)], predict(smooth_fit)[order(mark$campaign)], col = "red", lwd = 2)

# Jitter Plot - previous
plot(mark$previous,jitter(mark$y))
smooth_fit <- loess(mark$y ~ mark$previous)
lines(mark$previous[order(mark$previous)], predict(smooth_fit)[order(mark$previous)], col = "red", lwd = 2)

pairs(mark[, c(1, 11, 12, 13)])


###################
# Bucketing pdays #
###################
# See if there are better bucket to make from pdays, because the 999s are rough when numeric
# hist(mark$pdays[-which(mark$pdays == 999)])
# mark$pdays <- cut(mark$pday, 
#   breaks = c(0,3,6,9,12,15,18,21,24,27,30,33,Inf), 
#   labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', 'None'),
#   include.lowest = TRUE)


mod <- glm(y ~ ., data = mark, family = binomial)
summary(mod)
fin_mod <- stepAIC(mod, direction = "both")
coef(fin_mod)
# Excluded Variables
# Housing
# Loan


# Calculate AUC
predicted_probabilities <- predict(fin_mod, type = "response")
auc_value <- auc(mark$y, predicted_probabilities)
print(auc_value)
plot(roc(mark$y, fin_mod$fitted.values))

