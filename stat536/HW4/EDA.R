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



####### MODEL 1
mod1 <- glm(y ~ . - pdays - housing - loan, data = mark, family = binomial)
summary(mod1)
fin_mod1 <- stepAIC(mod1, direction = "both")
coef(fin_mod1)
# Excluded Variables
# Housing
# Loan
# AUC
predicted_probabilities1 <- predict(fin_mod1, type = "response")
auc_value1 <- auc(mark$y, predicted_probabilities1)
print(auc_value1)
plot(roc(mark$y, fin_mod1$fitted.values), col = 'blue')

####### MODEL 2
mod2 <- glm(y ~ . - pdays - housing - loan, data = mark, family = binomial(link='probit'))
fin_mod2 <- stepAIC(mod2, direction = "backward")
coef(fin_mod2)
# AUC
predicted_probabilities2 <- predict(fin_mod2, type = "response")
auc_value2 <- auc(mark$y, predicted_probabilities2)
print(auc_value2)
plot(roc(mark$y, fin_mod2$fitted.values), add = TRUE, col = "red")
confint(fin_mod2)



###### MODEL ASSUMPTIONS
library(car)
crPlots(fin_mod1, terms = ~ age + campaign + previous)
yhats <- predict(fin_mod1, type = "link")
par(mfrow = c(1, 3))
plot(mark$age, yhats, main = 'Age vs. Predicted Log-odds', xlab = 'Age', ylab = 'Log-odds')
plot(mark$campaign, yhats, main = 'Campaign vs. Predicted Log-odds', xlab = 'Campaign', ylab = 'Log-odds')
plot(mark$previous, yhats, main = 'Previous vs. Predicted Log-odds', xlab = 'Previous', ylab = 'Log-odds')
par(mfrow = c(1, 1))



###### Plot of effects
previous_range <- seq(min(mark$previous), max(mark$previous), length.out = 20)
age <- 35
job <- 'retired'
marital <- 'single'
education <- 'illiterate'
default <- 'no'
housing <- 'no'
loan <- 'no'
contact <- 'socialMedia'
month <- 'mar'
day_of_week <- 'wed'
campaign <- 1
pdays <- 999
poutcome <- 'nonexistent'

full_range <- expand.grid(age, job, marital, education, default, housing, loan,  contact, month, day_of_week, campaign, pdays, previous_range, poutcome)
colnames(full_range) <- c("age", "job", "marital", 'education', 'default', 'housing', 'loan', 'contact', 'month', 'day_of_week', 'campaign', 'pdays', 'previous', 'poutcome')

full_range$yhat <- predict(fin_mod1, newdata = full_range)
plot(previous_range, full_range$yhat)