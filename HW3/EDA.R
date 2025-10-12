library(mgcv)
library(splines)

edu <- read.table('SchoolResults.txt',header=T)

##################
# EDA

# Looks normal!
hist(edu$Score)

# There is some collinearity (ex: income vs. lunch)
pairs(edu)

# Examine relationship of Extracurricular Spending and Score
plot(edu$Lunch, edu$Score)
plot(edu$Computer, edu$Score)
plot(edu$Expenditure, edu$Score)
plot(edu$Income, edu$Score)
plot(edu$English, edu$Score)
plot(edu$STratio, edu$Score)

# RJ develop model excluding variables that appear not to matter
# variables that appear to be linear:
# English

# Variables that appear not to be linear: 
# Lunch
# Income 

# Variables that appear not to matter:
# Computer
# STratio
# Expenditure

########################################
# Excluding Variables: Make sure they are good to exclude
########################################
par(mfrow = c(3, 1))

# Expenditure
exp <- lm(Score ~ Expenditure, data = edu)
summary(exp)
4000*.0111 # Full Range: 44.4
y <- predict(exp, newdata = edu)
plot(edu$Expenditure, edu$Score, main = 'Expenditure vs. Score', xlab = 'Expenditure', ylab = 'Score', cex.main = 2, cex.lab = 2, cex.axis = 2)
lines(edu$Expenditure, y, col = 'red')
sqrt(mean((edu$Score - y)^2)) # RMSE: 39.9

# STratio
st <- lm(Score ~ STratio, data = edu)
summary(st)
(26-14)*3.778 # Full Range: 45
y <- predict(st, newdata = edu)
plot(edu$STratio, edu$Score, main = 'STratio vs. Score', xlab = 'STratio', ylab = 'Score', cex.main = 2, cex.lab = 2, cex.axis = 2)
lines(edu$STratio, y, col = 'red')
sqrt(mean((edu$Score - y)^2)) # RMSE: 39.9

# Computer
comp <- lm(Score ~ Computer, data = edu)
summary(comp)
3300*.00627 # Full Range: 20
y <- predict(comp, newdata = edu)
plot(edu$Computer, edu$Score, main = 'Computer vs. Score', xlab = 'Computer', ylab = 'Score', cex.main = 2, cex.lab = 2, cex.axis = 2)
lines(edu$Computer, y, col = 'red')
sqrt(mean((edu$Score - y)^2)) # RMSE: 40.4
par(mfrow = c(1, 1))

########################################
# Linear
########################################
eng <- lm(Score ~ English, data = edu)
summary(eng)
y <- predict(eng, newdata = edu)
plot(edu$English, edu$Score, main = 'English vs. Score', xlab = 'English', ylab = 'Score', cex.main = 2, cex.lab = 2, cex.axis = 2)
lines(edu$English, y, col = 'red')

########################################
# Non-Linear: Plot a linear fit and a spline to show non-linear
########################################
par(mfrow = c(2, 1))

# Lunch
lun_l <- lm(Score ~ Lunch, data = edu)
lun_s <- lm(Score ~ bs(Lunch, degree = 3), data = edu)
y_l <- predict(lun_l, newdata = edu)
plot(edu$Lunch, edu$Score, main = 'Lunch vs. Score', xlab = 'Lunch', ylab = 'Score', cex.main = 2, cex.lab = 2, cex.axis = 2)
lines(edu$Lunch, y_l, col = 'red')
lines(edu$Lunch[order(edu$Lunch)], lun_s$fitted.values[order(edu$Lunch)], col='green', lwd = 4)

# Income
inc_l <- lm(Score ~ Income, data = edu)
inc_s <- lm(Score ~ ns(Income, df = 4), data = edu)
y_l <- predict(inc_l, newdata = edu)
y_s <- predict(inc_s, newdata = edu)
plot(edu$Income, edu$Score, main = 'Income vs. Score', xlab = 'Income', ylab = 'Score', cex.main = 2, cex.lab = 2, cex.axis = 2)
lines(edu$Income, y_l, col = 'red')

lines(edu$Income[order(edu$Income)], inc_s$fitted.values[order(edu$Income)], col='green', lwd = 4)

par(mfrow = c(1, 1))

# My model is: 
# Splines: Lunch, Income
# Linear: English
# Exclude: Computer, STratio, Expenditure

# LOOCV RMSE and R2
gam_test_errors <- numeric(nrow(edu))
gam_r_sq <- numeric(nrow(edu))
for(i in 1:nrow(edu)){
  train = edu[-i,]
  test = edu[i,]

  ### GAM
  # gam will cv itself, but we want to compare OOS MSE
  model <- gam(Score ~ English + ns(Income, df = 4) + bs(Lunch, degree = 3), data = train)
  yhat <- predict(model, newdata = test)
  gam_test_errors[i] <- test$Score - yhat
  gam_r_sq[i] <- summary(model)$r.sq
}
sqrt(mean(gam_test_errors^2))
mean(gam_r_sq)

# In Sample RMSE, R2
model <- gam(Score ~ English + ns(Income, df = 4) + bs(Lunch, degree = 3), data = edu)
p <- predict(model, newdata = edu)
sqrt(mean((edu$Score - p)^2))
summary(model)$r.sq

