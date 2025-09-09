library(dplyr)
setwd(getSrcDirectory(function(){})[1])
kbb <- read.csv('KBB.csv', header = TRUE)

factor_cols = c('Make', 'Model', 'Trim', 'Type', 'Cylinder', 'Liter', 'Doors', 'Cruise', 'Sound', 'Leather')

kbb[factor_cols] <- lapply(kbb[factor_cols], factor)

full_model = lm(Price ~ ., data = kbb)
summary(full_model)

# Overfitting: Including all categories. includes a bunch of NAs, now have a bunch of irrelevant values
#   ie. if all sedans are Chevys, then no point including the break out of Type (covered in Chevys)
#   More dubious overfitting: y = B + e, but we model y = B1 + B2 + e. We put in more than we should, 
#     but due to noise it's significant (less bias, more variance)


# Underfitting: Not including enough variables. Model is y = B1 + B2 + e, but we fit y = B1 + e (more bias, lower variance)


# Variable selection: 
# Best Subset Selection: For all combinations of covariates, make a model and test. Often can't do, VERY large possibilites
# Forward Selection: Start with no covariates, then add 1 variable (find the most impactful). Then add another (find most impactful), etc.
# Backward Selection: Start with all covariates, then remove 1 variable (find least impactful). Then remove another, etc.
# Hybrid: Start with none, add variable and check if removing any of the existing ones improves it

# What is the best model?
# Depends on use case. PRedicting? NEed to explain it? etc. 
# AIC: -2log(Like) + 2P (generally used to find a good predictive model)
# BIC: -2log(Like) + Plong(n) (penalizes number of predictors) (generally used to find a simple inference model)
# R2 (inherently gets better the more variables you add)
# Adjusted R2 (adds a 'penatly' for adding more variables)
# Training Set Error
# Cross Validation Error


# Stepwise selection
library(MASS)
aic_model = stepAIC(full_model,direction = "both")
summary(aic_model)
# what are all these pieces in the summary output? (back to the slides)


###
### Predicting
###
yhat = predict(aic_model) #will give y-hat's for all X's in training data
?predict.lm

###
### Check assumptions
###
plot(aic_model)

## Plot Added Variables Plots
library(car)
avPlots(aic_model)
