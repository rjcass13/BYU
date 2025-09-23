gene_data = read.table("GeneExpression-reduced.txt", header = TRUE)

# EDA
hist(gene_data$Malignant)
# Shows not normal distirbution of y

pairs(gene_data[1:50, 1:5])
# Shows not a linear relationship between covariates and output
# Also shows lots of interaction between covariates (colinearity)

# Given the requirements of the questions, we want to use a Linearity model
# However, lots of issues with linearity in this
# 1. We have much fewer data points than we have possible predictors
# 2. Colinearity
# 3. Potential outliers
# 4. Non-linear relationships


# Issues with high dimensionality (more predictors than data): Overfitting, False Positives
# Need a low-dimensional representation (sparsity or parsimony), add constraints to model
# Options: Variable Selection, Shrinkage/Regularization (penalized least squares), 
#   combining predictors into M << p (patient) variables

# Shrinkage/Regularization
# Normal least squareds (find B that minimizes the least squares of B*x) but include a penalty function
#   which multiplies the shrinkage parameter (lmabda) by the sum of size of each B
# Common Size Choices
# 1. Ridge Regression: Size(B) = B^2 (gives a closed form solution for X'X)
# 2. LASSO: Size(B) = |B|
# 3. Elastic Net: Size(B) = alpha|B| + (1 - alpha)B^2 (0 <= alpha <= 1)
# Strengths: More covariates just makes the penalty stronger, so model pushed to fewer covariates
# Trades a little bit of bias for a lot less variance
# Important:
# 1. Scale matters
# 2. Dont 'shrink' the intercept
# 3. How do we pick lambda? Cross validation

# Cross Validation
# Split data into training set, test set. Fit model on training set, use model to predit outcomes in test set
# How often do we repeat? Can do a single-split. 
# K-fold: all data gets put randomly into one of k buckets. Eeach group has a turn as test set
# Leave one out validation: Each data point gets a turn being left out of the data, then perform fit

# Run full model
full.model = lm(Malignant ~ ., data=gene_data)
summary(full.model) # Lots of NAs, dimensionality too high

library(MASS)
forward.model = stepAIC(full.model,direction = "forward")
# Errors due to too many predictors


### Regularization/penalized regression
library(glmnet)
X = gene_data[,101:110] #using only a subset so we can visualize
lasso.model = glmnet(x=X,y=gene_data$Malignant, alpha = 1)
ridge.model = glmnet(x=X,y=gene_data$Malignant, alpha = 0)

# We can plot to see how the coefficients are shrunk/selected, 
plot(ridge.model)
plot(lasso.model)
# but note that neither of the axes are lambda

# these fit a variety of lambda values, but we can only use one in the final model. Which one? 
# the model gives deviance, and we could take lambda with the best deviance: 
plot(lasso.model$lambda,lasso.model$dev.ratio)
plot(ridge.model$lambda,ridge.model$dev.ratio)
# looks like lambda=0 is the best??? but that's just regression! Deviance is kinda like R2, it's not going to get better by removing variables. 


### Cross-validation!
lasso.cv = cv.glmnet(x=gene_data[,-1],y=gene_data$Malignant, alpha = 1) #won't work as x must be a matrix
X = as.matrix(gene_data[,-1],nrow = nrow(gene_data))
lasso.cv = cv.glmnet(x=X,y=gene_data$Malignant, alpha = 1)
plot(lasso.cv)
# ?cv.glmnet suggests two possible lambdas
lasso.cv$lambda.min
lasso.cv$lambda.1se #note this is default for predict() and coef() functions. 

### get coefficients:
betahats = coef(lasso.cv,s=lasso.cv$lambda.min)
sum(betahats!=0) #only 26 nonzero coefficients: much more manageable than 5000! 
# where are the p-values...



################################################
# A way to cross validate (manually)
these.data = sample(1:102,80)
train = gene_data[these.data,]
test = gene_data[-these.data,]


X_train = as.matrix(train[,-1],nrow = nrow(train))
lasso_train.cv = cv.glmnet(x=X_train,y=train$Malignant, alpha = 1)
plot(lasso_train.cv)
betahats = coef(lasso_train.cv,s=lasso_train.cv$lambda.min)
sum(betahats!=0)
train[, which(betahats != 0)]


# R-Squared
y = gene_data$Malignant
yhat = predict(lasso.cv, newx = X, s = "lambda.1se")

ss_res = sum((y - yhat)^2)
ss_tot = sum((y - mean(y))^2)
r_squared = 1 - (ss_res / ss_tot)
r_squared


# Confidence intervals from Ridge/LASSO
# LASSO is a tool, not a model (it's not proposing a distirbution, etc.)
# Ridge has closed-form solution, can calculate
# For LASSO, use bootstrapping
# Boostrap alogrithm, repeat MANY times. For b - 1, ...., B
# 1. Take a boostrap sample of size K from orirignal n points WITH REPLACEMENT
# 2. Calculate and retain Bhatb


# LASSO: Does variable selection, outperforms ridge when coefs are mostly 0, picks one of correlated variables
# Ridge: No variable selection, outperforms LASSO when lots of small coefs, shrinks correl X's towards eachother
# Elastic Net: Good balance, particularly when x's are correlated
# Assumptions: Linearity (yeah). Independence (more of just need a good sample). Normal - no. Equal Var - no

