library(dplyr)
library(car)
library(glmnet)
library(pls)
river = read.csv("Rivers.csv", header = TRUE)

max = max(river$Metric)
hist(log(max + 1 - river$Metric))

pairs(river[,1:7])

# Remove columns that are just constant across each value (necessary for PCR, but also just good to do)
sds <- apply(river, 2, sd)
river = river[, sds > 0]



# Define some general cuts used later on
X = as.matrix(river[,-1], nrow = nrow(river)) # Used in LASSO, Ridge, Elastic
X_p = model.matrix(Metric ~ ., data = river)[,-1] # Used in PCR, PLS
Y = river$Metric


# LASSO
m_lasso = glmnet(x = river, y = Y, alpha = 1)
cv_lasso = cv.glmnet(x = X, y = Y, alpha = 1)
yhat_lasso <- predict(cv_lasso, newx = X, s = "lambda.min")
resid_lasso <- Y - yhat_lasso
plot(yhat_lasso, resid_lasso, main = 'LASSO Residuals vs. Fitted', xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'red')
rmse_lasso_in = sqrt(mean((resid_lasso)^2))
r2_lasso_in = 1 - sum((resid_lasso)^2)/sum((Y - mean(Y))^2)

# Ridge
m_ridge = glmnet(x = river, y = Y, alpha = 0)
cv_ridge = cv.glmnet(x = X, y = Y, alpha = 0)
yhat_ridge <- predict(cv_ridge, newx = X, s = "lambda.min")
resid_ridge <- Y - yhat_ridge
plot(yhat_ridge, resid_ridge, main = 'Ridge Residuals vs. Fitted', xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'red')
rmse_ridge_in = sqrt(mean((resid_ridge)^2))
r2_ridge_in = 1 - sum((resid_ridge)^2)/sum((Y - mean(Y))^2)

# Elastic Net
m_elastic = glmnet(x = river, y = Y, alpha = 0)
cv_elastic = cv.glmnet(x = X, y = Y, alpha = 0)
yhat_elastic <- predict(cv_elastic, newx = X, s = "lambda.min")
resid_elastic <- Y - yhat_elastic
plot(yhat_elastic, resid_elastic, main = 'Elastic Residuals vs. Fitted', xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'red')
rmse_elastic_in = sqrt(mean((resid_elastic)^2))
r2_elastic_in = 1 - sum((resid_elastic)^2)/sum((Y - mean(Y))^2)

# PCR
# Create initial model, with a semi-large value for ncomp. 
# Plot the model and pick a value that minimizes RMSE/Maximizes R2, but keeps ncomp reasonable
m_pcr.cv = pcr(Metric ~ X_p, data = river, validation="CV", scale=T, ncomp = 15)
validationplot(m_pcr.cv, main = "PCR - Components vs. RMSE") # Based on these plots, 7 looks like a good option
plot(m_pcr.cv, "validation", val.type = "R2", legendpos = "bottomright", main = "PCR - Components vs. R2") 
# Recreate model with selected number of components
m_pcr_cv = pcr(Metric ~ X_p, data = river, validation="CV", scale=T, ncomp = 7)
# Calculate Residuals and R2, show
yhat_pcr = predict(m_pcr_cv, newx = X, ncomp=7)
resid_pcr <- Y - yhat_pcr
plot(yhat_pcr, resid_pcr, main = 'PCR Residuals vs. Fitted', xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'red')
rmse_pcr_in = sqrt(mean((resid_pcr)^2))
r2_pcr_in = 1 - sum((resid_pcr)^2)/sum((Y - mean(Y))^2)

# PLS
# Create initial model, with a semi-large value for ncomp. 
# Plot the model and pick a value that minimizes RMSE/Maximizes R2, but keeps ncomp reasonable
m_plsr.cv = plsr(Metric ~ X_p, data = river, validation="CV", scale=T, ncomp = 15)
validationplot(m_plsr.cv, main = "PLSR - Components vs. RMSE") # Looking at these plots, 4 feels like a good option
plot(m_plsr.cv, "validation", val.type = "R2", legendpos = "bottomright", main = "PLSR - Components vs. R2") 
# Recreate model with selected number of components
m_plsr.cv = plsr(Metric ~ X_p, data = river, validation="CV", scale=T, ncomp = 4)
# Calculate Residuals and R2, show
yhat_plsr = predict(m_plsr.cv, newx = X, ncomp=7)
resid_plsr <- Y - yhat_plsr
plot(yhat_plsr, resid_plsr, main = 'PLSR Residuals vs. Fitted', xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'red')
rmse_plsr_in = sqrt(mean((resid_plsr)^2))
r2_plsr_in = 1 - sum((resid_plsr)^2)/sum((Y - mean(Y))^2)
