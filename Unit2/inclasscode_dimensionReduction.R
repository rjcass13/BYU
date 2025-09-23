library(pls)
rm(list=ls())

### Load data
# It's not a csv, so we use read.table and have to tell it that spaces seperate the entries and there is a header row of column names
gene = read.table('GeneExpression-reduced.txt',sep = ' ',header=T)

X = as.matrix(gene[,-1],nrow = nrow(gene))
# LASSO
lasso.model = glmnet(x=gene, y=gene$Malignant, alpha = 1)
lasso.cv = cv.glmnet(x=X,y=gene$Malignant, alpha = 1)
yhat_lasso = predict(lasso.cv, newx = X, s = lasso.cv$lambda.1se)
resid_lasso = gene$Malignant - yhat_lasso
plot(yhat_lasso, resid_lasso, main = 'Lasso Residuals')
abline(h = 0, col = 'red')
rmse_lasso_in = sqrt(mean((gene$Malignant - yhat_lasso)^2))
r2_lasso_in = 1 - sum((gene$Malignant - yhat_lasso)^2)/sum((gene$Malignant - mean(gene$Malignant))^2)
# Conclusion: Does not appear to meet linear assumption

# Ridge
ridge.model = glmnet(x=gene, y=gene$Malignant, alpha = 0)
ridge.cv = cv.glmnet(x=X,y=gene$Malignant, alpha = 0)
yhat_ridge = predict(ridge.cv, newx = X, s = ridge.cv$lambda.1se)
resid_ridge = gene$Malignant - yhat_ridge
plot(yhat_ridge, resid_ridge)
abline(h = 0, col = 'red')
rmse_ridge_in = sqrt(mean((gene$Malignant - yhat_ridge)^2))
r2_ridge_in = 1 - sum((gene$Malignant - yhat_ridge)^2)/sum((gene$Malignant - mean(gene$Malignant))^2)
# Conclusion: Does not appear to meet linear assumption



### Dimension Reduction - there are several ways to do this in R. Here's one.
X = model.matrix(Malignant~., data = gene)[,-1] # X is a matrix of all but the first column of gene
model.cv = pcr(Malignant ~ X, data=gene, validation="CV", scale=T, ncomp = 20)
# `scale=TRUE` will make sure you scale the explanatory variables  
# The `validation` argument will run some cross-validation for you to determine the number of components to use. 

# summary(pcr_obj) prints out some nice summary statistics to help you choose the number of components
summary(model.cv)
# validationplot(pcr_obj)is a visual way of determining the number of components.
validationplot(model.cv)
plot(model.cv, "validation", val.type = "R2", legendpos = "bottomright") # R >= 2.1.0

# fits a principal component regression model with at most `ncomp` components.  
# model.2 = pcr(Malignant ~ X, data=gene, ncomp=2, scale=T)

# In-sample RMSE
sqrt(mean((gene$Malignant - predict(model.cv, X,ncomp=20))^2))

# Predict a single observation, we need to keep it as a matrix with ,drop=FALSE
predict(model.cv, X[1,,drop=FALSE],ncomp=20) 





# `plsr()` follows the same syntax as `pcr` but does partial least squares instead.
model_plsr.cv = plsr(Malignant ~ X, data=gene, validation="CV", scale=T, ncomp = 40)
summary(model_plsr.cv) 
# validationplot(pcr_obj)is a visual way of determining the number of components.
validationplot(model_plsr.cv)
plot(model_plsr.cv, "validation", val.type = "R2", legendpos = "bottomright") # R >= 2.1.0
# In-sample RMSE
sqrt(mean((gene$Malignant - predict(model_plsr.cv, X,ncomp=5))^2))
# Predict a single observation, we need to keep it as a matrix with ,drop=FALSE
predict(model_plsr.cv, X[1,,drop=FALSE],ncomp=5) 


# Assumptions for PCR and PLS
# Same LINE assumptions, but applied to the model fit