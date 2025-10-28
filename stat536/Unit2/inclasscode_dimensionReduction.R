library(pls)
library(MASS)
library(glmnet)
rm(list=ls())

### Load data
# It's not a csv, so we use read.table and have to tell it that spaces seperate the entries and there is a header row of column names
gene = read.table('GeneExpression-reduced.txt',sep = ' ',header=T)


# Linear selection
#linear = lm(Malignant ~ 1, data = gene)
#all_vars_formula <- as.formula("Malignant ~ .")
#all_vars_formula = lm(Malignant ~ ., data = gene)
#lm_aic = stepAIC(linear, scope = list(lower = linear, upper = all_vars_formula), direction = 'forward')
#summary(lm_aic)
linear = lm(Malignant ~ X37639_at + X37537_at + X837_s_at + X38087_s_at +
    X35748_at + X33451_s_at + X32647_at + X1260_s_at + X34189_at +
    X32891_at + X39298_at + X39976_at + X33921_at + X41724_at +
    X41714_at + X2006_at + X1098_at + X37370_i_at + X41733_at +
    X32532_at + X32146_s_at + X36295_at + X39660_at + X41708_at +
    X31906_at + X447_g_at + X37063_r_at + X38440_s_at + X1879_at +
    X35042_at + X40139_at + X33595_r_at + X34357_g_at + X39815_at +
    X1632_at + X1374_g_at + X375_at + X38269_at + X38717_at +
    X33291_at + X2030_at + X37829_at + X1043_s_at + X37203_at +
    X37543_at + X1380_at + X31919_at + X31649_at + X1144_at +
    X31435_at + X699_s_at + X31446_s_at + X35994_at + X36187_at +
    X291_s_at + X40177_at + X587_at + X38391_at + X38345_at +
    X520_at + X39651_at + X38133_at + X32875_at + X39817_s_at +
    X1751_g_at + X37563_at + X33503_at + X38263_at + X1377_at +
    X37928_at + X33032_r_at + X38934_at + X39742_at + X34855_at +
    X1295_at + X33545_at + X41672_at + X38563_at + X1095_s_at +
    X35189_at + X38869_at + X35015_at + X40105_at + X35627_at +
    X1135_at + X37411_at + X35364_at + X166_at + X36441_at +
    X41086_at + X39182_at + X34345_at + X482_at + X39705_at +
    X39399_at + X31424_at + X35111_at + X35528_at + X1854_at +
    X41326_at + X1909_at, data = gene)

summary(linear)

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