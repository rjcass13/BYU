# Edit this line for the correct file listing
cc = read.csv('~/CreditDebt.csv',stringsAsFactors = T)

###
###  EDA: correlation,scatterplots,boxplots,etc. 
###

# summary of dataset, one of my favorite functions!
summary(cc)

# Get indices of numeric variables.
numeric.variables = c(1:6,11)

# Correlations between numeric variables
cor(cc[,numeric.variables])
# round to two decimal digits
round(cor(cc[,numeric.variables]),2)

# That's a lot of numbers to look at, let's plot it instead
# scatterplot of each pair of numeric variables:
pairs(cc[,numeric.variables])

# Scatterplots for categorical variables are not as helpful as boxplots:
boxplot(cc$Balance ~ cc$Gender)
boxplot(cc$Balance ~ cc$Married)
boxplot(cc$Balance ~ cc$Ethnicity)
boxplot(cc$Balance ~ cc$Student)



### 
### Modeling
###

# Fit regression with all variables, which is represented by the "."
fullmodel = lm(Balance ~ .,data=cc)
summary(fullmodel) # there's summary again!!

# Stepwise selection
library(MASS)
model2 = stepAIC(fullmodel,direction = "both")
summary(model2)
# what are all these pieces in the summary output? (back to the slides)



###
### Predicting
###
yhat = predict(model2) #will give y-hat's for all X's in training data
?predict.lm



###
### Check assumptions
###
plot(model2)

## Plot Added Variables Plots
library(car)
avPlots(model2)


###
### Further helps
###

# Transformations
model3 = lm(log(Balance) ~ ., data=cc)
model4 = lm(Balance ~ sqrt(Income), data=cc)

# Interactions
model5 = lm(Balance ~ Student*Income, data=cc)

###
### To compare to other methods in the future: 
###

### Train/test
train = sample(1:nrow(cc),size = nrow(cc)/2)
summary(lm(Balance ~ Age + Limit + Income + Student,data=cc[train,]))
summary(lm(Balance ~ Age + Limit + Income + Student,data=cc[-train,]))









