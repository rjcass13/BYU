library(dplyr)

kbb = read.csv("KBB.csv")

########################
# Assumptions to check #
########################
# Linearity
library(car)
lm = lm(Price ~ Mileage, data = kbb)
avPlots(lm)
# Plot appears linear, yes

# Independence
# Comes mostly from experience: do I think all the input variables are independent?
# In this case, model/trim is directly dependent on make

# Normality
# Look at distribution of Price. Did so using the Positron dat explorer.
# Price is non-normal with heavy right tail
# Also, can use the following (2nd graph is QQ plot)
plot(lm)
# Resulting graph does not follow the straight line, implies non-normal

# Equal Variance
plot(lm)
# First output is Residuals vs. Fitted: 
# it appears relatively even the whole way across, think we're good to assume



# Other issues that might exist
# Collinearity (two or more variables move together)
# Interactions (one variable impacts how another variable impacts the outcome)


#####################################
# Accounting for Missed Assumptions #
#####################################
# Normality: Transform the data
# Collinearity: Only use one of the values
# Interactions: Include the interaction term in the model


# Transformation to adjust normality
log_transformation = lm(log(Price) ~ ., data=kbb)
plot(log_transformation)
# Normal looks good (QQ PLot) however, looking at the new residuals plot it no longer looks equal variance
root_transformation = lm(sqrt(Price) ~ ., data=kbb)
plot(root_transformation)
# Makes normal look good (QQ Plot), residuals look good (residuals vs. fitted)


