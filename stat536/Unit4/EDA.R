# read in the data
crash <- read.table('Crash.txt',header=T)

# Remove Case Number
crash <- crash[ , !names(crash) %in% c("CASENUM")]
# Type of Intersection should be factor
# REST_USE: number represents type of restraint. Should be factor
# AIR_BAG: WHICH airbag deployed. Factor
# VTRAFWAY: Type of Highway (factor)
# VNUM_LAN: # of lanes, as factor
# VSPD_LIM: Speed limit, only have in 5-75, so can treat as numeric (no categoriical)
# VALIGN: Straight vs. Curve, etc. (factor)
# VSURCOND: Roadway condition (factor)
# ALCOHOL: Factor
# HOUR: hour of day. Treat as factor?
# Basically, all factors except Speed Limit and maybe hour

# Columns to exclude from conversion (e.g., col2 and col4)
cols_to_exclude <- c("VSPD_LIM")
# Get names of columns to convert
cols_to_convert <- setdiff(names(crash), cols_to_exclude)
# Convert selected columns to factors
crash[cols_to_convert] <- lapply(crash[cols_to_convert], as.factor)

mod <- glm(SEVERITY ~ ., data = crash, family = binomial)
cut_off <- .5
pred <- predict(mod, newdata = crash)
pred_pos <- pred >= cut_off
pred_neg <- pred < cut_off
act_pos <- crash$SEVERITY == 1
p = sum(act_pos)
act_neg <- crash$SEVERITY == 0
n = sum(act_neg)
tp <- sum(pred_pos & act_pos)
fp <- sum(pred_pos & act_neg)
fn <- sum(pred_neg & act_pos)
tn <- sum(pred_neg & act_neg)
tp + fp + fn +tn

sensitivity <- tp/p
precision <- tp / (tp + fp)

summary(mod)

# library(glmnet)
# y <- as.matrix(crash$SEVERITY)
# x <- as.matrix(crash[, colnames(crash) != "SEVERITY"])
# mod <- glmnet(x, y, family = "binomial", alpha = 1)
# cvfit <- cv.glmnet(x, y, alpha = 1)
# optimal_lambda <- cvfit$lambda.min # Or cvfit$lambda.1se
# coef(cvfit, s = optimal_lambda)
library(MASS)
stepAIC(mod, direction = "backward")
fin_mod <- stepAIC(mod, direction = "both")
# Includes:
# LGT_COND, WETHER, ALOCHOL, TYP_INT, REST_USE, AIR_BAG, VNUM_LAN, VSPD_LIM, VALIGN
# Excludes: 
# VSURCOND, VTRAFWAY, HOUR
coef(fin_mod)
# Increase likeliehood: 
# LGT_COND2, 3, 4, 5 (3 is highest: Dark-Lighted)
# WEATHER3, 6, 10 (3 is highest: Sleet)
# Alcohol1 (Yes)
# TYP_INT5, 7 (7: Five point)
# REST_USE7, 8 (7 is highest, None)
# AIR_BAG1, 2, 3, 8, 9 (8 is highest: Combination, 1 is second highest: Front)
# VNUM_LAN3, 5, 6 (6 is highest: Six Lanes)
# VSPD_LIM: as speed increases
# VALIGN2, 3, 4: (4 highest: Curve, unknown. 3 is second highest: Curve left)
# Decrease likelihood:
# LGT_COND1 (Daylight)
# WEATHER2, 4, 5, 7, 11, 12 (7 and 12 highest: blowing sand and stuff)
# Alcohol2 (No)
# TYP_INT2, 3, 4, 6, 10 (10 is lowest: L-type)
# REST_USE1, 2, 3, 5 (5 is lowest: Motorcycle helmet)
# AIR_BAG7, 20 (20 is lowest: Not deployed)
# VNUM_LAN2, 4, 7 (7 is lowest: seven or more)
# VSPD_LIM: as speed decrease
# VALIGN1: Straight

library(pROC)

# Calculate AUC
predicted_probabilities <- predict(fin_mod, type = "response")
auc_value <- auc(crash$SEVERITY, predicted_probabilities)
print(auc_value)
plot(roc(crash$SEVERITY, fin_mod$fitted.values))
