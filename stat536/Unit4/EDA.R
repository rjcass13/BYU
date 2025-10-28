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
