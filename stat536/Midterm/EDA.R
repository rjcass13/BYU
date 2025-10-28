library(dplyr)
library(mgcv)
library(splines)
lp <- read.csv("LodgepoleInUintas.csv")
pred_indeces <- which(is.na(lp$Lodgepole))

pred <- lp[pred_indeces, ]
lp <- lp[-pred_indeces, ]


#######################
# Normality
#######################
# Not normal
hist(lp$Lodgepole, breaks = 15)
# Log makes it very normal for most of it, but has a low outlier
hist(log(lp$Lodgepole), breaks = 15)
# Very normal for a lot, but gets rough on lower bound
qqnorm(log(lp$Lodgepole), pch = 1, frame = FALSE)
qqline(log(lp$Lodgepole), col = "steelblue", lwd = 2)
# Sqrt makes it appear relatively normal
hist(sqrt(lp$Lodgepole), breaks = 15)
# Not quite as tight throughout, but much better behavior overall
qqnorm(sqrt(lp$Lodgepole), pch = 1, frame = FALSE)
qqline(sqrt(lp$Lodgepole), col = "steelblue", lwd = 2)



#######################
# Which factors matters?
#######################
# I feel fairly good assuming no-colinearity, at least in 1:1
pairs(lp[, 1:5])

plot(lp$LON, sqrt(lp$Lodgepole)) #probably linear, maybe exclude?
plot(lp$LAT, sqrt(lp$Lodgepole)) #probably linear, maybe exclude?
plot(lp$Slope, sqrt(lp$Lodgepole)) # Probably non-linear, want to have a graph that dives down into that low region. Variance much higher at lower slope
plot(lp$Aspect, sqrt(lp$Lodgepole)) # Seems to be U-shaped (makes sense as the start and finish are actually the same value)
plot(lp$ELEV, sqrt(lp$Lodgepole)) # weird normal shape, LOTS of variability in middle

# Confirming good to remove Lon/Lat
lon_l <- lm(Lodgepole ~ LON, data = lp)
summary(lon_l)$r.sq # .09
lat_l <- lm(Lodgepole ~ LAT, data = lp)
summary(lat_l)$r.sq # .123
slope_l <- lm(Lodgepole ~ Slope, data = lp)
summary(slope_l)$r.sq # .07

# Fit a test model and see what elements are 
mod <- gam(sqrt(Lodgepole) ~ LON + LAT + Slope + bs(Aspect, df = 3) + ns(ELEV, df = 4), data = lp)
summary(mod) 
# Slope can be found to be significant in some cases, but only the linear term (higher degress didn't matter)
# As the degree for ELEV increases, the impact of slope decreases
# Check assumptions
library(gratia)
appraise(mod) # Does appear normal, linear, and with equal variance

# Important factors:
# LAT, LON, Aspect, ELEV




#######################
# Testing Models
#######################
set.seed(1337)
ts <- sample(1:nrow(lp),20)
train <- lp[-ts,]
test <- lp[ts,] 

mod <- gam(sqrt(Lodgepole) ~ LON + LAT + Slope + bs(Aspect, df = 3) + ns(ELEV, df = 3), data = lp, method = "REML")
# yhat <- predict(mod, newdata = test)
# yhat <- predict(mod, newdata = lp)
# RMSE <- sqrt(mean((test$Lodgepole - yhat)^2))
# RMSE





# Try a LOESS model. Can only use 4 factors, so remove Slope
mod <- loess(sqrt(Lodgepole) ~ LON + LAT + Aspect + ELEV, data = lp)
summary(mod)
yhat <- predict(mod, newdata = test)
RMSE <- sqrt(mean((test$Lodgepole - yhat)^2))
RMSE


