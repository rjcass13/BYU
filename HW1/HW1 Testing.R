library(dplyr)
setwd(getSrcDirectory(function(){})[1])
kbb <- read.csv('KBB.csv', header = TRUE)

factor_cols = c('Make', 'Model', 'Trim', 'Type', 'Cylinder', 'Liter', 'Doors', 'Cruise', 'Sound', 'Leather')

kbb[factor_cols] <- lapply(kbb[factor_cols], factor)

model = lm(Price ~ ., data = kbb)

summary(model)

