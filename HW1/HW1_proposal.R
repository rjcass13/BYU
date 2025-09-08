library(dplyr)
kbb <- read.csv('kbb.csv', header = TRUE)
head(data, 10)

sum <- summary(data)


library(skimr)
skim <- skim(data)

hist(kbb$Price)
plot(kbb$Mileage, kbb$Price)
plot(kbb$Mileage, kbb$Price, col = factor(kbb$Make), pch = 19)

mod = lm(Price ~ ., data = kbb)
plot(mod)
