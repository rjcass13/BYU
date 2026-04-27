library(tidyverse)

HomeSales <- read_csv("https://grimshawville.byu.edu/BYUStat535/ironco.csv")

summary(HomeSales)

HomeSales[HomeSales$const == 2, ]

HomeSales[HomeSales$area == 544, ]
HomeSales[HomeSales$price == 11500, ]

pairs(HomeSales[, c(2:10, 12)])
mod <- lm(price ~ ., data = HomeSales)
plot(mod)
summary(mod)
# Obs 53 and 74 stick out, but maybe not outliers
# Interaction of yr_built and eff_age
# Interaction of area, yr_bult
# Interaction of gar, yr_built
# area/lot/yr_built have positive relationship with price, age has negative

# Expected Relationship
# Lot, area, baths, gar, floors, basement, const, roof, quality yr_built: positive
# eff_age: negative

# Expected interactions: 
# lot/area, perhaps also combined with house add-ons (baths, gar, floors, bsmt)
# the quality values: const, roof, quality
# yr_built, eff_age

# Expected Non-linear?
# perhaps area/lot with price. I don't think I expect that to be linear, maybe something that starts to plateau after a while
# perhaps the quality metrics as well (though they are discrete so maybe can't just call it linear anyway)
# probably yr_built/eff_age. Be surprised is it's just linear. Maybe after a certain age it's just 'old'


model1 <- lm(price ~ ., data = HomeSales)
summary(model1)
coef(model1)


CaseA <- data.frame(lot = 0.25,
                    area = 1300,
                    baths = 2,
                    gar = 1,
                    floors = 1,
                    basmt = 0,
                    const = 4,
                    roof = 3,
                    quality = 4,
                    yr_built = 1988,
                    eff_age = 5)

predict(model1, newdata = CaseA, interval = "prediction", level = 0.90)
