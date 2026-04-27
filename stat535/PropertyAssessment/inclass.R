library(tidyverse)
library(MASS)
library(leaps)

HomeSales <- read_csv("https://grimshawville.byu.edu/BYUStat535/ironco.csv")

# All model search (Greedy)
greedy_models <- regsubsets(x = HomeSales[, 2 : 12], y = HomeSales$price, 
                            method = 'exhaustive', nbest = 6, all.best = TRUE, 
                            nvmax = 12)
summary_models <- summary(greedy_models)

best_bic_row <- which.min(summary_models$bic)
coef(greedy_models, best_bic_row)
# Area, Baths, Basement, Roof, Year Built
# Excludes Lot

# BIC Hybrid search
mod <- lm(price ~ ., data = HomeSales)
n <- nrow(HomeSales)
bic_mod <- stepAIC(mod, k = log(n), direction = 'both')
steps <- nrow(bic_mod$anova) - 1 # 6
bic_mod$coefficients
# Area, Baths, Basement, Roof, Year Built
# Seems reasonable, explainable, excludes Lot



# BIC Hybrid search
mod_int <- lm(price ~ (.)^2, data = HomeSales)
bic_mod_int <- stepAIC(mod_int, k = log(n), direction = 'both')
steps_int <- nrow(bic_mod_int$anova) - 1 # 45
bic_mod_int$coefficients
# Most of the 1 way, a few 2-ways
# Lot, Area, Baths, Floors, Const, Roof, Quality, Eff_Age, 2-ways




# Do it all again without Obs 53
HomeSales_53 <- HomeSales[-53, ]
# All model search (Greedy)
greedy_models_53 <- regsubsets(x = HomeSales_53[, 2 : 12], y = HomeSales_53$price, 
                            method = 'exhaustive', nbest = 6, all.best = TRUE, 
                            nvmax = 12)
summary_models_53 <- summary(greedy_models)

best_bic_row_53 <- which.min(summary_models$bic)
coef(greedy_models_53, best_bic_row_53)
# Area, Baths, Basement, Roof, Year Built. No change

# BIC Hybrid search
mod_53 <- lm(price ~ ., data = HomeSales_53)
n_53 <- nrow(HomeSales_53)
bic_mod_53 <- stepAIC(mod_53, k = log(n_53), direction = 'both')
steps_53 <- nrow(bic_mod_53$anova) - 1 # 6
bic_mod_53$coefficients
# Area, Baths, Basement, Roof, Year Built. Same


# BIC Hybrid search
mod_int_53 <- lm(price ~ (.)^2, data = HomeSales_53)
bic_mod_int_53 <- stepAIC(mod_int_53, k = log(n_53), direction = 'both')
steps_int_53 <- nrow(bic_mod_int_53$anova) - 1 # 46
bic_mod_int_53$coefficients
# Lot, Area, Baths, Basement, Const, Roof, Quality, Year Built, Eff_Age, less 2-ways than with 53

HomeSales[53, ]
