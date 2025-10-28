library(dplyr)
library(ggplot2)
library(tidyr)

#########################################
# Questions 1-5
#########################
lotr = read.csv("lotr_data.csv")

# all character columns to factor:
lotr$Captain <- as.factor(lotr$Captain)
lotr$Landscape <- as.factor(lotr$Landscape)
lotr$Weapon <- as.factor(lotr$Weapon)

# Q 1-2
lotr_anova_1 <- aov(Orcs ~ Captain + Landscape + Weapon, data = lotr)
summary(lotr_anova_1)

# Q 3-5
lotr_anova_2 <- aov(Orcs ~ Landscape + Weapon + Landscape*Weapon, data = lotr)
summary(lotr_anova_2)


#########################################
# Questions 6-11
#########################
# DF: Total = 1 (Grand Mean) + Residuals + Factor 1 + Factor 2
q1 = 48-43-2-1
# Sum of Squares: All values sum to total
q3 = 1520 - 907 - 146 - 421
# Mean squares = Sum Squares / DF
q2 = 146/q1
q4 = q3/q1
# F = Mean Squares / Resid. Mean Squares
q5 = q4/10
# Calculate F Stat vs. the Residuals DF
q6 = pf(q5, 2, 43, lower.tail = FALSE)
pf(7.3, 2, 43, lower.tail = FALSE)

# 13-15:
n1 = 4
n2 = 3
nrep = 4
d1 = n1 - 1
d2 = n2 - 1
dint = d1*d2
dtot = n1*n2*nrep
dres = dtot - 1 - d1 - d2 - dint


# 17-18
data <- data.frame(
  Category = c("A1", "A2", "A3"),
  B1 = c(10, 15, 12),
  B2 = c(12, 17, 14)
)

# Convert to long format
data_long <- data %>%
  pivot_longer(cols = c(B1, B2),
               names_to = "FactorB",
               values_to = "Value")

ggplot(data_long, aes(x = Category, y = Value, color = FactorB, group = FactorB)) +
  geom_line(size = 1) +  # Adjust line thickness
  labs(x = "Factor A Levels",
       y = "Response Variable") +
  theme_minimal() # Use a clean theme

#########################
# Q19 - 24
#########################
library(emmeans)
library(multcomp)
racer = read.csv("ReactionTime.csv")
racer$RaceType = as.factor(racer$RaceType)
q20 <- aov(ReactionTime ~ Experience + RaceType, data = racer)
summary(q20)

# Q21
# alpha divides by number of pairwise tests (alpha .05 become .05/3)
pairwise.t.test(racer$ReactionTime, racer$RaceType, p.adj = "bonferroni")
# .0292 > .05/3, so NASCAR x Formula1 not significant

# Q22
# Calculate estimated marginal means (adjusted means for each group)
em <- emmeans(q20, specs = ~ RaceType)
# Get directly adjusted p-values from the emmeans object
summary(pairs(em, adjust = "bonferroni"))
## DONE THROUGH 21

# Q23
summary(glht(q20, linfct = mcp(RaceType = "Tukey")))

# Q24
q24 <- aov(ReactionTime ~ RaceType, data = racer)
summary(q24)

#########################
# Q25-Q30
#########################
falcons <- read.csv("Falcons.csv")
falcons$Gender <- as.factor(falcons$Gender)
falcons$Habitat <- as.factor(falcons$Habitat)

# Q26
interaction.plot(falcons$Habitat, falcons$Gender, falcons$TopSpeed)

# Q28
q28 <- aov(TopSpeed ~ Gender, data = falcons)
summary(q28)

# Q29
Anova(lm(TopSpeed ~ Gender + Habitat + Gender * Habitat, data = falcons), type = 'II')

# Q30
library(car)
q30 <- aov(TopSpeed ~ Habitat, data = falcons)
summary(glht(q30, linfct = mcp(Habitat = "Tukey")))

# Done through 38
