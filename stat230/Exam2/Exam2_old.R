library(dplyr)

#########################################
# Questions 1-4
jedi = read.csv("Jedi_data.csv")

# all character columns to factor:
jedi$Planet <- as.factor(jedi$Planet)
jedi$Years_of_training <- as.factor(jedi$Years_of_training)
jedi$Gender <- as.factor(jedi$Gender)

# Q 1-2
q1_2 <- aov(Midi_count ~ Planet + Years_of_training + Gender, data = jedi)
summary(q1_2)

# Q 3-4
q3_4 <- aov(Midi_count ~ Years_of_training + Gender + Years_of_training*Gender, data = jedi)
summary(q3_4)

#########################################
# Questions 5-10
# DF: Total = 1 (Grand Mean) + Residuals + Factor 1 + Factor 2
# Sum of Squares: All values sum to total
q3 = 1440 - 810 - 136 - 430
# Mean squares = Sum Squares / DF
q2 = 136/2
q4 = 64/2
# F = Mean Squares / DF(Residuals)
q5 = 32/10
# Calculate F Stat vs. the Residuals DF
q6 = pf(3.2, 2, 43, lower.tail = FALSE)

#################################
# Questions 19 - 24
library(emmeans)
library(multcomp)
music = read.csv("Music.csv")
music$Instrument = as.factor(music$Instrument)
# Q20
q20 <- aov(RT ~ Experience + Instrument, data = music)
summary(q20)

# Q21
pairwise.t.test(music$RT, music$Instrument, p.adj = "bonferroni")

# Q22
# Calculate estimated marginal means (adjusted means for each group)
em <- emmeans(q20, specs = ~ Instrument)
# Get directly adjusted p-values from the emmeans object
summary(pairs(em, adjust = "bonferroni"))

# Q23
summary(glht(q20, linfct = mcp(Instrument = "Tukey")))

# Q24
q24 <- aov(RT ~ Instrument, data = music)
summary(q24)

#########################
# Q25-Q30
#########################
library(car)
sharks <- read.csv("BlueSharkLS.csv")
sharks$Gender <- as.factor(sharks$Gender)
sharks$Continent <- as.factor(sharks$Continent)

# Q26
interaction.plot(sharks$Continent, sharks$Gender, sharks$LS)

# Q28
q28 <- aov(LS ~ Gender, data = sharks)
summary(q28)

# Q29
Anova(lm(LS ~ Gender + Continent + Gender * Continent, data = sharks), type = 'II')

# Q30
q30 <- aov(LS ~ Continent, data = sharks)
summary(glht(q30, linfct = mcp(Continent = "Tukey")))

#########################
# Q39
#########################
d <- read.csv('q39.csv')
d$Lighting <- as.factor(d$Lighting)
d$Music <- as.factor(d$Music)
q39 <- aov(Study.Score ~ Lighting + Music + Lighting*Music, data = d)
summary(q39)
sds = 0
for (i in 1:3) {
  for (j in 1:2) {
    sds = sds + print(var(d$Study.Score[which(d$Music == i & d$Lighting == j)])) 
  }
}
sqrt(sds/6)
