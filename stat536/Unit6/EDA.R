library(ggplot2)
library(ranger)
library(vip)
ltr <- read.csv('letter-recognition.csv', stringsAsFactors = TRUE)

# Basic box plot of 'outcome' by 'group1' and 'group2'
ggplot(ltr, aes(x = letter, y = ybar)) +
  geom_boxplot() +
  labs(title = "Box Plots of Outcome by Group1 and Group2",
       x = "Group 1",
       y = "Outcome Variable") +
  theme_minimal()

# Most useful variables appear to be xbar, ybar, xege, yege, width
# Most identifiable letters appear to be A, H, I, L, M, T, W

f <- ranger(letter ~ ., data  = ltr, num.trees = 200
  #, mtry = 3
  , classification = TRUE, importance = "impurity")
f$prediction.error
vip(f)
