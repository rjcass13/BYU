library(tidyverse)
library(knitr)

Final4 <- read_csv("https://grimshawville.byu.edu/BYUStat535/Final4.csv")

summary(Final4)


hist(Final4$audience)

kable(Final4[Final4$audience == max(Final4$audience),])
