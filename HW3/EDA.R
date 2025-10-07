edu <- read.table('SchoolResults.txt',header=T)

##################
# EDA

# Looks normal!
hist(edu$Score)

# There is some collinearity (ex: income vs. lunch)
pairs(edu)

# Examine relationship of Extracurricular Spending and Score
plot(edu$Lunch, edu$Score)
plot(edu$Computer, edu$Score)
plot(edu$Expenditure, edu$Score)
plot(edu$Income, edu$Score)
plot(edu$English, edu$Score)
plot(edu$STratio, edu$Score)
