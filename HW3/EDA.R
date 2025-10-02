edu <- read.table('SchoolResults.txt',header=T)

##################
# EDA

# Looks normal!
hist(edu$Score)

# There is some collinearity (ex: income vs. lunch)
pairs(edu)
