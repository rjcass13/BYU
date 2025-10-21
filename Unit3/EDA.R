mark <- read.table("TargetedMarketing.csv", sep = ';', header = TRUE)

# Define numeric columns:
numeric_cols <- c('age', 'campaign', 'pdays', 'previous')
# Define factor columns
factor_cols <- setdiff(names(mark), numeric_cols)
# Convert factor columns to factors
mark[factor_cols] <- lapply(mark[factor_cols], as.factor)

# Set all unknown or non-existant data to 'NA' throughout table
mark$pdays[which(mark$pdays == 999)] <- NA
# mean(mark$pdays, na.rm = TRUE)
