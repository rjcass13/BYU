mark <- read.table("TargetedMarketing.csv", sep = ';', header = TRUE, stringsAsFactors = TRUE)

# Set all unknown or non-existant data to 'NA' throughout table
mark$pdays[which(mark$pdays == 999)] <- NA
# mean(mark$pdays, na.rm = TRUE)
