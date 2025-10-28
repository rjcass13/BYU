mark <- read.table("TargetedMarketing.csv", sep = ';', header = TRUE, stringsAsFactors = TRUE)

# Set all unknown or non-existant data to 'NA' throughout table
mark$pdays[which(mark$pdays == 999)] <- NA
# mean(mark$pdays, na.rm = TRUE)

mark$y <- ifelse(mark$y == "no", 0, 1)

# Jitter Plot - age
plot(mark$age,jitter(mark$y))
smooth_fit <- loess(mark$y ~ mark$age)
lines(mark$age[order(mark$age)], predict(smooth_fit)[order(mark$age)], col = "red", lwd = 2)

# Jitter Plot - pdays
pday_df <- mark[mark$pdays != 999, ] 
plot(pday_df$pdays,jitter(pday_df$y))
smooth_fit <- loess(pday_df$y ~ pday_df$pdays)
lines(pday_df$pdays[order(pday_df$pdays)], predict(smooth_fit)[order(pday_df$pdays)], col = "red", lwd = 2)

# Jitter Plot - campaign
plot(mark$campaign,jitter(mark$y))
smooth_fit <- loess(mark$y ~ mark$campaign)
lines(mark$campaign[order(mark$campaign)], predict(smooth_fit)[order(mark$campaign)], col = "red", lwd = 2)

# Jitter Plot - previous
plot(mark$previous,jitter(mark$y))
smooth_fit <- loess(mark$y ~ mark$previous)
lines(mark$previous[order(mark$previous)], predict(smooth_fit)[order(mark$previous)], col = "red", lwd = 2)