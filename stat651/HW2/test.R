# 1. Define Mean and Variance
mu <- 750
sd <- 350
var <- sd*sd

# 2. Calculate Parameters
# Scale (theta) = variance / mean
# Shape (alpha) = mean / scale
scale_param <- var / mu
shape_param <- mu / scale_param

# 3. Create a sequence of x values for the plot
# Using mean +/- 5*SD for a good range
sd_val <- sqrt(var)
x <- seq(max(0, mu - 5*sd_val), mu + 5*sd_val, length=1000)

# 4. Calculate the PDF (dgamma)
y <- dgamma(x, shape=shape_param, scale=scale_param)

# 5. Plot the Density
plot(x, y, type="l", lwd=2, col="blue",
     main="Gamma Distribution (Mean=750, Var=200)",
     xlab="x", ylab="Density")