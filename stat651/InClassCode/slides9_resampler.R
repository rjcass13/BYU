curve(x^2*(1-x), from = 0, to = 1)

curve(0.1 * dbeta(x, 2, 2), add = TRUE, col = 'red')

nsim <- 10e3
x <- rbeta(nsim, 2, 2)
w <- x^2*(1-x) / dbeta(x, 2, 2)
wn <- w/sum(w)

# Find normalizing constant
chat <- 1/mean(w)

curve(x^2*(1-x)*chat, from = 0, to = 1)
curve(dbeta(x, 3, 2), add = TRUE, col = 'red')

# P(theta < 0.5)
sum((x < 0.5)*wn)
pbeta(.5, 3, 2)

# Estimate E(theta)
sum(wn*x)
# Truth is 3/5

# Generate samples with importance resampling
# This sample should be much smaller than the initial draw
nsim2 <- 1000
x_resample <- sample(x, size = nsim2, replace = TRUE, prob = wn)
hist(x_resample, freq = FALSE)
curve(dbeta(x, 3, 2), add = TRUE, col = 'red')

