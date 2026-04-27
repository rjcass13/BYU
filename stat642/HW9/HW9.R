two_log <- function(x, p0) {
  n <- length(x)
  x_bar <- mean(x)
  sum_x <- sum(x)
  -2 * (sum_x*(log(p0) + log(1-x_bar) - log(x_bar) - log(1-p0)) + n*(log(1-p0) - log(1-x_bar)))
}

p0 <- .25

n_sim <- 10000
n <- 10
res <- numeric(n_sim)
for (i in 1:n_sim) {
  x <- rbinom(n = n, size = 1, prob = p0)
  res[i] <- two_log(x, p0)
}

hist(res, freq = FALSE)
curve(dchisq(x, df = 1), add = TRUE, col = 'red')
