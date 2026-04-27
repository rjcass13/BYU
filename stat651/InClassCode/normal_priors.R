x <- c(7, 5, 5, 5, 6, 5, 10, 6, 4.5, 5, 5, 7, 7, 4.5, 4.5)
hist(x, breaks = 10)

n <- length(x)
xbar <- mean(x)
s2 <- var(x)

rinvgamma <- function(n, shape, scale) {
  1.0 / rgamma(n, shape = shape, rate = scale)
}

dinvgamma <- function(x, shape, scale) {
  dgamma(1.0 / x, shape = shape, rate = scale) / x^2
}

# Exploring the distribution
a1 <- (n-1)/2
b1 <- (n-1)*s2/2


nsim <- 10000

sig2 <- rinvgamma(nsim, a1, b1)
mu <- rnorm(nsim, xbar, sqrt(sig2/n))

cbind(mu, sig2)
plot(mu, sig2, pch = '.')

mean(mu)

# Posterior prob that the avg. time spent on HW1 > 7
mean(mu>7)




# Gibbs sampling
update_mu <- function(xbar, n, sig2, m0, v0) {
  v1 <- 1.0/(1.0/v0 + n/sig2)
  m1 <- v1 * (m0/v0 + n*xbar/sig2)
  rnorm(1, m1, sqrt(v1))
}

update_sig2 <- function(x, n, mu, a0, b0) {
  a1 <- a0 + n
  b1 <- b0 + sum((x-mu)^2)
  sig2 <- rinvgamma(1, a1/2, b1/2)
}

n_iter <- 10e3
sims <- as.data.frame(matrix(NA, nrow = n_iter, ncol = 2))
colnames(sims) <- c('mu', 'sig2')

# Prior for mu
m0 <- 16
v0 <- .001

state <- list(mu = 5, sig2 = mean(c(1, 4, 5))) # Just guesses of initial state

for(i in 1:n_iter) {
  state$mu <- update_mu(xbar, n, sig2 = state$sig2, m0, v0) # mu0 and v0 are the prior
  state$sig2 <- update_sig2(x, n, mu = state$mu, a0 = 5, b0 = 5*10) # a0 and b0 are the priors: a0 prior sample size, b0 n * var between students 
  sims[i, "mu"] <- state$mu
  sims[i, "sig2"] <- state$sig2
}

plot(sims[, 'mu'], type = 'l')
plot(sims[, 'sig2'], type = 'l')
hist(sims[, 'mu'], freq = FALSE)
curve(dnorm(x, m0, sqrt(v0)), from = 15, to = 17, col = 'red', add = TRUE)

