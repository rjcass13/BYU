## data: time in hours to complete hw 1
x <- c(7, 5, 5, 5, 6, 5, 10, 6, 4.5, 5, 5, 7, 7, 4.5, 4.5)
hist(x, breaks = 10) # normality ... questionable

(n <- length(x))

## sufficient statistics
(xbar <- mean(x))
(s2 <- var(x))

## function to draw from inverse-gamma distribution
rinvgamma <- function(n, shape, scale) {
  1.0 / rgamma(n, shape = shape, rate = scale)
}

## functions to draw from respective full conditional distributions (under conditionally conjugate priors)
update_mu <- function(xbar, n, sig2, m0, v0) {
  v1 <- 1.0 / (1.0/v0 + n/sig2)
  m1 <- v1 * (m0/v0 + n*xbar/sig2)
  rnorm(1, mean = m1, sd = sqrt(v1))
}

update_sig2 <- function(x, n, mu, a0, b0) {
  a1 <- a0 + n
  b1 <- b0 + sum((x - mu)^2)
  rinvgamma(1, shape = 0.5*a1, scale = 0.5*b1)
}

n_iter <- 10e3 # number of iterations

sims <- matrix(NA, nrow = n_iter, ncol = 2) # store samples
colnames(sims) <- c("mu", "sig2")
head(sims)

state <- list(mu = 150.0, sig2 = 5000.0) # initialize the state of the chain

## prior specification
m0 <- 7.0
v0 <- 2.0^2
a0 <- 5.0 # n0
b0 <- 5.0 * 10 # n0 * s0^2

## run Gibbs sampler
for (i in 1:n_iter) {
  state$mu <- update_mu(xbar = xbar, n = n, sig2 = state$sig2, 
                        m0 = m0, v0 = v0)
  
  state$sig2 <- update_sig2(x = x, n = n, mu = state$mu, 
                            a0 = a0, b0 = b0)
  
  sims[i, "mu"] <- state$mu
  sims[i, "sig2"] <- state$sig2
  
  cat(i, "of", n_iter, "\r")
}

head(sims)
tail(sims)

plot(sims[,"mu"], type = "l") # trace plot
plot(sims[,"sig2"], type = "l") # trace plot

## summarize marginal posterior of mu (and add prior to see the change)
hist(sims[,"mu"], freq = FALSE)
curve(dnorm(x, m0, sqrt(v0)), from = 3.0, to = 10.0, col = "red", add = TRUE)
