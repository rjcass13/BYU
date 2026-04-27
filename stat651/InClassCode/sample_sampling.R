# Simulate a hierarchical bayesian model: Dating example
# slides2_modeling : slide 11

# Draw from Prior (theorized interest of our date)
nsim <- 10000
a <- 5
b <- 1
theta <- rbeta(nsim, a, b)
hist(theta)

# Simulate X | Theta
n <- 6 # Possible choices
X <- matrix(NA, nrow = nsim, ncol = n)

for (i in 1:nsim) {
  X[i,] <- rbinom(n, size = 1, theta[i])
}

Y <- cbind(theta, X)
head(Y)

# Using the Predictive Prior calculated in slides:
x_tilde <- 1
gamma(a + x_tilde) * gamma(b + 1 - x_tilde)/(gamma(a) * gamma(b) * (a+b)) 
x_tilde <- 0
gamma(a + x_tilde) * gamma(b + 1 - x_tilde)/(gamma(a) * gamma(b) * (a+b))

# Compare to simulated data:
colMeans(X)
