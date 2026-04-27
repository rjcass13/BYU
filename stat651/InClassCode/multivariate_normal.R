mu <- c(0, 1, -2)
sig <- matrix(data = c(4, 1, 0, 1, 2, -0.5, 0, -0.5, 6), nrow = 3)

L <- t(chol(sig)) # Normally gives upper, transpose to lower

z <- rnorm(3)

mu + L %*% z

n_sim <- 1000

Z <- matrix(rnorm(3*n_sim), nrow = 3)

X <- mu + L %*% Z
hist(X[1, ], freq = F)
curve(dnorm(x, 0, sqrt(4)), add = TRUE)


rowMeans(X)
cov(t(X))

cov(X[1, ], X[2, ])
