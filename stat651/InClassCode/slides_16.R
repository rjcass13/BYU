library(mvtnorm)
nt <- 300
tt <- seq(-1, 1, length = nt)

Covfn <- function(tau2, phi, d) {
  tau2 * exp(-phi * d)
}
Covfn <- function(tau2, phi, d) {
  tau2 * exp(-phi * d^2)
}

# dist gives just a triangle matrix, use as.matrix to fill in whole thing
D <- dist(tt) |> as.matrix()

tau2 <- 2
phi <- .5

Sig <- Covfn(tau2, phi, D)
mu <- rep(0, nt)


y <- rmvnorm(1, mean = mu, sigma = Sig)

plot(tt, y, type = 'l', ylim = c(-4, 4))


# 2D example
n_latlon <- 50
lat <- seq(-5, 5, length = n_latlon)
lon <- seq(-5, 5, length = n_latlon)
latlon <- expand.grid(lat = lat, lon = lon)

D <- dist(latlon) |> as.matrix()

tau2 <- 2
phi <- .5

Sig <- Covfn(tau2, phi, D)
mu <- rep(0, n_latlon^2)

Y <- matrix(c(rmvnorm(1, mean = mu, sigma = Sig)), nrow = n_latlon)
image(lat, lon, Y)




## Posterior (interpolation)
nt <- 300
t_obs <- c(-.6, .1, .5)
y <- c(3, -1, .2)
n <- length(y)
tt <- seq(-1, 1, length = nt)

ttt <- c(tt, t_obs)

tau2 <- 1
phi <- 3

D <- as.matrix(dist(ttt))
D_tt <- D[1:nt, 1:nt]
D_tto <- D[1:nt, (nt+1):(nt+n)]
D_oo <- D[(nt+1):(nt+n), (nt+1):(nt+n)]

sig_tt <- Covfn(tau2, phi, D_tt)
sig_tto <- Covfn(tau2, phi, D_tto)
sig_oo <- Covfn(tau2, phi, D_oo)

mu_tt <- rep(0, nt)
mu_oo <- rep(0, n)

mu_ttgo <- mu_tt + sig_tto %*% solve(sig_oo, y - mu_oo)
sig_ttgo <- sig_tt - sig_tto %*% solve(sig_oo, t(sig_tto))

Y <- matrix(NA, nrow = 100, ncol = nt)

for (i in 1:100) {
  Y[i,] <- rmvnorm(1, mean = mu_ttgo, sigma = sig_ttgo)
}

plot(t_obs, y, ylim = c(-2, 6), xlim = c(-1, 1))
for (i in 1:100) {
  lines(tt, Y[i,], col = i)
}

