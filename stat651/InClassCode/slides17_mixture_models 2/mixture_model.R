library("palmerpenguins")
library("tidyverse")
library('coda')

str(penguins)
table(penguins$species, penguins$island)

ggplot(data = penguins %>% filter(sex == "male"), 
       aes(x = flipper_length_mm)) + 
  geom_histogram(aes(y = after_stat(density))) + 
  facet_grid(species ~ .) + xlab("Flipper length (mm)")
ggplot(data = penguins %>% filter(sex == "male"), aes(x = flipper_length_mm)) + geom_histogram()



#### Model spec
## y[i] ~ Normal(mu_vec[Z[i]], sig2_vec[Z[i]])
## Z[i] ~ Cat(w[i])
## w ~ Dir(alpha)
## mu_vec[k] ~ Normal(mu_mean, mu_var)
## sig_vec[k] ~ Unif(0, sig_max)
## mu_mean ~ Normal(m0, v0)
## mu_var ~ InvGa(a0/2, b0/2)

source("functions.R")

dat <- penguins %>% filter(sex == "male") %>% select("flipper_length_mm", "species")

y <- dat$flipper_length_mm
(n <- length(y))

prior <- list(
  mu_mean = list(family = "Normal", m0 = mean(y), v0 = (diff(range(y)) / 2)^2),
  mu_var = list(family = "Inv-Gamma", a0 = 5.0, b0 = 5.0 * (diff(range(y)) / 2)^2),
  max_sig = 50.0,
  alpha = 1.0 # Symmetric Dirichlet shape parameter
)

K <- 3 # number of mixture components
n_iter <- 2000

state <- list( # initialize the state of the chain
  Z = sample.int(K, size = n, replace = TRUE),
  w = rep(1.0 / K, K),
  mu_vec = seq(min(y), max(y), length = K),
  sig2_vec = rep(diff(range(y))/6.0, K),
  mu_mean = mean(y),
  mu_var = (diff(range(y)) / 2)^2
)

sim_Z <- matrix(NA, nrow = n_iter, ncol = n)
sim_w <- matrix(NA, nrow = n_iter, ncol = K)
sim_mu <- matrix(NA, nrow = n_iter, ncol = K)
sim_sig2 <- matrix(NA, nrow = n_iter, ncol = K)
sim_hypers <- matrix(NA, nrow = n_iter, ncol = 3)
colnames(sim_hypers) <- c("mu_mean", "mu_var", "sig_s0")


## Gibbs sampler
pb <- txtProgressBar(min = 0, max = n_iter, style = 3)
for (ii in 1:n_iter) {
  
  for (i in 1:n) { # this could be done in parallel
    state$Z[i] <- upd_Zi(yi = y[i], K = K, w = state$w, 
                         mus = state$mu_vec, sig2s = state$sig2_vec)
  }
  
  state$w <- upd_w(Z = state$Z, K = K, alpha = prior$alpha)
  
  for (k in 1:K) {
    y_k <- y[which(state$Z == k)]
    state$mu_vec[k] <- upd_nnmkv(x = y_k, v = state$sig2_vec[k], 
                                 m0 = state$mu_mean, v0 = state$mu_var)
    
    state$sig2_vec[k] <- upd_sig2(sig2_old = state$sig2_vec[k], x = y_k, 
                                  m = state$mu_vec[k], max_sig = prior$max_sig, 
                                  wslice = 5.0)
  }
  
  state$mu_mean <- upd_nnmkv(x = state$mu_vec, v = state$mu_var, 
                             m0 = prior$mu_mean$m0, v0 = prior$mu_mean$v0)
  
  state$mu_var <- upd_nigvkm(x = state$mu_vec, m = state$mu_mean, 
                             a0 = prior$mu_var$a0, b0 = prior$mu_var$a0)
  
  sim_Z[ii,] <- state$Z
  sim_w[ii,] <- state$w
  sim_mu[ii,] <- state$mu_vec
  sim_sig2[ii,] <- state$sig2_vec
  sim_hypers[ii, c("mu_mean", "mu_var")] <- c(state$mu_mean, state$mu_var)
  setTxtProgressBar(pb, ii)
}
close(pb)


plot(as.mcmc(sim_Z[,1]))
plot(as.mcmc(sim_mu))
plot(as.mcmc(sim_w))
plot(as.mcmc(sim_sig2))
effectiveSize(sim_mu[,1])


sim_cocluster <- array(NA, dim = c(n, n, n_iter))

for (ii in 1:n_iter) {
  sim_cocluster[,,ii] <- outer(sim_Z[ii,], sim_Z[ii,], FUN = "==")
}

pp_cocluster <- apply(sim_cocluster, c(1, 2), mean)
library('lattice')
levelplot(pp_cocluster)



# Density Estimation
range(y)
ny <- 1000
yy <- seq(175, 235, length = ny)

fy0 <- matrix(NA, nrow = ny, ncol = K)

ii <- 1000

for (i in 1:ny) {
  for (k in 1:K) {
    fy0[i,k] <- sim_w[ii, k] * dnorm(yy[i], mean = sim_mu[ii,k], sd = sqrt(sim_sig2[[i,k]]))
  }
}

fy <- rowSums(fy0)
plot(yy, fy, type = 'l')
