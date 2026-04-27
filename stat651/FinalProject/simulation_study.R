suppressPackageStartupMessages(library("rstan"))
source('functions.R')

p <- 11
super_p <- 1
alpha_true <- -2

# -------------------------------
# Shared index structure
# -------------------------------
shared_inds <- rep(0, p)

# Race - Nonwhite
shared_inds[7:9]   <- 1


# -------------------------------
# Priors (UPDATED for current model)
# -------------------------------
alpha_var <- 10

beta_means <- rep(0, p)
beta_sds   <- rep(10, p)

# optional informative priors
beta_means[grep("^age", colnames(X))]   <- 0.05
beta_means[grep("^hours", colnames(X))] <- 0.03

beta_sds[grep("^age", colnames(X))]   <- 10
beta_sds[grep("^hours", colnames(X))] <- 10

# hierarchical variance for race
race_shared_mean <- 0
race_mean_var <- 10



n <- 100
set.seed(123)

X <- generate_X(n)

beta_true <- generate_beta_true(p, shared_inds, alpha_var, prior_means = beta_means, prior_sds = beta_sds, race_hier_mean = race_shared_mean, race_mean_var)

y <- generate_data(n, X, beta_true)

data_stan <- generate_stan_data(n, p, X, super_p, y, shared_inds, alpha_var, prior_means = beta_means, prior_sds = beta_sds, race_hier_mean = race_shared_mean, race_mean_var)
fit_stan <- stan(
  model_code = readLines("final_vec.stan"),
  data = data_stan,
  pars = c("beta"),
  iter = 100,
  warmup = 20,
  chains = 4
)
post <- rstan::extract(fit_stan)


beta_est   <- apply(post$beta, 2, mean)
beta_lower <- apply(post$beta, 2, quantile, 0.025)
beta_upper <- apply(post$beta, 2, quantile, 0.975)

ord <- c(
  1:2,
  3:6,
  7:10,
  11
)


ypos <- p:1  

plot(beta_est[ord],
     ypos,
     xlim = range(c(beta_lower, beta_upper)),
     pch = 19,
     yaxt = "n",
     ylab = "Coefficient",
     xlab = "Beta value",
     main= "N = 5000")

segments(beta_lower[ord], ypos,
         beta_upper[ord], ypos)



axis(2, at = ypos, labels = ord, las = 2)

points(beta_true[ord], ypos, col = "blue", pch = 17)

