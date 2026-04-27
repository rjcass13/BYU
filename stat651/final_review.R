# Gapminder: LifeExpect_ct = beta_1 ln(GDPP) + beta_2 year + beta_3 continent
library(gapminder)
library('coda')

y <- as.matrix(gapminder$lifeExp)

#-1 removes the intercept
X <- model.matrix(~ -1 + log(gdpPercap) + I(year - min(year)) + continent, data = gapminder)
colnames(X)

summary(lm(y~-1+X))
# can look at RSE: sigma^2 should be about that value squared
5.813^2

lm_bayes <- function(y, X, iters = 5000, burnin = 100, 
  inits = list(
    beta = rep(0, ncol(X)),
    gamma = 1/100
  ),
  consts = list(
    # Prior params for betas
    beta_0 = rep(0, ncol(X)),
    Sigma_0_inv = solve(100000*diag(ncol(X))),
    # Prior params for sigma^2
    nu_0 = 0.01,
    sigma2_0 = 1
  )
) {

  n <- nrow(X)
  p <- ncol(X)

  samps <- matrix(nrow = iters, ncol = p+1)

  gamma <- inits$gamma
  beta <- inits$beta

  # Precompute some stuff that will be used in the loop
  XtX <- crossprod(X) # Apparently crossprod is more efficient than %*%. Look into for my research
  Xty <- crossprod(X, y)
  Xty_prior <- crossprod(consts$Sigma_0_inv, consts$beta_0)
  shape_prior <- (consts$nu_0 + n)/2
  rate_prior <- consts$nu_0 * consts$sigma2_0

  for (i in 1:(iters+burnin)) {
    # Update beta | gamma, data
    A <- gamma*XtX + consts$Sigma_0_inv
    A_inv <- solve(A)
    B <- gamma*Xty + Xty_prior
    beta <- MASS::mvrnorm(n=1, A_inv%*%B, A_inv)
    
    # Update gamma | beta, data
    SSR <- crossprod(y - X%*%beta)
    gamma <- rgamma(1, shape = shape_prior, rate = .5*(SSR + rate_prior))

    if (i > burnin){
      samps[i-burnin, ] <- c(beta, 1/gamma)
    }
  }
  samps
}

# To run on multiple chains
samples <- vector("list", 4)
for (chain in 1:length(samples)) {
  set.seed(1337+chain)
  beta_start <- MASS::mvrnorm(n=1, mu = rep(0, 7), Sigma = 10000*diag(7))
  gamma_start <- rgamma(1, 5, 10)
  samples[[chain]] <- lm_bayes(y, X, inits = list(beta = beta_start, gamma = gamma_start))
}

colMeans(samples[[1]])


# MCMC Diagnostics

# Gelman-Rubin (Rhat)
# - Run on multiple chains only
# - Compares varoance within each chainto variance between chains
# - coda::gelman.diag
# Bad if Rhat > 1.1
# - Increase burnin
# - potential multimodailty (identifiability) issue
# - potential poor mixing (change sampler or tuning parameters)

# Raftery-Lewis
# - Run on single chain only
# - Have you run your chain long enouch to estimate 2.5th percentile (default in coda) up to MC error
# - coda::raftery.diaf
# Bad if I > 5
# - Increase thinng, or try reparameterizing

# Effective Smaple Size
# - Eithe single or multiple chains
# - How much information the posterior sample contains
# - coda::effectiveSize
# Bad if really low compared to smaple
# - Try reparameterization, center/scaling


run_diagnostics <- function(samples_list) {
  mcmc_obj <- coda::mcmc.list(
    lapply(samples_list, coda::mcmc)
  )

  raftery_results <- lapply(mcmc_obj, coda::raftery.diag)
  names(raftery_results) <- paste0("Chain_", 1:length(raftery_results))

  list(
    Samples = mcmc_obj,
    Gelman = coda::gelman.diag(mcmc_obj),
    Raftery = raftery_results,
    ESS = coda::effectiveSize(mcmc_obj)
  )
}

samples_diag <- run_diagnostics(samples)

samples_diag$Gelman
samples_diag$Raftery
samples_diag$ESS

coda::traceplot(samples_diag$Samples)

allsamps <- as.matrix(samples_diag$Samples)


#######################
# Heteroskedasticity example

W <- cbind(1, X[,2])

# Use pen/paper to get log full conditional
log_theta <- function(theta, W, e_hat, theta_0, Sigma_theta_inv) {
  w_theta <- W%*%theta
  log_lik <- -.5*sum(w_theta + e_hat^2 / exp(w_theta))
  diff <- theta-theta_0
  log_prior <- -.5*as.numeric(t(diff)%*% Sigma_theta_inv %*% diff)
  log_lik + log_prior
}

lm_bayes_hetero <- function(y, X, W, iters = 5000, burnin = 100, 
  inits = list(
    beta = rep(0, ncol(X)),
    theta = c(0,0)
  ),
  consts = list(
    # Prior params for betas
    beta_0 = rep(0, ncol(X)),
    Sigma_0_inv = solve(100000*diag(ncol(X))),
    # Prior params for sigma^2
    theta_0 = c(0,0),
    Sigma_theta_inv = solve(100000*diag(2)),
    # Proposal Distribution
    V = 1/100*matrix(c(0.46, -0.012, -0.012, 0.00045), nrow=2, byrow=TRUE)
  )
) {

  n <- nrow(X)
  p <- ncol(X)

  samps <- matrix(nrow = iters, ncol = p+2)

  theta <- inits$theta
  beta <- inits$beta

  # Precompute some stuff that will be used in the loop
  Xty_prior <- crossprod(consts$Sigma_0_inv, consts$beta_0)

  acc <- 0
  for (i in 1:(iters+burnin)) {
    # Update beta | Gamma, data
    prec_w <- 1/exp(W%*%theta)
    A <- t(X*as.numeric(prec_w))%*%X + consts$Sigma_0_inv
    A_inv <- solve(A)
    B <- t(X*as.numeric(prec_w))%*%y + Xty_prior
    beta <- MASS::mvrnorm(n=1, A_inv%*%B, A_inv)
    
    # Update gamma | beta, data
    e_hat <- y - X%*%beta

    theta_prop <- MASS::mvrnorm(n=1, mu = theta, Sigma = consts$V)

    log_r <- log_theta(theta_prop, W, e_hat, consts$theta_0, consts$Sigma_theta_inv) - log_theta(theta, W, e_hat, consts$theta_0, consts$Sigma_theta_inv)

    if (log(runif(1)) < log_r) {
      # Accept
      theta <- theta_prop
      if (i > burnin) acc <- acc + 1
    }

    if (i > burnin) {
      samps[i - burnin, ] <- c(beta, theta)
    }
  }
  cat("Acceptance Rate (theta):", acc / iters, "\n")
  return(samps)
}



samples_hetero <- vector("list", 4)
for (s in 1:length(samples_hetero)) {
  set.seed(637 + s)
  beta.start <- MASS::mvrnorm(
    1,
    mu = rep(0, ncol(X)),
    Sigma = 100 * diag(ncol(X))
  )
  theta.start <- MASS::mvrnorm(
    1,
    mu = rep(0, 2),
    Sigma = matrix(c(1, -0.01, -0.01, 0.001), byrow = T, nrow = 2)
  )

  samples_hetero[[s]] <- lm_bayes_hetero(
    y,
    X,
    W,
    burnin = 3000,
    inits = list(beta = beta.start, theta = theta.start)
  )
}

samples_hetero_diag <- run_diagnostics(samples_hetero)

coda::traceplot(samples_hetero_diag$Samples)
samples_hetero_diag$Gelman
samples_hetero_diag$Raftery
samples_hetero_diag$EffectiveSize

allsamps_hetero <- as.matrix(samples_hetero_diag$Samples)
cov(allsamps_hetero[, 8:9])

##### MODEL COMPARISON #####

dim(allsamps)
dim(allsamps_hetero)

dim(X)

loglik_mat <- matrix(nrow = nrow(allsamps), ncol = nrow(X))
loglik_mat_hetero <- matrix(nrow = nrow(allsamps_hetero), ncol = nrow(X))

for (s in 1:nrow(loglik_mat)) {
  params <- allsamps[s, ]
  loglik_mat[s, ] <- dnorm(
    y,
    mean = X %*% params[1:7],
    sd = sqrt(params[8]),
    log = T
  )

  params_hetero <- allsamps_hetero[s, ]
  loglik_mat_hetero[s, ] <- dnorm(
    y,
    mean = X %*% params[1:7],
    sd = sqrt(exp(W %*% params_hetero[8:9])),
    log = T
  )
}

#Do we need a heteroskedastic model?
library(loo)

loo::waic(loglik_mat)
#p_waic is 7.9 ---> Pretty good!
loo::waic(loglik_mat_hetero)
#Pretty good too

# > loo_compare(loo(loglik.mat), loo(loglik.mat.hetero))
#        elpd_diff se_diff
# model2  0.0       0.0
# model1 -2.4       3.9

loo_compare(loo(loglik_mat), loo(loglik_mat_hetero))