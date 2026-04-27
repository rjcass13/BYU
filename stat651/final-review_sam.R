###########################################################################
install.packages("gapminder")
library(gapminder)

X <- model.matrix(
  ~ -1 +
    log(gdpPercap) +
    I(year - min(year)) +
    continent,
  data = gapminder
)

y <- as.matrix(gapminder$lifeExp)

summary(lm(y ~ -1 + X))

run.diagnostics <- function(samples.list) {
  mcmc.obj <- coda::mcmc.list(
    lapply(samples.list, coda::mcmc)
  )

  raftery.results <- lapply(mcmc.obj, coda::raftery.diag)

  names(raftery.results) <- paste0("Chain_", seq_along(raftery.results))

  list(
    Samples = mcmc.obj,
    Gelman = coda::gelman.diag(mcmc.obj),
    Raftery = raftery.results,
    EffectiveSize = coda::effectiveSize(mcmc.obj)
  )
}

lm.bayes <- function(
  y,
  X,
  iters = 5000,
  burnin = 100,
  seed = 651,
  inits = list(
    beta = rep(0, ncol(X)),
    gamma = 1 / 10
  ),
  consts = list(
    #Prior params for beta
    Sigma0.inv = solve(100000 * diag(ncol(X))),
    beta.0 = rep(0, ncol(X)),
    #Prior params for sigma^2
    nu.0 = 0.01,
    sigma2.0 = 1
  )
) {
  n <- nrow(X)
  p <- ncol(X)

  samps <- matrix(nrow = iters, ncol = p + 1)

  gamma <- inits$gamma
  beta <- inits$beta

  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  Xty.prior <- crossprod(consts$Sigma0.inv) %*% consts$beta.0

  shape.prior <- (consts$nu.0 + n) / 2
  rate.prior <- consts$nu.0 * consts$sigma2.0

  for (i in 1:(iters + burnin)) {
    #Update beta | gamma, data
    A = gamma * XtX + consts$Sigma0.inv
    A.inv <- solve(A)
    B = gamma * Xty + Xty.prior
    beta <- MASS::mvrnorm(1, mu = A.inv %*% B, A.inv)

    #Update gamma | beta, data
    SSR <- crossprod(y - X %*% beta)
    gamma <- rgamma(1, shape = shape.prior, rate = 0.5 * (SSR + rate.prior))

    if (i > burnin) {
      samps[i - burnin, ] <- c(beta, 1 / gamma)
    }
  }
  samps
}

samples <- vector("list", 4)

#Now run 4 chains
for (s in 1:length(samples)) {
  set.seed(637 + s)
  beta.start <- MASS::mvrnorm(n = 1, mu = rep(0, 7), Sigma = 10000 * diag(7))
  gamma.start <- rgamma(1, 5, 10)
  samples[[s]] <- lm.bayes(
    y,
    X,
    seed = 651 + s,
    inits = list(
      beta = beta.start,
      gamma = gamma.start
    )
  )
}

samples.diag <- run.diagnostics(samples)

samples.diag$Gelman
samples.diag$Raftery
samples.diag$EffectiveSize

allsamps <- as.matrix(samples.diag$Samples)
coda::traceplot(samples.diag$Samples)

###########################################################################
# Question 2 #
###########################################################################

W <- cbind(1, X[, 2])

log.theta <- function(theta, W, e.hat, theta.0, Sigma.theta.inv) {
  w.theta <- W %*% theta
  log.lik <- -0.5 * sum(w.theta + e.hat^2 / exp(w.theta))
  diff <- theta - theta.0
  log.prior <- -0.5 * as.numeric(t(diff) %*% Sigma.theta.inv %*% diff)
  log.lik + log.prior
}

lm.bayes.hetero <- function(
  y,
  X,
  W,
  iters = 5000,
  burnin = 500,
  inits = list(beta = rep(0, ncol(X)), theta = c(0, 0)),
  consts = list(
    Sigma0.inv = solve(100000 * diag(ncol(X))),
    beta.0 = rep(0, ncol(X)),
    theta.0 = c(0, 0),
    Sigma.theta.inv = solve(100000 * diag(2)),
    V = 1 /
      100 *
      matrix(c(0.46, -0.012, -0.012, 0.00045), nrow = 2, byrow = TRUE)
  )
) {
  n <- nrow(X)
  p <- ncol(X)
  samps <- matrix(nrow = iters, ncol = p + 2)

  theta <- inits$theta
  beta <- inits$beta

  beta.prior.part <- consts$Sigma0.inv %*% consts$beta.0

  acc <- 0
  for (i in 1:(iters + burnin)) {
    # 1. Update beta | theta, data (Efficient weighted least squares)
    prec.w <- 1 / exp(W %*% theta)
    A <- t(X * as.vector(prec.w)) %*% X + consts$Sigma0.inv
    A.inv <- solve(A)
    B <- t(X * as.vector(prec.w)) %*% y + beta.prior.part

    beta <- MASS::mvrnorm(1, mu = A.inv %*% B, Sigma = A.inv)

    # 2. Update theta | beta, data (Metropolis-Hastings Step)
    e.hat <- y - X %*% beta

    theta.prop <- MASS::mvrnorm(
      n = 1,
      mu = theta,
      Sigma = consts$V
    )

    log.r <- log.theta(
      theta.prop,
      W,
      e.hat,
      consts$theta.0,
      consts$Sigma.theta.inv
    ) -
      log.theta(theta, W, e.hat, consts$theta.0, consts$Sigma.theta.inv)

    if (log(runif(1)) < log.r) {
      theta <- theta.prop
      if (i > burnin) acc <- acc + 1
    }

    if (i > burnin) {
      samps[i - burnin, ] <- c(beta, theta)
    }
  }

  cat("Acceptance Rate (theta):", acc / iters, "\n")
  return(samps)
}

samples.hetero <- vector("list", 4)
for (s in 1:length(samples.hetero)) {
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

  samples.hetero[[s]] <- lm.bayes.hetero(
    y,
    X,
    W,
    burnin = 3000,
    inits = list(beta = beta.start, theta = theta.start)
  )
}

samples.hetero.diag <- run.diagnostics(samples.hetero)

coda::traceplot(samples.hetero.diag$Samples)
samples.hetero.diag$Gelman
samples.hetero.diag$Raftery
samples.hetero.diag$EffectiveSize

allsamps.hetero <- as.matrix(samples.hetero.diag$Samples)
cov(allsamps.hetero[, 8:9])

##### MODEL COMPARISON #####

dim(allsamps)
dim(allsamps.hetero)

dim(X)

loglik.mat <- matrix(nrow = nrow(allsamps), ncol = nrow(X))
loglik.mat.hetero <- matrix(nrow = nrow(allsamps.hetero), ncol = nrow(X))

for (s in 1:nrow(loglik.mat)) {
  params <- allsamps[s, ]
  loglik.mat[s, ] <- dnorm(
    y,
    mean = X %*% params[1:7],
    sd = sqrt(params[8]),
    log = T
  )

  params.hetero <- allsamps.hetero[s, ]
  loglik.mat.hetero[s, ] <- dnorm(
    y,
    mean = X %*% params[1:7],
    sd = sqrt(exp(W %*% params.hetero[8:9])),
    log = T
  )
}

#Do we need a heteroskedastic model?
library(loo)

loo::waic(loglik.mat)
#p_waic is 7.9 ---> Pretty good!
loo::waic(loglik.mat.hetero)
#Pretty good too

# > loo_compare(loo(loglik.mat), loo(loglik.mat.hetero))
#        elpd_diff se_diff
# model2  0.0       0.0
# model1 -2.4       3.9

loo_compare(loo(loglik.mat), loo(loglik.mat.hetero))

###################################################################
# PPL # Doesn't Work as well
###################################################################

library(nimble)

hetero_code <- nimbleCode({
  for (i in 1:n) {
    mu[i] <- (X[i, 1:p_x] %*% beta[1:p_x])[1, 1]

    log_sigma2[i] <- (W[i, 1:p_w] %*% theta[1:p_w])[1, 1]

    y[i] ~ dnorm(mu[i], sd = sqrt(exp(log_sigma2[i])))
  }

  # --- Priors ---
  beta[1:p_x] ~ dmnorm(beta_mu0[1:p_x], prec = beta_prec0[1:p_x, 1:p_x])
  theta[1:p_w] ~ dmnorm(theta_mu0[1:p_w], prec = theta_prec0[1:p_w, 1:p_w])
})

hetero_constants <- list(
  n = nrow(X),
  p_x = ncol(X),
  p_w = ncol(W),
  X = X,
  W = W,
  beta_mu0 = rep(0, ncol(X)),
  beta_prec0 = diag(1e-5, ncol(X)),
  theta_mu0 = rep(0, ncol(W)),
  theta_prec0 = diag(0.1, ncol(W))
)


hetero_data <- list(y = as.vector(y))

hetero_inits <- list(
  beta = rep(0, ncol(X)),
  theta = rep(0, ncol(W))
)

nimble_mod <- nimbleModel(
  code = hetero_code,
  constants = hetero_constants,
  data = hetero_data,
  inits = hetero_inits
)

conf <- configureMCMC(nimble_mod, monitors = c("beta", "theta"))
mcmc_build <- buildMCMC(conf)
compiled_mod <- compileNimble(nimble_mod)
compiled_mcmc <- compileNimble(mcmc_build, project = nimble_mod)

samples_nimble <- runMCMC(
  compiled_mcmc,
  niter = 10000,
  nburnin = 2000,
  nchains = 4,
  samplesAsCodaMCMC = TRUE
)

coda::traceplot(samples_nimble)
