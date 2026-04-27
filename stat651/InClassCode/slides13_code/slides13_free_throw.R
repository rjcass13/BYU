### free-throw data

y <- c(15, 4, 3, 4, 2, 10, 5, 10, 2, 0) # free-throws made
n <- c(20, 7, 6, 4, 4, 12, 6, 12, 2, 1) # attempts
(N <- length(y)) # number of players
y/n # MLEs of theta

## model:
# y_i ~ Binomial(n_i, theta_i)
# theta_i ~ Beta(alpha, beta)
# alpha = M * mu
# beta = M * (1 - mu)
## mu is interpretable as the population mean for thetas
## M is a precision parameter: Variance(theta) = mu * (1 - mu) / (M + 1); controls how much the thetas vary around mu

### prior (try playing with this and check sensitivity of inferences to prior choice)

sh_M <- 6 # M ~ Gamma(sh_M, scale = sc_M)
sc_M <- 2

sh1_mu <- 7 # mu ~ Beta(sh1_mu, sh2_mu)
sh2_mu <- 3

## prior-predictive simulation
nsim <- 10e3
psim_M <- rgamma(nsim, shape = sh_M, scale = sc_M)
psim_mu <- rbeta(nsim, shape1 = sh1_mu, shape2 = sh2_mu)
psim_theta <- rbeta(nsim, shape1 = psim_M * psim_mu, shape2 = psim_M * (1.0 - psim_mu))

hist(psim_theta) # marginal prior for thetas


## functions for Gibbs sampler

## full conditionals for theta are all known Beta distributions
upd_theta <- function(y, n, M, mu) {
  J <- length(y)
  alpha <- M * mu
  beta <- M * (1.0 - mu)
  rbeta(J, shape1 = alpha + y, shape2 = beta + n - y)
}

## using a random-walk Metropolis sampler for M (sampling log(M) which doesn't have a boundary at 0)
upd_M <- function(M_old, theta, mu, sh_M, sc_M, cand_sig) {
  
  logtarget <- function(lM) {
    M <- exp(lM)
    alpha <- M * mu
    beta <- M * (1.0 - mu)
    sum(dbeta(theta, alpha, beta, log = TRUE)) + # log "likelihood"
      dgamma(M, shape = sh_M, scale = sc_M, log = TRUE) + # log prior
      lM # log Jacobian
  }
  
  lM_old <- log(M_old)
  
  lM_cand <- rnorm(1, mean = lM_old, sd = cand_sig) # draw candidate on log space
  
  log_accpt_ratio <- logtarget(lM_cand) - logtarget(lM_old)
  log_uniform <- log(runif(1, min = 0.0, max = 1.0))
  
  if (log_uniform < log_accpt_ratio) {
    M_out <- exp(lM_cand)
    accpt <- 1
  } else {
    M_out <- M_old
    accpt <- 0
  }
  
  list(M = M_out, accpt = accpt)
}

## using a slice sampler with Neal's (2003) stepping-out-and-shrinking algorithm 
library("qslice")

upd_mu <- function(mu_old, theta, M, sh1_mu, sh2_mu, w = 0.2) {
  
  logtarget <- function(mu) {
    if (mu < 0.0 || mu > 1.0) {
      out <- -Inf
    } else {
      alpha <- M * mu
      beta <- M * (1.0 - mu)
      out <- sum(dbeta(theta, alpha, beta, log = TRUE)) + # log "likelihood"
        dbeta(mu, shape1 = sh1_mu, shape2 = sh2_mu, log = TRUE) # log prior
    }
    out
  }
  
  qslice::slice_stepping_out(x = mu_old, log_target = logtarget, w = w)
}

## Set up MCMC

n_iter <- 1000
state <- list(theta = rbeta(N, 5, 5), # initialize
              M = 3.0, mu = 0.5)
sims <- matrix(NA, ncol = N + 2, nrow = n_iter)
colnames(sims) <- c(paste0("theta", 1:N), "M", "mu")
head(sims)

## Run Gibbs sampler
accpt_tot <- 0
for (i in 1:n_iter) {
  
  state$theta <- upd_theta(y = y, n = n, M = state$M, mu = state$mu)
  
  tmp <- upd_M(M_old = state$M, theta = state$theta, mu = state$mu, 
               sh_M = sh_M, sc_M = sc_M, cand_sig = 1.0)

  accpt_tot <- accpt_tot + tmp$accpt
  state$M <- tmp$M
  
  tmp <- upd_mu(mu_old = state$mu, theta = state$theta, M = state$M, 
                sh1_mu = sh1_mu, sh2_mu = sh2_mu)
  state$mu <- tmp$x
  
  sims[i, 1:N] <- state$theta
  sims[i, "M"] <- state$M
  sims[i, "mu"] <- state$mu
}

library("coda")

## don't forget to do diagnostics and discard burn-in samples!

plot(as.mcmc(sims[,"M"]))
accpt_tot / n_iter

plot(as.mcmc(sims[,"mu"]))

hist(sims[,"mu"], freq = FALSE, xlim = c(0,1))
curve(dbeta(x, sh1_mu, sh2_mu), col = "red", add = TRUE) # compare prior to posterior for mu

plot(as.mcmc(sims[,1:N])) # will show 10 plots, a few at a time

hist(sims[,"theta4"], freq = FALSE, xlim = c(0,1)) # posterior of thete_4 for fourth player who made 4/4 free throws
hist(psim_theta, freq = FALSE, xlim = c(0,1)) # compare with prior

## posterior predictive distribution for Player 4's next five free throws
sims_ypred4 <- rbinom(n_iter, size = 5, prob = sims[,"theta4"])
plot(table(sims_ypred4) |> prop.table())

## posterior predictive distribution of theta for a new player
sims_alpha <- sims[,"M"] * sims[,"mu"]
sims_beta <- sims[,"M"] * (1.0 - sims[,"mu"])
sims_theta_new <- rbeta(n_iter, shape1 = sims_alpha, shape2 = sims_beta)

hist(sims_theta_new, freq = FALSE, xlim = c(0,1))
hist(psim_theta, freq = FALSE, xlim = c(0,1))

## posterior predictive distribution for new player's next five free throws
sims_ypred_new <- rbinom(n_iter, size = 5, prob = sims_theta_new)
plot(table(sims_ypred_new) |> prop.table())
