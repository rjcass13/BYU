### data

y <- c(15, 4, 3, 4, 2, 10, 5, 10, 2, 0)
n <- c(20, 7, 6, 4, 4, 12, 6, 12, 2, 1)
(N <- length(y))
y/n


### prior

nsim <- 10e3

# Spread of free throw percentage, maybe .2 for sd
# sigma^2 = mu(1-mu)/(M+1): .2^2 = .7(1-.7)/(M+1)
.7*(1-.7)/(.2^2) - 1 # 4.25
curve(dgamma(x, 2, scale = 2), from = 0, to = 10)
sh_M <- 4
sc_M <- 2



# Average free throw percentage for players
curve(dbeta(x, 7, 3))
sh1_mu <- 35
sh2_mu <- 15

psim_M <- rgamma(nsim, shape = sh_M, scale = sc_M)
psim_mu <- rbeta(nsim, shape1 = sh1_mu, shape2 = sh2_mu)
psim_theta <- rbeta(nsim, shape1 = psim_M * psim_mu, shape2 = psim_M * (1.0 - psim_mu))

hist(psim_theta)

upd_theta <- function(y, n, M, mu) {
  J <- length(y)
  alpha <- M*mu
  beta <- M*(1-mu)
  rbeta(J, shape1 = alpha + y, shape2 = beta + n - y)
}

upd_M <- function(M_old, theta, mu, sh_M, sc_M, cand_sig) {
  log_target <- function(lM) {
    M <- exp(lM)
    alpha <- M * mu
    beta <- M * (1-mu)
    sum(dbeta(theta, alpha, beta, log = TRUE)) + 
      dgamma(M, shape = sh_M, scale = sc_M, log = TRUE) + 
      lM # Jacobian
  }

  lM_old <- log(M_old)

  lM_cand <- rnorm(1, mean = lM_old, sd = cand_sig)

  log_accept_ratio <- log_target(lM_cand) - log_target(lM_old)

  if (log(runif(1)) <= log_accept_ratio) {
    M_out <- exp(lM_cand)
    accept <- 1
  } else {
    M_out <- M_old
    accept <- 0
  }

  list(M = M_out, accept = accept)
}

upd_mu <- function(mu_old, theta, M, sh1_mu, sh2_mu, w = 0.2) {
  log_target <- function(mu) {
    if (mu <= 0 | mu > 1) {
      out <- -Inf
    } else {
      alpha <- M * mu
      beta <- M * (1-mu)
      out <- sum(dbeta(theta, alpha, beta, log = TRUE)) + 
        dbeta(mu, shape1 = sh1_mu, shape2 = sh2_mu, log = TRUE)
    }
    out
  }

  qslice::slice_stepping_out(x = mu_old, log_target = log_target, w = w)
}

n_iter <- 1000
state <- list(theta = rbeta(N, 5, 5), M = 3.0, mu = 0.5)
sims <- matrix(NA, ncol = N + 2, nrow = n_iter)
colnames(sims) <- c(paste0("theta", 1:N), "M", "mu")
accept_total <- 0

for (i in 1:n_iter) {
  state$theta <- upd_theta(y, n, M = state$M, mu = state$mu)
  temp_M <- upd_M(state$M, state$theta, state$mu, sh_M = sh_M, sc_M = sc_M, cand_sig = 1.0)
  accept_total <- accept_total + temp_M$accept
  state$M <- temp_M$M
  temp_mu <- upd_mu(state$mu, state$theta, state$M, sh1_mu = sh1_mu, sh2_mu = sh2_mu)
  state$mu <- temp_mu$x
  
  sims[i, 1:N] <- state$theta
  sims[i, "M"] <- state$M
  sims[i, "mu"] <- state$mu
}

library('coda')
plot(as.mcmc(sims[, "M"]))
accept_total/n_iter

plot(as.mcmc(sims[, "mu"]))

hist(sims[, "mu"], freq = FALSE)
curve(dbeta(x, sh1_mu, sh2_mu), col = 'red', add = TRUE)

plot(as.mcmc(sims[, 1:N]))

hist(sims[, "theta4"])

ypred_4 <- rbinom(n_iter, size = 5, prob = sims[, "theta4"])
table(ypred_4) |> prop.table()


theta_new <- rbeta(n_iter, shape1 = )