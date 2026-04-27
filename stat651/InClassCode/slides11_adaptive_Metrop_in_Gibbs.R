library('mvtnorm')
library('coda')
library('posterior')


data <- data.frame(dose = c(-.86, -.3, -.05, .73),
                   n = c(5, 5, 5, 5), 
                   y = c(0, 1, 3, 5))

plot(data$dose, data$y / data$n)

## model: y[i] ~ Binom(n[i], inv-logit(alpha + beta*dose[i]))

## components of prior
prior <- list(
  m_a = 0.0, # alpha ~ Normal(m_a, v_a)
  v_a = 3.0,
  m_b = 1.0, # beta ~ Normal(m_b, v_b)
  v_b = 1.0
)

## log likelihood
llik <- function(y, n_vec, x, alpha, beta) {
  abx <- alpha + beta*x
  sum(y*(abx)) - sum(n_vec*log(1.0 + exp(abx)))
}

llik(y = data$y, n_vec = data$n, x = data$dose, alpha = 0.0, beta = 1.0)

## log of joint posterior density (unnormalized)
logjoint <- function(par, data, prior) {
  llik(y = data$y, n_vec = data$n, x = data$dose, alpha = par[1], beta = par[2]) + 
    dnorm(par[1], mean = prior$m_a, sd = sqrt(prior$v_a), log = TRUE) + 
    dnorm(par[2], mean = prior$m_b, sd = sqrt(prior$v_b), log = TRUE)
}

## log full conditional for alpha (unnormalized)
logfc_alpha <- function(alpha, state, data, prior) {
  llik(y = data$y, n_vec = data$n, x = data$dose, alpha = alpha, beta = state$beta) + 
    dnorm(alpha, mean = prior$m_a, sd = sqrt(prior$v_a), log = TRUE)
}

## log full conditional for beta (unnormalized)
logfc_beta <- function(beta, state, data, prior) {
  llik(y = data$y, n_vec = data$n, x = data$dose, alpha = state$alpha, beta = beta) + 
    dnorm(beta, mean = prior$m_b, sd = sqrt(prior$v_b), log = TRUE)
}

## to modify...
rw_mvnorm_MetHast <- function(logtarget, par_old, cand_Sig, data, prior) {
  
  ## draw candidate
  par_cand <- rmvnorm(1, mean = par_old, sigma = cand_Sig)
  
  ## compute acceptance prob
  lalpha <- logtarget(par_cand, data = data, prior = prior) - 
    logtarget(par_old, data = data, prior = prior) + 
    dmvnorm(par_old, mean = par_cand, sigma = cand_Sig, log = TRUE) - 
    dmvnorm(par_cand, mean = par_old, sigma = cand_Sig, log = TRUE)
  
  ## decision
  if (log(runif(1)) < lalpha) { # simulate event with probability min(1, alpha)
    par_out <- par_cand
    accept <- TRUE
  } else {
    par_out <- par_old
    accept <- FALSE
  }
  
  list(par = par_out, accept = accept)
}

n_iter = 1000
state <- list(alpha = 1, beta = 1)
sims <- matrix(NA, ncol = length(state), nrow = n_iter) 
colnames(sims) <- names(state)
accepts <- numeric(n_iter)
cand_Sig <- diag(1, 2)

for (i in 1:n_iter) {
  tmp <- rw_mvnorm_MetHast(logtarget = logjoint, par_old = unlist(state), cand_Sig = cand_Sig, data = data, prior = prior)
  state$alpha <- tmp$par[1]
  state$beta <- tmp$par[2]

  accepts[i] <- tmp$accept

  sims[i, "alpha"] <- state$alpha
  sims[i, "beta"] <- state$beta
}


plot(as.mcmc(sims))

mean(accepts)

plot(sims[, 1], sims[, 2])
cov(sims)
cand_Sig <- 5 * cov(sims)

ess_mean(sims[, 1])
ess_mean(sims[, 2])
