
## bioassay example

data <- data.frame(logdose = c(-0.86, -0.3, -0.05, 0.73),
                   n_vec = c(5, 5, 5, 5), 
                   y = c(0, 1, 3, 5))

plot(data$logdose, data$y / data$n)

## prior hyperparameters
prior <- list(
  m_a = 0.0, # alpha ~ Normal(m_a, v_a)
  v_a = 3.0,
  m_b = 1.0, # beta ~ Normal(m_b, v_b)
  v_b = 1.0
)

llik <- function(y, n_vec, x, alpha, beta) {
  abx <- alpha + beta*x
  sum(y*(abx)) - sum(n_vec*log(1.0 + exp(abx)))
}

log_post_unnormalized <- function(par, data, prior) {
  
  log_lik <- llik(y = data$y, n_vec = data$n_vec, x = data$logdose, 
                  alpha = par[1], beta = par[2])
  
  log_prior_alpha <- dnorm(par[1], 
                           mean = prior$m_a, sd = sqrt(prior$v_a), 
                           log = TRUE)
  
  log_prior_beta <- dnorm(par[2], 
                           mean = prior$m_b, sd = sqrt(prior$v_b), 
                           log = TRUE)
  
  log_lik + log_prior_alpha + log_prior_beta
}

log_post_unnormalized(par = c(1.0, 1.0), data = data, prior = prior)

res <- optim(c(0.0, 1.0), \(par) -log_post_unnormalized(par, data = data, prior = prior), hessian = TRUE)

appx_mean <- res$par
appx_sigma <- solve(res$hessian)

library("MASS")
sim <- mvrnorm(n = 10e3, mu = appx_mean, Sigma = appx_sigma)
dens <- kde2d(x = sim[, 1], y = sim[, 2])
contour(dens$x, dens$y, dens$z, col = "red", lwd = 2, xlab = "alpha", ylab = "beta")



### Draws from posterior using STAN (for comparison)

library("rstan")

data_stan <- list(N = length(data$y), y = data$y, x = data$logdose, n_vec = data$n)

params <- c("alpha", "beta") # parameters for which we want to extract samples

# n_cores <- parallel::detectCores()   # Count how many cores are available
n_cores <- 3
options(mc.cores = n_cores) # determine number of chains to run in parallel

fit_stan <- stan(model_code = readLines("slides6_bioassay.stan"),
                 data = data_stan, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_cores)

summary(fit_stan)

sim_stan <- extract(fit_stan) # collect samples from all chains

plot(sim_stan$alpha, sim_stan$beta) # draws from the joint posterior
contour(dens$x, dens$y, dens$z, col = "red", lwd = 2, xlab = "alpha", ylab = "beta", add = TRUE)
