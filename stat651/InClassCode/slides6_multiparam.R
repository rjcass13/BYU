data <- data.frame(dose = c(-.86, -.3, -.05, .73),
                   n = c(5, 5, 5, 5), 
                   y = c(0, 1, 3, 5))

plot(data$dose, data$y / data$n)

## prior hyperparameters
m_a <- 0.0 # alpha ~ Normal(m_a, v_a)
v_a <- 3.0
m_b <- 1.0 # beta ~ Normal(m_b, v_b)
v_b <- 1.0

## sample from prior
n_sim <- 50
alphas <- rnorm(n_sim, mean = m_a, sd = sqrt(v_a))
hist(alphas)
betas <- rnorm(n_sim, mean = m_b, sd = sqrt(v_b))
hist(betas)

## samples of logistic curves (prior)
xx <- seq(from = -1.0, to = 1.0, length = 200)
curves <- matrix(NA, nrow = n_sim, ncol = length(xx))

for (i in 1:n_sim) {
  num <- exp(alphas[i] + betas[i]*xx)
  curves[i,] <-  num / (1.0 + num)
}

head(curves)

plot(xx, curves[1,], lty = 2, type = "l",
     xlim = range(xx), ylim = c(0,1))

for (i in 1:n_sim) {
  lines(xx, curves[i,], lty = 2, col = i)  
}

## functions to evaluate components of the posterior
llik <- function(y, n_vec, x, alpha, beta) {
  abx <- alpha + beta*x
  sum(y*(abx)) - sum(n_vec*log(1.0 + exp(abx)))
}

llik(y = data$y, n_vec = data$n, x = data$dose, alpha = 0.0, beta = 1.0)

## full conditional for alpha
logfc_alpha <- function(alpha, beta, y, n_vec, x, m_a, v_a) {
  llik(y, n_vec, x, alpha, beta) + dnorm(alpha, mean = m_a, sd = sqrt(v_a), log = TRUE)
}

## full conditional for beta
logfc_beta <- function(beta, alpha, y, n_vec, x, m_b, v_b) {
  llik(y, n_vec, x, alpha, beta) + dnorm(beta, mean = m_b, sd = sqrt(v_b), log = TRUE)
}

## griddy Gibbs sampler
n_alpha <- 200
alpha_seq <- seq(-3, 3, length = n_alpha)

n_beta <- 200
beta_seq <- seq(-2, 6, length = n_alpha)

state <- list(alpha = 0.0, beta = 1.5) # initialize

n_iter <- 1000
sims <- matrix(NA, nrow = n_iter, ncol = length(state))
colnames(sims) <- names(state)

pb <- txtProgressBar(min = 1, max = n_iter, initial = 1, style = 3)
for (i in 1:n_iter) {
  
  ## update alpha
  lp_alpha <- sapply(alpha_seq, function(a) logfc_alpha(alpha = a, beta = state$beta, y = data$y, 
                                                        n_vec = data$n, x = data$dose, m_a = m_a, v_a = v_a))
  state$alpha <- sample(alpha_seq, size = 1, prob = exp(lp_alpha - max(lp_alpha)))
  
  ## update beta
  lp_beta <- sapply(beta_seq, function(b) logfc_beta(beta = b, alpha = state$alpha, y = data$y, 
                                                        n_vec = data$n, x = data$dose, m_b = m_b, v_b = v_b))
  state$beta <- sample(beta_seq, size = 1, prob = exp(lp_beta - max(lp_beta)))

  ## store the current state
  sims[i, "alpha"] <- state$alpha
  sims[i, "beta"] <- state$beta
  setTxtProgressBar(pb,i)
}

plot(sims[,"alpha"], sims[,"beta"])

## samples of logistic curves (posterior)
n_curve <- 50
curves_post <- matrix(NA, nrow = n_curve, ncol = length(xx))
iter_keep <- floor(seq(1, n_iter, length = n_curve))

for (i in 1:n_curve) {
  num <- exp(sims[iter_keep[i], "alpha"] + sims[iter_keep[i], "beta"]*xx)
  curves_post[i,] <-  num / (1.0 + num)
}

head(curves)

plot(data$dose, data$y / data$n, pch = 20)

for (i in 1:n_curve) {
  lines(xx, curves_post[i,], lty = 2, col = i)  
}



### STAN model

library("rstan")

data_stan <- list(N = length(data$y), y = data$y, x = data$dose, n_vec = data$n)

params <- c("alpha", "beta") # parameters for which we want to extract samples

# n_cores <- parallel::detectCores()   # Count how many cores are available
n_cores <- 3
options(mc.cores = n_cores) # determine number of chains to run in parallel

fit_stan <- stan(model_code = readLines("slides6_bioassay.stan"),
                 data = data_stan, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_cores)

summary(fit_stan)

sim_stan <- extract(fit_stan) # collect samples from all chains

plot(sim_stan$alpha, sim_stan$beta)
