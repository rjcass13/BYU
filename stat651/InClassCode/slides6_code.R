dt <- data.frame(dose = c(-.86, -.3, -.05, .73), 
                n = c(5, 5, 5, 5),
                y = c(0, 1, 3, 5))

plot(dt$dose, dt$y/dt$n)

# Guesses for our priors
m_a <- 0
v_a <- 3
m_b <- 1
v_b <- 1

n_sim <- 1000
alphas <- rnorm(n_sim, m_a, sqrt(v_a))
betas <- rnorm(n_sim, m_b, sqrt(v_b))

xx <- seq(-1, 1, length = 200)

curves <- matrix(NA, nrow = n_sim, ncol = length(xx))

for (i in 1:n_sim){
  num <- exp(alphas[i]+betas[i]*xx)
  curves[i, ] <- num / (1+ num)
}

# Normally don;t plot with data, just pick prior values that appear to give the shape you expect
plot(dt$dose, dt$y/dt$n)
for (i in 1:n_sim){
  lines(xx, curves[i, ], lty = 2, col = i)
}

######### Gibbs
llik <- function(y, n_vec, x, alpha, beta){
  abx <- alpha + beta*x
  sum(y*(abx)) - sum(n_vec*log(1 + exp(abx)))
}

# Test it
llik(y = dt$y, n_vec = dt$n, x = dt$dose, alpha = 0, beta = 1)

logfc_alpha <- function(alpha, beta, y, n_vec, x, m_a, v_a) {
  llik(y, n_vec, x, alpha, beta) + dnorm(alpha, mean = m_a, sd = sqrt(v_a), log = TRUE)
}

logfc_beta <- function(beta, alpha, y, n_vec, x, m_b, v_b) {
  llik(y, n_vec, x, alpha, beta) + dnorm(beta, mean = m_b, sd = sqrt(v_b), log = TRUE)
}

# Initialize things
n_alpha <- 200
alpha_seq <- seq(-3, 3, length = n_alpha)
n_beta <- 500
beta_seq <- seq(-2, 30, length = n_beta)

state <- list(alpha = 0, beta = 1.5)
n_iter <- 1000
sims <- as.data.frame(matrix(NA, nrow = n_iter, ncol = length(state)))
colnames(sims) <- names(state)

pb <- txtProgressBar(min = 1, max = n_iter, initial = 1, style = 3)
for (i in 1:n_iter) {
  ## Update Alpha
  lp_alpha <- sapply(alpha_seq, \(a) logfc_alpha(alpha = a, beta = state$beta, y = dt$y, n_vec = dt$n, x= dt$dose, m_a, v_a))
  state$alpha <- sample(alpha_seq, 1, prob = exp(lp_alpha - max(lp_alpha))) # It auto-standardizes the probabilities. Max(lp_alpha) is to prevent overflow

  ## Update Beta
  lp_beta <- sapply(beta_seq, \(b) logfc_beta(beta = b, alpha = state$alpha, y = dt$y, n_vec = dt$n, x= dt$dose, m_b, v_b))
  state$beta <- sample(beta_seq, 1, prob = exp(lp_beta - max(lp_beta))) # It auto-standardizes the probabilities. Max(lp_alpha) is to prevent overflow

  sims[i, 'alpha'] <- state$alpha
  sims[i, 'beta'] <- state$beta

  setTxtProgressBar(pb, i)
}

plot(sims[, 'alpha'], sims[, 'beta'])


n_curve <- 50
curves_post <- matrix(NA, nrow = n_curve, ncol = length(xx))

iter_keep <- floor(seq(1, n_iter, length = n_curve))

for(i in 1:n_curve) {
  num <- exp(sims[iter_keep[i], 'alpha'] + sims[iter_keep[i], 'beta']*xx)
  curves_post[i, ] <- num/(1 + num)
}

plot(dt$dose, dt$y/dt$n)

for (i in i:n_curve) {
  lines(xx, curves_post[i, ], lty = 2, col = i)
}

#### STAN MODEL

library("rstan")

data_stan <- list(N = length(dt$y), y = dt$y, x = dt$dose, n_vec = dt$n)

params <- c("alpha", "beta")

n_cores <- parallel::detectCores()   # Count how many cores are available
options(mc.cores = n_cores)          

fit_stan <- stan(model_code = readLines("slides6_bioassay.stan"),
                 data = data_stan, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_cores)

summary(fit_stan)

sim_stan <- extract(fit_stan) # collect samples from all chains

plot(sim_stan$alpha, sim_stan$beta)

# Highest Posterior Density
index_hpd <- which(sim_stan$lp__ >= quantile(sim_stan$lp__, .05))

points(sim_stan$alpha[index_hpd], sim_stan$beta[index_hpd], col = 'red', pch = 19)

ld50 <- -sim_stan$alpha/sim_stan$beta
hist(ld50, breaks = 30)

mean(sim_stan$beta > 0)
