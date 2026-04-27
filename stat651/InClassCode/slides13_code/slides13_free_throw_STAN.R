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

library("rstan")

data <- list(y = y, n = n, N = N)

params <- c("M", "mu", "theta", "theta_new", "y_new", "y_4_new")

n_cores <- parallel::detectCores()   # Count how many cores are available
options(mc.cores = n_cores)          

fit_stan <- stan(model_code = readLines("slides13_free_throws.stan"),
                 data = data, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_cores)

summary(fit_stan)
sim_stan <- extract(fit_stan)

rstan::traceplot(fit_stan, pars = params)

prop.table(table(sim_stan$y_new))
prop.table(table(sim_stan$y_4_new))

plot(fit_stan, pars = "theta")
plot(fit_stan, pars = "theta_new")
