library("rstan")
library("posterior")
library("bayesplot")
library("coda")
library("goftest")

rm(list = ls())

###### Poisson process example

### data

dat <- read.table("../../data/computer-failures-process.dat", head = TRUE)

### original model
## y ~ poisson(theta);
## theta ~ gamma(6.0, 0.5);

### STAN

n <- nrow(dat)
dat$month <- 1:n

data <- list(n = nrow(dat),
             y = dat$x)

params <- c("theta", 
            "y_pred", "y_llik", "y_upc", 
            "theta_upc")

# n_cores <- parallel::detectCores()   # Count how many cores are available
# n_chains <- n_cores
n_chains <- 5
options(mc.cores = n_chains)

fit_stan <- stan(model_code = readLines("gof_Pois.stan"),
                 data = data, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_chains)

sim_stan <- extract(fit_stan)

rstan::summary(fit_stan, pars = c("theta"))
str(sim_stan)

rstan::traceplot(fit_stan, pars = c("theta", "lp__"))

plot(fit_stan, pars = "y_pred")

y_hat <- colMeans(sim_stan$y_pred)
plot(y_hat, data$y); abline(0, 1)

plot(y_hat, data$y - y_hat); abline(h = 0)

hist(sim_stan$y_pred[,1] - data$y[1])
summary(sim_stan$y_pred[,1] - data$y[1])

indx_upc <- 2000
hist(sim_stan$y_upc[indx_upc,])

plot((1:data$n)/data$n, sort(sim_stan$y_upc[indx_upc,]), xlim = c(0,1), ylim = c(0,1))
abline(0, 1, lty = 2)

ks.test(sim_stan$y_upc[indx_upc,], punif) # Kolmogorov-Smirnov test
goftest::ad.test(sim_stan$y_upc[indx_upc,], null = "punif") # Anderson-Darling test

sim_stan$theta_upc[indx_upc]
hist(sim_stan$theta_upc)

sim_stan$y_llik

library("loo")
loo::loo(sim_stan$y_llik)
loo::waic(sim_stan$y_llik)


### process model
## vector[n] mu = rate * days;
## y ~ poisson(mu);
## rate ~ gamma(1.5, 2.0);

data <- list(n = nrow(dat),
             y = dat$x, 
             days = dat$t,
             month = dat$month)

params <- c("rate", 
            "y_pred", "y_llik", "y_upc", 
            "rate_upc")

# n_cores <- parallel::detectCores()   # Count how many cores are available
# n_chains <- n_cores
n_chains <- 5
options(mc.cores = n_chains)

fit_stan <- stan(model_code = readLines("gof_Pois_proc.stan"),
                 data = data, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_chains)

sim_stan <- extract(fit_stan)

rstan::summary(fit_stan, pars = c("rate"))
str(sim_stan)

rstan::traceplot(fit_stan, pars = c("rate", "lp__"))

plot(fit_stan, pars = "y_pred")

y_hat <- colMeans(sim_stan$y_pred)
plot(y_hat, data$y); abline(0, 1)

plot(y_hat, data$y - y_hat); abline(h = 0)

hist(sim_stan$y_pred[,1] - data$y[1])
summary(sim_stan$y_pred[,1] - data$y[1])

indx_upc <- 2000
hist(sim_stan$y_upc[indx_upc,])

plot((1:data$n)/data$n, sort(sim_stan$y_upc[indx_upc,]), xlim = c(0,1), ylim = c(0,1))
abline(0, 1, lty = 2)

ks.test(sim_stan$y_upc[indx_upc,], punif)
goftest::ad.test(sim_stan$y_upc[indx_upc,], null = "punif")

sim_stan$rate_upc[indx_upc]
hist(sim_stan$rate_upc)

sim_stan$y_llik

library("loo")
loo::loo(sim_stan$y_llik)
loo::waic(sim_stan$y_llik)



### trend model
## vector[n] rate = exp(alpha + beta*month);
## vector[n] mu = rate .* days;
## y ~ poisson(mu);
## alpha ~ normal(-1.0, 5.0);
## beta ~ normal(0.0, 5.0);

data <- list(n = nrow(dat),
             y = dat$x, 
             days = dat$t,
             month = dat$month)

plot(dat$month, dat$x/dat$t)

params <- c("alpha", "beta", 
            "y_pred", "y_llik", "y_upc", 
            "alpha_upc", "beta_upc")

# n_cores <- parallel::detectCores()   # Count how many cores are available
# n_chains <- n_cores
n_chains <- 5
options(mc.cores = n_chains)

fit_stan <- stan(model_code = readLines("gof_Pois_proc_trend.stan"),
                 data = data, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_chains)

sim_stan <- extract(fit_stan)

rstan::summary(fit_stan, pars = c("alpha", "beta"))
str(sim_stan)

rstan::traceplot(fit_stan, pars = c("alpha", "beta", "lp__"))

plot(fit_stan, pars = "y_pred")

y_hat <- colMeans(sim_stan$y_pred)
plot(y_hat, data$y); abline(0, 1)

plot(y_hat, data$y - y_hat); abline(h = 0)

hist(sim_stan$y_pred[,1] - data$y[1])
summary(sim_stan$y_pred[,1] - data$y[1])

indx_upc <- 2000
hist(sim_stan$y_upc[indx_upc,])

plot((1:data$n)/data$n, sort(sim_stan$y_upc[indx_upc,]), xlim = c(0,1), ylim = c(0,1))
abline(0, 1, lty = 2)

ks.test(sim_stan$y_upc[indx_upc,], punif)
goftest::ad.test(sim_stan$y_upc[indx_upc,], null = "punif")

sim_stan$alpha_upc[indx_upc]
sim_stan$beta_upc[indx_upc]
hist(sim_stan$alpha_upc)

sim_stan$y_llik

library("loo")
loo::loo(sim_stan$y_llik)
loo::waic(sim_stan$y_llik)



### hierarchical model
## vector[n] mu = rates .* days;
## real rate_a = rate_mean^2 / rate_var;
## real rate_b = rate_mean / rate_var;
## 
## y ~ poisson(mu);
## rates ~ gamma(rate_a, rate_b);
## 
## rate_mean ~ gamma(1.5, 2.0);
## rate_var ~ gamma(1.0, 2.0);

data <- list(n = nrow(dat),
             y = dat$x, 
             days = dat$t,
             month = dat$month)

plot(dat$month, dat$x/dat$t)

params <- c("rates", "rate_mean", "rate_var", 
            "y_pred", "y_llik", "y_upc", 
            "rates_upc", "rate_mean_upc", "rate_var_upc")

# n_cores <- parallel::detectCores()   # Count how many cores are available
# n_chains <- n_cores
n_chains <- 5
options(mc.cores = n_chains)

fit_stan <- stan(model_code = readLines("gof_Pois_proc_hier.stan"),
                 data = data, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_chains)

sim_stan <- extract(fit_stan)

rstan::summary(fit_stan, pars = c("rates", "rate_mean", "rate_var"))
str(sim_stan)

rstan::traceplot(fit_stan, pars = c("rates", "rate_mean", "rate_var", "lp__"))

plot(fit_stan, pars = "y_pred")

plot(dat$month, dat$x/dat$t)
lines(colMeans(sim_stan$rates), col = "red", pch = 19)

y_hat <- colMeans(sim_stan$y_pred)
plot(y_hat, data$y); abline(0, 1)

plot(y_hat, data$y - y_hat); abline(h = 0)

hist(sim_stan$y_pred[,1] - data$y[1])
summary(sim_stan$y_pred[,1] - data$y[1])

indx_upc <- 2000
hist(sim_stan$y_upc[indx_upc,])

plot((1:data$n)/data$n, sort(sim_stan$y_upc[indx_upc,]), xlim = c(0,1), ylim = c(0,1))
abline(0, 1, lty = 2)
ks.test(sim_stan$y_upc[indx_upc,], punif)
goftest::ad.test(sim_stan$y_upc[indx_upc,], null = "punif")

plot((1:data$n)/data$n, sort(sim_stan$rates_upc[indx_upc,]), xlim = c(0,1), ylim = c(0,1))
abline(0, 1, lty = 2)
ks.test(sim_stan$rates_upc[indx_upc,], punif)
goftest::ad.test(sim_stan$rates_upc[indx_upc,], null = "punif")


sim_stan$rate_mean_upc[indx_upc]
sim_stan$rate_var_upc[indx_upc]

sim_stan$y_llik

library("loo")
loo::loo(sim_stan$y_llik)
loo::waic(sim_stan$y_llik)

