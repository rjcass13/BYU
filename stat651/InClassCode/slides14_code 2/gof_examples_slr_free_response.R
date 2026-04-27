library("rstan")
library("posterior")
library("bayesplot")
library("coda")

rm(list = ls())

###### simple linear regression example

### data

dat <- read.csv("multiple_choice_short_answer2.csv", head = TRUE)
plot(dat$multiple_choice, dat$short_answer)

### Model 1: Normal regression
## alpha ~ normal(0.0, 100.0);
## beta ~ normal(0.0, 100.0);
## sig ~ cauchy(0.0, 20.0);
## y ~ normal(alpha + beta * x, sig);

### STAN

## grid for predictions
n_grid <- 100
x_grid <- seq(1, 100, length = n_grid)

data <- list(n = nrow(dat),
             n_grid = n_grid,
             y = dat$short_answer, 
             x = dat$multiple_choice,
             x_grid = x_grid)

params <- c("alpha", "beta", "sig", 
            "y_pred", "y_grid", "y_llik", "y_upc", 
            "alpha_upc", "beta_upc", "sig_upc")

# n_cores <- parallel::detectCores()   # Count how many cores are available
# n_chains <- n_cores
n_chains <- 5
options(mc.cores = n_chains)

fit_stan <- stan(model_code = readLines("gof_slr_free_response.stan"),
                 data = data, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_chains)

sim_stan <- extract(fit_stan)

rstan::summary(fit_stan, pars = c("alpha", "beta", "sig"))
str(sim_stan)

rstan::traceplot(fit_stan, pars = c("alpha", "beta", "sig"))

plot(fit_stan, pars = c("alpha", "beta", "sig"))
# plot(fit_stan, pars = "y_pred")

## plot prediction intervals
plot(x_grid, colMeans(sim_stan$y_grid), type = "l", col = "red", lwd = 2)
abline(0, 1, lty = 2)
lines(x_grid, apply(sim_stan$y_grid, 2, function(x) quantile(x, 0.05)), lty = 2, col = "red", lwd = 2)
lines(x_grid, apply(sim_stan$y_grid, 2, function(x) quantile(x, 0.95)), lty = 2, col = "red", lwd = 2)
points(data$x, data$y)

## Residuals
y_hat <- colMeans(sim_stan$y_pred)
plot(y_hat, data$y); abline(0, 1)

plot(y_hat, data$y - y_hat); abline(h = 0) # residual plot

hist(sim_stan$y_pred[,1] - data$y[1])
summary(sim_stan$y_pred[,1] - data$y[1])

## Uniform parameterization (UPC) checks
indx_upc <- 2000 # pick just one iteration of MCMC
hist(sim_stan$y_upc[indx_upc,])

plot((1:data$n)/data$n, sort(sim_stan$y_upc[indx_upc,]), xlim = c(0,1), main = "Uniform quantile-quantile")
abline(0, 1, lty = 2)

library("goftest")
ks.test(sim_stan$y_upc[indx_upc,], punif)
goftest::ad.test(sim_stan$y_upc[indx_upc,], null = "punif") # do the upcs for y come from a Uniform distribution?

## Combine UPCs across MCMC iterations
upc_pvals_y <- apply(sim_stan$y_upc, 1, function(x) goftest::ad.test(x, null = "punif")$p)
hist(upc_pvals_y)
pcauchy(mean(tan(pi * (0.5 - upc_pvals_y))), lower.tail = FALSE) # Cauchy combination method

## UPCs for model parameters
sim_stan$alpha_upc[indx_upc]
sim_stan$beta_upc[indx_upc]
sim_stan$sig_upc[indx_upc]

## Quantile residuals
qresid <- numeric(data$n)
for (i in 1:data$n) {
  qresid[i] <- mean(sim_stan$y_pred[,i] <= data$y[i])
}
hist(qresid)
plot((1:data$n)/data$n, sort(qresid)); abline(0, 1, lty = 2)

## Posterior predictive p-value on T(y, theta) = max(y)
hist(apply(sim_stan$y_pred, 1, max)) # each row of y_pred is a new draw from from the posterior predictive distribution, thus creating a new data set
abline(v = max(data$y), col = "red", lwd = 2)
mean(apply(sim_stan$y_pred, 1, max) <= max(data$y))

## Information criteria
str(sim_stan$y_llik)

library("loo")
loo::loo(sim_stan$y_llik)
loo::waic(sim_stan$y_llik)

## Manual calculation of WAIC
(lppd <- log(colMeans(exp(sim_stan$y_llik))) |> sum())
(p_waic <- apply(sim_stan$y_llik, 2, var) |> sum())
-2*lppd + 2*p_waic # WAIC




### Model 2: Beta regressionl
## alpha ~ normal(0.0, 10.0);
## beta ~ normal(0.0, 10.0);
## M ~ cauchy+(20.0, 20.0);
## 
## for (i in 1:n) {
##   real xb = alpha + beta * x[i];
##   real mu = inv_logit(xb);
##   real a = M * mu;
##   real b = M * (1.0 - mu);
##   target += beta_lpdf(yp[i] | a, b) - log100;
## }

params2 <- c("alpha", "beta", "M", 
             "y_pred", "y_grid", "y_llik", "y_upc", 
             "alpha_upc", "beta_upc", "M_upc")

fit2_stan <- stan(model_code = readLines("gof_slrbeta_free_response.stan"),
                  data = data, pars = params2,
                  iter = 5000, warmup = 500, thin = 5, chains = n_chains)

sim2_stan <- extract(fit2_stan)

rstan::summary(fit2_stan, pars = c("alpha", "beta", "M"))
str(sim2_stan)

rstan::traceplot(fit2_stan, pars = c("alpha", "beta", "M"))

plot(fit2_stan, pars = c("alpha", "beta", "M"))
# plot(fit2_stan, pars = "y_pred")

## plot prediction intervals
plot(x_grid, colMeans(sim2_stan$y_grid), type = "l", col = "red", lwd = 2, ylim = c(0, 100))
abline(0, 1, lty = 2)
lines(x_grid, apply(sim2_stan$y_grid, 2, function(x) quantile(x, 0.05)), lty = 2, col = "red", lwd = 2)
lines(x_grid, apply(sim2_stan$y_grid, 2, function(x) quantile(x, 0.95)), lty = 2, col = "red", lwd = 2)
points(data$x, data$y)

## Residuals
y_hat2 <- colMeans(sim2_stan$y_pred)
plot(y_hat2, data$y); abline(0, 1)

plot(y_hat2, data$y - y_hat); abline(h = 0)

## Posterior predictive distribution around specific point
id <- 5
hist(sim2_stan$y_pred[,id]); abline(v = data$y[id])
hist(sim2_stan$y_pred[,id] - data$y[id])
summary(sim2_stan$y_pred[,id] - data$y[id])

### Uniform parameterization checks
## for one iteration of MCMC
indx_upc <- 1000
hist(sim2_stan$y_upc[indx_upc,])
ks.test(sim2_stan$y_upc[indx_upc,], punif) # Kolmogorov-Smirnov test
goftest::ad.test(sim2_stan$y_upc[indx_upc,], null = "punif") # Anderson-Darling test

## across iterations of MCMC
upc2_pvals_y <- apply(sim2_stan$y_upc, 1, function(x) goftest::ad.test(x, null = "punif")$p)
hist(upc2_pvals_y)
pcauchy(mean(tan(pi * (0.5 - upc2_pvals_y))), lower.tail = FALSE) # Cauchy combination method

## UPC for parameters
sim2_stan$alpha_upc[indx_upc]
sim2_stan$beta_upc[indx_upc]
sim2_stan$M_upc[indx_upc]

## Quantile residuals
qresid2 <- numeric(data$n)
for (i in 1:data$n) {
  qresid2[i] <- mean(sim2_stan$y_pred[,i] <= data$y[i])
}
hist(qresid2)
plot((1:data$n)/data$n, sort(qresid2)); abline(0, 1, lty = 2)


## Posterior predictive p-value on T(y, theta) = max(y)
hist(apply(sim2_stan$y_pred, 1, max))
abline(v = max(data$y), col = "red", lwd = 2)
mean(apply(sim2_stan$y_pred, 1, max) <= max(data$y))

## Information criteria
sim2_stan$y_llik

library("loo")
loo::loo(sim2_stan$y_llik)
loo::waic(sim2_stan$y_llik)

## Compare models
loo::loo(sim_stan$y_llik)
loo::loo(sim2_stan$y_llik)
