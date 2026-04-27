
## data (x1, x2, and y) available from: https://www2.stat.duke.edu/~pdh10/FCBS/Replication/chapter9.R
x1<-c(0,0,0,0,0,0,1,1,1,1,1,1)
x2<-c(23,22,22,25,27,20,31,23,27,28,22,24)
y<-c(-0.87,-10.74,-3.27,-1.97,7.50,-7.25,17.05,4.96,10.40,11.05,0.26,2.51)

par(mfrow = c(1,1))
plot(y ~ x2, pch = 16, xlab = "age", ylab = "change in maximal oxygen uptake", 
     col = c("black", "gray")[x1+1])
legend(27, 0, legend = c("aerobic", "running"), 
       pch = c(16, 16), col = c("gray", "black"))



## STAN
library("rstan")

data_stan <- list(y = y, N = length(y), X = cbind(x1, x2))
data_stan$K <- ncol(data_stan$X)

params <- c("alpha", "beta", "sigma", "llik")

# n_cores <- parallel::detectCores()   # Count how many cores are available
n_cores <- 5
options(mc.cores = n_cores)          

fit_stan <- stan(model_code = readLines("slides15_regression_oxygen.stan"),
                 data = data_stan, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_cores)

sim_stan <- extract(fit_stan)

plot(fit_stan, par = "beta")
rstan::traceplot(fit_stan)
summary(fit_stan)

plot(y ~ x2, pch = 16, xlab = "age", ylab = "change in maximal oxygen uptake", 
     col = c("black", "gray")[x1+1], cex = 2)
legend(27, 0, legend = c("aerobic", "running"), 
       pch = c(16, 16), col = c("gray", "black"))
for (i in floor(seq(1, length(sim_stan$lp__), length = 50))) {
  abline(a = sim_stan$alpha[i], b = sim_stan$beta[i,2])
}
for (i in floor(seq(1, length(sim_stan$lp__), length = 50))) {
  abline(a = sim_stan$alpha[i] + sim_stan$beta[i,1], b = sim_stan$beta[i,2], col = "gray")
}

library("loo")
loo(sim_stan$llik)


### include interaction?
data_stan2 <- list(y = y, N = length(y), X = cbind(x1, x2, x1*x2))
data_stan2$K <- ncol(data_stan2$X)

fit_stan2 <- stan(model_code = readLines("slides15_regression_oxygen.stan"),
                  data = data_stan2, pars = params,
                  iter = 5000, warmup = 500, thin = 5, chains = n_cores)

sim_stan2 <- extract(fit_stan2)
loo(sim_stan2$llik)

plot(fit_stan2, par = "beta")

plot(y ~ x2, pch = 16, xlab = "age", ylab = "change in maximal oxygen uptake", 
     col = c("black", "gray")[x1+1], cex = 2)
legend(27, 0, legend = c("aerobic", "running"), 
       pch = c(16, 16), col = c("gray", "black"))
for (i in floor(seq(1, length(sim_stan$lp__), length = 50))) {
  abline(a = sim_stan2$alpha[i], b = sim_stan2$beta[i,2])
}
for (i in floor(seq(1, length(sim_stan$lp__), length = 50))) {
  abline(a = sim_stan2$alpha[i] + sim_stan2$beta[i,1], b = sim_stan2$beta[i,2] + sim_stan2$beta[i,3], col = "gray")
}

