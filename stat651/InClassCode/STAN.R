library("rstan")

data <- list(y = y, n = n, 
             m0 = 30.0, sd0 = sqrt(100.0), s0 = 5.0)

params <- c("mu", "sig")

n_cores <- parallel::detectCores()   # Count how many cores are available
options(mc.cores = n_cores)          

fit_stan <- stan(model_code = readLines("normal_normal_cauchy.stan"),
                 data = data, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_cores)

sim_stan <- extract(fit_stan)
