suppressPackageStartupMessages(library("rstan"))
library('dplyr')
library('fastDummies')
library(tidyverse)

set.seed(1337)

data <- read_csv("train.csv")

y <- data[[15]]

data <- data |>
  mutate(
    age = as.numeric(scale(age)),
    hours = as.numeric(scale(`hours-per-week`)),
    education = case_when(
      education %in% c("Doctorate", "Masters") ~ "Graduate",
      education %in% c("Bachelors", "Assoc-voc", "Prof-school", "Assoc-acdm", "Some-college") ~ "Undergraduate",
      education %in% c("HS-grad", "12th", "7th-8th", "9th", "10th", "11th") ~ "HighSchool",
      TRUE ~ "Elementary"
    ),
    marital = ifelse(`marital-status` %in% c("Divorced","Separated","Never-married","Widowed"), "Single", "Married")
  )

X <- model.matrix(~ age + hours + education + marital + race + gender, data)[, -1]

p <- ncol(X)

colnames(X)

shared_inds <- rep(0, p)

# Race only gets hierarchy
race_cols <- grep("^race", colnames(X))
shared_inds[race_cols[1:(length(race_cols) - 1)]] <- 1

super_p <- 1

alpha_var <- 10

beta_means <- rep(0, p)
beta_sds   <- rep(10, p)

# optional informative priors
beta_means[grep("^age", colnames(X))]   <- 0.05
beta_means[grep("^hours", colnames(X))] <- 0.03

beta_sds[grep("^age", colnames(X))]   <- 10
beta_sds[grep("^hours", colnames(X))] <- 10

# hierarchical variance for race
race_shared_mean <- 0
race_mean_var <- 10

data_stan <- list(n = nrow(data), p = p, X = X, super_p = super_p, shared_inds = shared_inds, y = y,
                  alpha_var = alpha_var, prior_means = beta_means, prior_sds = beta_sds, 
                  race_hier_mean = race_shared_mean, race_hier_var = race_mean_var)

params <- c("beta", "shared_mean")

n_cores <- parallel::detectCores()   # Count how many cores are available
options(mc.cores = n_cores)          

start_time <- Sys.time()
fit_stan <- stan(model_code = readLines("final_vec.stan"),
                 data = data_stan, pars = params,
                 iter = 2000, warmup = 1000, thin = 5, chains = n_cores)
end_time <- Sys.time()

print(end_time - start_time)

sim_stan <- extract(fit_stan) # collect samples from all chains

sim_beta <- extract(fit_stan, pars="beta")
# occ_cols <- grep("^occupation_", colnames(dt), value = TRUE)
# 
# occ_counts <- colSums(dt[, occ_cols])
# sort(occ_counts)