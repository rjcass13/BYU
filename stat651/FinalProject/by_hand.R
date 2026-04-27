source('functions.R')
library(tidyverse)

##############################
######## PREPARE DATA ########
##############################

data <- read_csv("train.csv")

y <- data[,15]

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
X <- scale(X, center = TRUE, scale = FALSE)
y = data$`income_>50K`

p <- ncol(X)

################################
######## PREPARE PRIORS ########
################################

alpha_var <- 10

beta_means <- rep(0, p) # Betas default to 0 mean (updated later for age and hours)
beta_sds   <- rep(0.7, p) # Betas default to .7 var (updated later for age and hours)

# optional informative priors
beta_means[grep("^age", colnames(X))]   <- 0.05
beta_means[grep("^hours", colnames(X))] <- 0.03

beta_sds[grep("^age", colnames(X))]   <- 0.5
beta_sds[grep("^hours", colnames(X))] <- 0.5

# The non-white Race have hierarchical prior
super_p <- 1 # Number of shared parameters
shared_inds <- rep(0, p)
race_cols <- grep("^race", colnames(X))
shared_inds[race_cols[1:(length(race_cols) - 1)]] <- 1

# Hierarchical variance for race
race_var <- 0.5
race_mean_var <- 0.5


##########################
######## RUN MCMC ########
##########################
if (FALSE) {
  mcmc_iter <- 2000
  mcmc_burn_in <- 200

  mcmc_start_time <- Sys.time()
  out <- run_mcmc(X = X, y = y, alpha_var = alpha_var, beta_means = beta_means, beta_sds = beta_sds,
    race_var = race_var, race_mean_var = race_mean_var, shared_inds = shared_inds, n_iter = mcmc_iter)
  mcmc_end_time <- Sys.time()

  # Checking acceptance rates
  out$accept_alpha
  out$accept_beta
  out$accept_shared

  samples <- out$samples

  ess <- coda::effectiveSize(samples)
  #discard burnin
  samples <- samples[-c(1:mcmc_burn_in),]

  # Trace plot
  plot(samples[,9], type = "l")

  # Posterior means
  beta_hat <- colMeans(samples)

  # Posterior SDs
  beta_sd <- apply(samples, 2, sd)
}


##########################
######## RUN STAN ########
##########################
ppl_iter <- 2000
ppl_burn_in <- 1000
data_stan <- list(n = nrow(data), p = p, X = X, super_p = super_p, shared_inds = shared_inds, y = y,
                  alpha_var = alpha_var, prior_means = beta_means, prior_sds = beta_sds, 
                  race_hier_mean = race_shared_mean, race_hier_var = race_mean_var)

params <- c("alpha", "beta", "shared_mean")

n_cores <- parallel::detectCores()   # Count how many cores are available
options(mc.cores = n_cores)          

ppl_start_time <- Sys.time()
fit_stan <- stan(model_code = readLines("final_vec.stan"), data = data_stan, pars = params,
                 iter = 2000, warmup = 1000, chains = n_cores)
ppl_end_time <- Sys.time()

sim_stan <- extract(fit_stan) # collect samples from all chains
