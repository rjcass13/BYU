#####################################
######### BY HAND FUNCTIONS #########
#####################################

# Log Posterior for use in MCMC
log_posterior <- function(alpha, beta, shared_params, X, y, alpha_var, beta_means, beta_sds, race_var, race_mean_var, shared_inds) {
  
  eta <- alpha + X %*% beta
  loglik <- sum(y * eta - log1p(exp(eta)))
  
  logprior <- 0
  
  # alpha
  logprior <- logprior + dnorm(alpha, 0, sqrt(alpha_var), log = TRUE)
  
  # Get the prior contribution for each beta
  for (i in 1:length(beta)) {
    # race hierarchical
    if (shared_inds[i] == 1) {
      logprior <- logprior + dnorm(beta[i], shared_params[1], sqrt(race_var), log = TRUE)
    } else {
      # independent priors
      logprior <- logprior + dnorm(beta[i], beta_means[i], beta_sds[i], log = TRUE
      )
    }
  }
  
  # prior for race mean
  logprior <- logprior + dnorm(shared_params[1], 0, sqrt(race_mean_var), log = TRUE)
  
  return(loglik + logprior)
}

# Perform 1 MCMC step for one variable
mh_step <- function(current, proposal_sd, log_post_fn) {
  
  proposal <- rnorm(1, current, proposal_sd)
  
  log_accept_ratio <- log_post_fn(proposal) - log_post_fn(current)
  
  if (log(runif(1)) < log_accept_ratio) {
    return(list(value = proposal, accept = 1))
  } else {
    return(list(value = current, accept = 0))
  }
}

# Perform an MCMC simulation
run_mcmc <- function(X, y, alpha_var, beta_means, beta_sds, race_var, race_mean_var, shared_inds, n_iter = 2000) {
  p <- ncol(X)
  
  # Initialize
  alpha <- 0
  beta <- rep(0, p)
  shared_params <- 0
  proposal_sd_alpha <- 0.1
  proposal_sd_beta  <- c(0.07, 0.07, 0.2, 0.1, 0.1, 0.15, 0.1, 0.1, 0.1, 0.07, 0.07)
  proposal_sd_shared <- rep(0.8, length(shared_params))
  
  accept_alpha <- 0
  accept_beta  <- rep(0, p)
  accept_shared <- rep(0, length(shared_params))
  
  race_cols <- which(shared_inds == 1)
  nonrace_cols <- which(shared_inds == 0)
  
  
  # Storage
  beta_store <- matrix(0, n_iter, p)
  
  for (iter in 1:n_iter) {
    # --- update alpha ---
    res <- mh_step(alpha, proposal_sd_alpha, function(a) {
      log_posterior(a, beta, shared_params, X, y, alpha_var, beta_means, beta_sds, race_var, race_mean_var, shared_inds)
    })
    alpha <- res$value
    accept_alpha <- accept_alpha + res$accept
    
    # --- update beta (non-race individually) ---
    for (j in nonrace_cols) {
      res <- mh_step(beta[j], proposal_sd_beta[j], function(bj) {
        beta_temp <- beta
        beta_temp[j] <- bj
        log_posterior(alpha, beta_temp, shared_params, X, y, alpha_var, beta_means, beta_sds, race_var, race_mean_var, shared_inds)
      })

      beta[j] <- res$value
      accept_beta[j] <- accept_beta[j] + res$accept
    }
    # --- block update for race coefficients ---
    beta_block_current <- beta[race_cols]
    
    # propose new block
    proposal_block <- beta_block_current + 
      rnorm(length(race_cols), 0, proposal_sd_beta[race_cols])
    
    # build proposed full beta
    beta_temp <- beta
    beta_temp[race_cols] <- proposal_block
    
    # compute log acceptance ratio
    beta_temp_lik <- log_posterior(alpha, beta_temp, shared_params, X, y, alpha_var, beta_means, beta_sds, race_var, race_mean_var, shared_inds)
    beta_lik <- log_posterior(alpha, beta, shared_params, X, y, alpha_var, beta_means, beta_sds, race_var, race_mean_var, shared_inds)
    log_accept_ratio <- beta_temp_lik - beta_lik
    
    # accept/reject
    if (log(runif(1)) < log_accept_ratio) {
      beta[race_cols] <- proposal_block
      accept_beta[race_cols] <- accept_beta[race_cols] + 1
    }
    
    # --- update shared params ---
    for (k in 1:length(shared_params)) {
      res <- mh_step(shared_params[k], proposal_sd_shared[k], function(sk) {
        sp_temp <- shared_params
        sp_temp[k] <- sk
        log_posterior(alpha, beta, sp_temp, X, y, alpha_var, beta_means, beta_sds, race_var, race_mean_var, shared_inds)
      })
      
      shared_params[k] <- res$value
      accept_shared[k] <- accept_shared[k] + res$accept
    }
    
    beta_store[iter, ] <- beta
  }
  
  list(samples = beta_store, 
    accept_alpha = accept_alpha / n_iter, 
    accept_beta  = accept_beta / n_iter,
    accept_shared = accept_shared / n_iter
  )
}

###################################
######### DATA GENERATION #########
###################################

one_hot <- function(n, k) {
  mat <- matrix(0, n, k)
  idx <- sample(1:k, n, replace = TRUE)
  mat[cbind(1:n, idx)] <- 1
  return(mat)
}

generate_X <- function(n) {
  
  age   <- as.numeric(scale(runif(n, 18, 80)))
  hours <- as.numeric(scale(runif(n, 1, 100)))
  
  edu   <- one_hot(n, 3)
  marital <- one_hot(n, 1)
  race  <- one_hot(n, 4)
  gender <- rbinom(n, 1, 0.5)
  
  X <- cbind(
    age,        # 1
    hours,      # 2
    edu,        # 3–5
    marital,    # 6
    race,       # 7-10
    gender      # 11
  )
  
  return(X)
}

generate_beta_true <- function(p, shared_inds, alpha_var, prior_means, prior_sds, race_hier_mean, race_mean_var) {

  # Continuous effects
  beta_true <- prior_means

  # Shared Means
  for (i in 1:p) {
    if (shared_inds[i] > 0) {
        beta_true[i] <- race_hier_mean + rnorm(1, 0, race_mean_var)
    }
  }
  
  return(beta_true)
}

generate_data <- function(n, X, beta_true) {
  
  eta  <- alpha_true + X %*% beta_true
  prob <- 1 / (1 + exp(-eta))
  
  y <- rbinom(n, 1, prob)
  return(y)
}

generate_stan_data <- function(n, p, X, super_p, y, shared_inds, alpha_var, prior_means, prior_sds, race_hier_mean, race_mean_var, race_cols){
  
  list(
    n = n,
    p = p,
    X = X,
    super_p = super_p,
    shared_inds = shared_inds,
    y = y,
    alpha_var = alpha_var, 
    prior_means = beta_means, 
    prior_sds = beta_sds, 
    race_hier_mean = race_hier_mean, 
    race_hier_var = race_mean_var, 
    race_cols = race_cols
  )
}