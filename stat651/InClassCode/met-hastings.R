library('posterior')
y <- c(3, 4, 3, 1, 1, 4, 1, 4, 2, 1, 1, 3, 1, 1, 4)

sum_y <- sum(y)
n <- length(y)

log_target <- function(lambda, stats) {
  if (lambda < 0 | lambda > 10) {
    out <- -Inf
  } else {
    out <- -stats$n*lambda + stats$sum_y*log(lambda) - 1/(2*4^2)*(lambda-1)^2
  }

  out
}
# curve(exp(log_target(x, n=n, sum_y = sum_y)), from = 0, to = 10)

rw_norm_met <- function(log_target, lambda_old, candidate_sd, stats) {
  # Step 1: Draw Candidate
  lambda_candidate <- rnorm(1, lambda_old, sd = candidate_sd)

  # Step 2: Compute Acceptance Probability
  log_alpha <- log_target(lambda_candidate, stats) - log_target(lambda_old, stats)

  # Step 3: Decision
  if (log(runif(1)) < log_alpha) {
    out <- lambda_candidate
  } else {
    out <- lambda_old
  }

  out
}

n_iter <- 10000
state <- list(lambda = 1.5)
sims <- numeric(n_iter)
stats <- list(n = n, sum_y = sum_y)

for (i in 1:n_iter) {
  state$lambda <- rw_norm_met(log_target, state$lambda, 5, stats)
  sims[i] <- state$lambda
}

plot(sims, type = 'l')
hist(sims, freq = FALSE)
xx <- seq(0, 4, length = 200)
lines(xx, 500*sapply(xx, \(x) exp(log_target(x, stats = stats))), col = 'blue')


ess_mean(sims)
acf(sims)

plot(sims[seq(1, n_iter, by = 10)], type = 'l')
ess_mean(sims[seq(1, n_iter, by = 10)])
