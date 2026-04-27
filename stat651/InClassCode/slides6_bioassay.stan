data {
  int<lower=0> N; // number of dose levels
  array[N] int<lower=0> n_vec; // number of trials at each dose level
  array[N] int<lower=0> y; // number of adverse events at each dose level
  vector[N] x; // dose
}

parameters {
  real alpha; // intercept
  real beta; // slope
}

model {
  vector[N] theta_vec = inv_logit(alpha + beta * x); // translate to binomial probability
  
  y ~ binomial(n_vec, theta_vec);
  alpha ~ normal(0.0, sqrt(3.0));
  beta ~ normal(1.0, 1.0);
}
