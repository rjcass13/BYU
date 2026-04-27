data {
  int <lower=1> n;
  int <lower=1> n_grid;
  vector[n] x;
  vector[n] y;
  vector[n_grid] x_grid;
}

parameters {
  real alpha;
  real beta;
  real<lower=0.0> sig;
}

model {
  alpha ~ normal(0.0, 100.0);
  beta ~ normal(0.0, 100.0);
  sig ~ cauchy(0.0, 20.0);
  
  y ~ normal(alpha + beta * x, sig);
}

generated quantities {
  vector[n] y_pred;
  vector[n_grid] y_grid;
  vector[n] y_llik;
  vector<lower=0.0, upper=1.0>[n] y_upc;
  real<lower=0.0, upper=1.0> alpha_upc;
  real<lower=0.0, upper=1.0> beta_upc;
  real<lower=0.0, upper=1.0> sig_upc;
  
  for (i in 1:n) {
    real mu_i = alpha + beta * x[i];
    y_pred[i] = normal_rng(mu_i, sig);
    y_llik[i] = normal_lpdf(y[i] | mu_i, sig);
    y_upc[i] = normal_cdf(y[i] | mu_i, sig);
  }  

  for (i in 1:n_grid) {
    real mu_i = alpha + beta * x_grid[i];
    y_grid[i] = normal_rng(mu_i, sig);
  }

  alpha_upc = normal_cdf(alpha| 0.0, 100.0);
  beta_upc = normal_cdf(beta| 0.0, 100.0);
  sig_upc = 2.0*(cauchy_cdf(sig| 0.0, 20.0) - 0.5); // fix support mismatch
}