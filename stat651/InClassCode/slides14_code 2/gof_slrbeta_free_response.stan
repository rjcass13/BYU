data {
  int <lower=1> n;
  int <lower=1> n_grid;
  vector[n] x;
  vector[n] y;
  vector[n_grid] x_grid;
}

transformed data {
  vector<lower=0.0, upper=1.0>[n] yp = y / 100.1; // hack so that 100s don't break it
  real log100 = log(100.1);
}

parameters {
  real alpha;
  real beta;
  real<lower=0.0> M;
}

model {
  alpha ~ normal(0.0, 10.0);
  beta ~ normal(0.0, 10.0);
  M ~ cauchy(20.0, 20.0);
  
  for (i in 1:n) {
    real xb = alpha + beta * x[i];
    real mu = inv_logit(xb);
    real a = M * mu;
    real b = M * (1.0 - mu);
    target += beta_lpdf(yp[i] | a, b) - log100;
  }
}

generated quantities {
  vector[n] y_pred;
  vector[n_grid] y_grid;
  vector[n] y_llik;
  vector<lower=0.0, upper=1.0>[n] y_upc;
  real<lower=0.0, upper=1.0> alpha_upc;
  real<lower=0.0, upper=1.0> beta_upc;
  real<lower=0.0, upper=1.0> M_upc;
  
  for (i in 1:n) {
    real xb_i = alpha + beta * x[i];
    real mu_i = inv_logit(xb_i);
    real a_i = M * mu_i;
    real b_i = M * (1.0 - mu_i);

    y_pred[i] = beta_rng(a_i, b_i) * 100.1;
    y_llik[i] = beta_lpdf(yp[i] | a_i, b_i) - log100;
    y_upc[i] = beta_cdf(yp[i] | a_i, b_i);
  }  

  for (i in 1:n_grid) {
    real xb_i = alpha + beta * x_grid[i];
    real mu_i = inv_logit(xb_i);
    real a_i = M * mu_i;
    real b_i = M * (1.0 - mu_i);

    y_grid[i] = beta_rng(a_i, b_i) * 100.1;
  }  

  alpha_upc = normal_cdf(alpha | 0.0, 10.0);
  beta_upc = normal_cdf(beta | 0.0, 10.0);
  M_upc = (cauchy_cdf(M | 20.0, 20.0) - cauchy_cdf(0.0 | 20.0, 20.0)) / (1.0 - cauchy_cdf(0.0 | 20.0, 20.0));
}
