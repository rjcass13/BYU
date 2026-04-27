data {
  int<lower=1> n;
  int<lower=0> y[n];
  vector<lower=0>[n] days;
  vector<lower=0>[n] month;
}

parameters {
  real alpha;
  real beta;
}

model {
  vector[n] rate = exp(alpha + beta*month);
  vector[n] mu = rate .* days;
  y ~ poisson(mu);
  alpha ~ normal(-1.0, 5.0);
  beta ~ normal(0.0, 5.0);
}


generated quantities {
  vector[n] y_pred;
  vector[n] y_llik;
  vector<lower=0.0, upper=1.0>[n] y_upc;
  real<lower=0.0, upper=1.0> alpha_upc;
  real<lower=0.0, upper=1.0> beta_upc;
  
  for (i in 1:n) {
    real mu_i = exp(alpha + beta*month[i]) * days[i];
    real y_upc_lower;
    real y_upc_upper;
    
    y_pred[i] = poisson_rng(mu_i);
    y_llik[i] = poisson_lpmf(y[i] | mu_i);
    
    if (y[i] == 0) {
      y_upc_lower = 0.0;
    } else {
      y_upc_lower = poisson_cdf(y[i] - 1 | mu_i);
    }
  
    y_upc_upper = poisson_cdf(y[i] | mu_i);
    
    real u = uniform_rng(0, 1);
    y_upc[i] = y_upc_lower + u * (y_upc_upper - y_upc_lower);
  }
  
  alpha_upc = normal_cdf(alpha | -1.0, 5.0);
  beta_upc = normal_cdf(beta | 0.0, 5.0);
}
