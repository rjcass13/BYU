data {
  int<lower=1> n;
  int<lower=0> y[n];
  vector<lower=0>[n] days;
  vector<lower=0>[n] month;
}

parameters {
  real<lower=0.0> rate;
}

model {
  vector[n] mu = rate * days;
  y ~ poisson(mu);
  rate ~ gamma(1.5, 2.0);
}

generated quantities {
  vector[n] y_pred;
  vector[n] y_llik;
  vector<lower=0.0, upper=1.0>[n] y_upc;
  real<lower=0.0, upper=1.0> rate_upc;
  
  for (i in 1:n) {
    real mu_i = rate * days[i];
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
  
  rate_upc = gamma_cdf(rate | 1.5, 2.0);
}
