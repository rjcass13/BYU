data {
  int<lower=1> n;
  int<lower=0> y[n];
}

parameters {
  real<lower=0.0> theta;
}

model {
  y ~ poisson(theta);
  theta ~ gamma(6.0, 0.5);
}

generated quantities {
  vector[n] y_pred;
  vector[n] y_llik;
  vector<lower=0.0, upper=1.0>[n] y_upc;
  real<lower=0.0, upper=1.0> theta_upc;
  
  for (i in 1:n) {
    real y_upc_lower;
    real y_upc_upper;
    
    y_pred[i] = poisson_rng(theta);
    y_llik[i] = poisson_lpmf(y[i] | theta);
    
    if (y[i] == 0) {
      y_upc_lower = 0.0;
    } else {
      y_upc_lower = poisson_cdf(y[i] - 1 | theta);
    }
  
    y_upc_upper = poisson_cdf(y[i] | theta);
    
    real u = uniform_rng(0, 1);
    y_upc[i] = y_upc_lower + u * (y_upc_upper - y_upc_lower);
  }
  
  theta_upc = gamma_cdf(theta | 6.0, 0.5);
}
