data {
  int<lower=1> n;
  int<lower=0> y[n];
  vector<lower=0>[n] days;
  vector<lower=0>[n] month;
}

parameters {
  vector<lower=0>[n] rates;
  real<lower=0> rate_mean;
  real<lower=0> rate_var;
}

model {
  vector[n] mu = rates .* days;
  real rate_a = rate_mean^2 / rate_var;
  real rate_b = rate_mean / rate_var;
  
  y ~ poisson(mu);
  rates ~ gamma(rate_a, rate_b);
  
  rate_mean ~ gamma(1.5, 2.0);
  rate_var ~ gamma(1.0, 2.0);
}

generated quantities {
  vector[n] y_pred;
  vector[n] y_llik;
  vector<lower=0.0, upper=1.0>[n] y_upc;
  vector<lower=0.0, upper=1.0>[n] rates_upc;
  real<lower=0.0, upper=1.0> rate_mean_upc;
  real<lower=0.0, upper=1.0> rate_var_upc;
  real rate_a = rate_mean^2 / rate_var;
  real rate_b = rate_mean / rate_var;

  for (i in 1:n) {
    real mu_i = rates[i] * days[i];
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
    
    rates_upc[i] = gamma_cdf(rates[i] | rate_a, rate_b);
  }
  
  rate_mean_upc = gamma_cdf(rate_mean | 1.5, 2.0);
  rate_var_upc = gamma_cdf(rate_var | 1.0, 2.0);
}