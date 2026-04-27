data {
  int <lower=1> N;
  int<lower=0> n[N];
  int<lower=0> y[N];
}

parameters {
  vector<lower=0.0, upper=1.0>[N] theta;
  real<lower=0.0, upper=1.0> mu;
  real<lower=0.0> M;
}

// transformed parameters { // only used if the prior is placed on alpha, beta. Then we would define M and mu here
//   real<lower=0.0> alpha = M * mu;
//   real<lower=0.0> beta = M * (1.0 - mu);
// }

model {
  y ~ binomial(n, theta);
  
  real alpha = M * mu;
  real beta = M * (1.0 - mu);
  theta ~ beta(alpha, beta);
  
  M ~ gamma(7.0, 1.0);
  mu ~ beta(2.5, 2.0);
}

generated quantities {
  real alpha = M * mu;
  real beta = M * (1.0 - mu);
  real theta_new;
  int y_new;
  int y_4_new;
 
  theta_new = beta_rng(alpha, beta);
  y_new = binomial_rng(5, theta_new);
  y_4_new = binomial_rng(5, theta[4]);
}