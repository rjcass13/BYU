data {
  int<lower=0> N;   // number of data items
  int<lower=0> Jcat;  // number of categorical predictors
  int<lower=0> J;  // number of continuous predictors
  matrix[N, Jcat] Xcat; // categorical predictor matrix
  matrix[N, J] X;   // continuous predictor matrix
  vector[N] y;      // outcome vector
}

parameters {
  real alpha;           // intercept
  
  vector[J] beta;       // coefficients for continuous predictors
  real<lower=0> tau2;            // global shrinkage
  vector<lower=0>[J] lambda;     // local shrinkage
  
  vector[Jcat] beta_cat;   // coefficients for categorical predictors

  real<lower=0> sigma;  // error scale parameter
}

model {
  y ~ normal(alpha + X * beta + Xcat * beta_cat, sigma);  // data model
  
  alpha ~ normal(0.0, 1.0e3);
  
  beta ~ normal(0.0, sqrt(tau2 * lambda));
  tau2 ~ cauchy(0.0, sigma);
  lambda ~ cauchy(0.0, 1.0);
  
  beta_cat ~ normal(0.0, 1.0e3);
  
  sigma ~ cauchy(0.0, 10.0);
}

// generated quantities {
//   vector[N] llik;
//   for (i in 1:N) {
//     llik[i] = normal_lpdf(y[i] | alpha + X[i,] * beta + X_cat[i,] * beta_cat, sigma);
//   }
// }