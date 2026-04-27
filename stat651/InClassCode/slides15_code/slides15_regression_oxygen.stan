data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  matrix[N, K] X;   // predictor matrix
  vector[N] y;      // outcome vector
}

parameters {
  real alpha;           // intercept
  vector[K] beta;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}

model {
  y ~ normal(X * beta + alpha, sigma);  // data model
  alpha ~ normal(0.0, 1.0e3);
  beta ~ normal(0.0, 1.0e2);
  sigma ~ cauchy(0.0, 10.0);
}

generated quantities {
  vector[N] llik;
  for (i in 1:N) {
    llik[i] = normal_lpdf(y[i] | X[i,] * beta + alpha, sigma);
  }
}
