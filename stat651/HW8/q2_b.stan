data {
    int<lower=0> n; //Total datapoints
    int<lower=0> p; // Total betas
    matrix[n, p] X; // Predictor matrix
    vector[n] y; // Responses
    real alpha_0;
    real beta_0;
    real lambda_0; // scale factor
}

parameters {
    vector[p] beta_raw;
    real alpha;
    real<lower=0> sigma2;
    vector<lower=0>[p] lambda;
    real<lower=0> tau;
}

transformed parameters {
  vector[p] beta;
  beta = beta_raw .* lambda * tau * sqrt(sigma2); // Multiply by tau
}

model {
    lambda ~ cauchy(0, 1);
    tau ~ cauchy(0, 1);
    alpha ~ normal(0, 10); // Weekly informative prior
    sigma2 ~ inv_gamma(alpha_0, beta_0);
    beta_raw ~ std_normal();
    y ~ normal(X * beta + alpha, sqrt(sigma2));  // data model
}