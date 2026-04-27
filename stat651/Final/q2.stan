data {
    int<lower=0> N;
    array[N] real y;
    matrix[N,3] x;
    real alpha_mean;
    real alpha_sd;
    real beta_shape;
    real beta_rate;
    real phi_mean;
    real phi_sd;
    real sig2_shape;
    real sig2_scale;
}

parameters {
    real alpha;
    real beta;
    real phi;
    real<lower=0> epsilon;
}

transformed parameters {
  vector[3] betas;
  betas[1] = alpha - beta*phi^2; // Multiply by tau
  betas[2] = 2 * beta * phi;
  betas[3] = -beta;
}

model {
    alpha ~ normal(alpha_mean, alpha_sd);
    beta ~ gamma(beta_shape, beta_rate);
    phi ~ normal(phi_mean, phi_sd);
    epsilon ~ inv_gamma(sig2_shape, sig2_scale);
    
    y ~ normal(x * betas, epsilon);
}
