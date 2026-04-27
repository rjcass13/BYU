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
    vector[p] beta;
    real alpha;
    real<lower=0> sigma2;
}

model {
    alpha ~ normal(0, 10); // Weekly informative prior
    sigma2 ~ inv_gamma(alpha_0, beta_0);
    beta ~ normal(0, sigma2 * lambda_0);
    y ~ normal(X * beta + alpha, sqrt(sigma2));  // data model
}

generated quantities {
    vector[n] y_llik; 

    for (i in 1:n) { 
        y_llik[i] = normal_lpdf(y[i] | X[i] * beta + alpha, sqrt(sigma2));
    }
}
