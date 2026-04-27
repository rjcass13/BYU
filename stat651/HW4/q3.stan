data {
    int<lower=0> N;
    array[N] int y;
    vector[N] x; //dose
    array[N] int n_vec;

    vector[2] mu;
    matrix[2, 2] L_Sigma; // Cholesky factor of covariance
}

parameters {
    vector[2] theta; 
}

transformed parameters {
  // Put into 2 different variables
  real alpha;
  real beta;
  alpha = theta[1];
  beta = theta[2];
}

model {
    vector[N] theta_vec = inv_logit(alpha + beta * x);

    y ~ binomial(n_vec, theta_vec);
    theta ~ multi_normal_cholesky(mu, L_Sigma);
}
