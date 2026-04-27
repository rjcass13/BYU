data {
    int<lower=0> N; //Number of Schools
    int<lower=0> n; //Total datapoints
    array[n] real y; // Responses
    array[n] int grp; //Associated group for eash response
    real nu_0;
    real sigma2_0;
    real eta_0;
    real tau2_0;
    real mu_0;
    real gamma2_0;
}

parameters {
    array[N] real theta;
    real mu;
    real<lower=0> sigma2;
    real<lower=0> tau2;
    
}

transformed parameters {
    array[n] real theta_vec;
    for (i in 1:n) {
        theta_vec[i] = theta[grp[i]];
    }
}

model {
    y ~ normal(theta_vec, sqrt(sigma2));
    theta ~ normal(mu, sqrt(tau2));
    sigma2 ~ inv_gamma(nu_0/2, nu_0 * sigma2_0/2);
    tau2 ~ inv_gamma(eta_0/2, eta_0 * tau2_0/2);
    mu ~ normal(mu_0, sqrt(gamma2_0));
}