data {
    int<lower=0> N; //Number of Schools
    int<lower=0> n; //Total datapoints
    array[n] real y; // Responses
    array[n] int grp; //Associated group for each response
    real sigma2_0;
    real eta_0;
    real tau2_0;
    real mu_0;
    real gamma2_0;
    real alpha_0;
}

parameters {
    array[N] real theta;
    array[N] real<lower=0> sigma2;
    real mu;
    real<lower=0> nu;
    real<lower=0> tau2;
    
}

transformed parameters {
    array[n] real theta_vec;
    array[n] real sigma2_vec;
    for (i in 1:n) {
        theta_vec[i] = theta[grp[i]];
    }
    for (i in 1:n) {
        sigma2_vec[i] = sigma2[grp[i]];
    }
}

model {
    y ~ normal(theta_vec, sqrt(sigma2_vec));
    theta ~ normal(mu, sqrt(tau2));
    sigma2 ~ inv_gamma(nu/2, nu * sigma2_0/2);
    tau2 ~ inv_gamma(eta_0/2, eta_0 * tau2_0/2);
    mu ~ normal(mu_0, sqrt(gamma2_0));
    nu ~ exponential(alpha_0); 
}

generated quantities {
    vector[n] y_rep;
    vector[n] y_upc; 
    vector[n] y_llik; 
    vector[N] theta_upc; 
    vector[N] sigma2_upc; 

    for (i in 1:n) { 
        real theta_i = theta[grp[i]]; 
        real sigma2_i = sigma2[grp[i]]; 
        y_rep[i] = normal_rng(theta_i, sqrt(sigma2_i)); 
        y_upc[i] = normal_cdf(y[i] | theta_i, sqrt(sigma2_i));
        y_llik[i] = normal_lpdf(y[i] | theta_i, sqrt(sigma2_i));
    }

    for (i in 1:N) {
        theta_upc[i] = normal_cdf(theta[i] | mu, sqrt(tau2));
    }

    for (i in 1:N) {
        sigma2_upc[i] = inv_gamma_cdf(sigma2[i] | nu/2, nu * sigma2_0 / 2);
    }
}
