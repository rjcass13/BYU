data {
    int<lower=0> n;          //Total datapoints
    int<lower=0> p;          // Total betas
    matrix[n, p] X;          // Predictor matrix
    int<lower=0> super_p;    // Number of shared parameters
    array[p] int shared_inds;   // Indicators of which shared parameter a Beta uses
    vector[n] y;             // Responses
    int<lower=0> sig2_0;     // Intercept prior: var.
    int<lower=0> gamma_0;    // Age prior: scale
    real theta_s_0;          // Edu prior: shared mean
    int<lower=0> nu2_0;      // Edu prior: group var.
    int<lower=0> nu2_s_0;    // Edu prior: shared var.
    real mu_s_0;             // Marital prior: shared mean
    int<lower=0> tau2_0;     // Marital prior: group var.
    int<lower=0> tau2_s_0;   // Marital prior: shared var.
    real lambda_0;           // Occ. prior: mean
    int<lower=0> rho2_0;     // Occ. prior: var.
    real pi_s_0;             // Race prior: shared mean
    int<lower=0> kappa2_0;   // Race prior: group var.
    int<lower=0> kappa2_s_0; // Race prior: shared var.
    real phi_0;              // Gender prior: mean
    int<lower=0> eps2_0;     // Gender prior: variance
    int<lower=0> psi_0;      // HoursPerWeek prior: scale
}

parameters {
    vector[p] beta;
    vector[super_p] shared_params;
}

model {
    for (i in 1:super_p) {
        // Edu - Elementary-Middle
        if (i == 1) shared_params[i] ~ normal(theta_s_0, sqrt(nu2_s_0));
        // Edu - HighSchool
        if (i == 2) shared_params[i] ~ normal(theta_s_0, sqrt(nu2_s_0));
        // Edu - Undergraduate
        if (i == 3) shared_params[i] ~ normal(theta_s_0, sqrt(nu2_s_0));
        // Edu - Graduate
        if (i == 4) shared_params[i] ~ normal(theta_s_0, sqrt(nu2_s_0));
        // Marital - Married
        if (i == 5) shared_params[i] ~ normal(mu_s_0, sqrt(tau2_s_0));
        // Marital - Single
        if (i == 6) shared_params[i] ~ normal(mu_s_0, sqrt(tau2_s_0));
        // Race - Non-White
        if (i == 7) shared_params[i] ~ normal(pi_s_0, sqrt(kappa2_s_0));
    }

    for (i in 1:p) {
        // BETAS: 
        // 1: Age
        if (i == 1) beta[i] ~ cauchy(0, gamma_0);
        // 2: HoursPerWeek
        if (i == 2) beta[i] ~ cauchy(0, psi_0);
        // 3-4, 15: Edu - Elementary-Middle
        if (i == 3 || i == 4 || i == 15) beta[i] ~ normal(shared_params[shared_inds[i]], sqrt(nu2_0));
        // 5-8, 13: Edu - HighSchool
        if ((i >= 5 && i <= 8) || i == 13) beta[i] ~ normal(shared_params[shared_inds[i]], sqrt(nu2_0));
        // 9-11, 16-17: Edu - Undergraduate
        if ((i >= 9 && i <= 11) || i == 16 || i == 17) beta[i] ~ normal(shared_params[shared_inds[i]], sqrt(nu2_0));
        // 12, 14: Edu - Graduate
        if (i == 12 || i == 14) beta[i] ~ normal(shared_params[shared_inds[i]], sqrt(nu2_0));
        // 18-20: Marital - Married
        if (i >= 18 && i <= 20) beta[i] ~ normal(shared_params[shared_inds[i]], sqrt(tau2_0));
        // 21-23: Marital - Single
        if (i >= 21 && i <= 23) beta[i] ~ normal(shared_params[shared_inds[i]], sqrt(tau2_0));
        // 24-37: Occupation
        if (i >= 24 && i <= 37) beta[i] ~ normal(lambda_0, sqrt(rho2_0));
        // 38-40: Race - Non-White
        if (i >= 38 && i <= 40) beta[i] ~ normal(shared_params[shared_inds[i]], sqrt(kappa2_0));
        // 41: Race - White
        if (i == 41) beta[i] ~ normal(pi_s_0, sqrt(kappa2_0));
        // 42: Gender - Male
        if (i == 42) beta[i] ~ normal(phi_0, sqrt(eps2_0));
    }

    y ~ normal(X * beta, sqrt(sig2_0));  // data model
}
