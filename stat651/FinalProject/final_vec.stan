data {
    int<lower=0> n;
    int<lower=0> p;
    matrix[n, p] X;

    int<lower=0> super_p;
    array[p] int shared_inds;

    array[n] int<lower=0, upper=1> y;

    real<lower=0> alpha_var; // Intercept

    vector[p] prior_means;
    vector<lower=0>[p] prior_sds;

    real race_hier_mean;
    real race_hier_var;
    array[4] int race_cols;
}

parameters {
    vector[p] beta_raw;
    real shared_raw;
    real alpha;
}

transformed parameters {
    vector[p] beta;
    real shared_mean;

    // -------------------------
    // Shared effects
    // -------------------------
    shared_mean = race_hier_mean + sqrt(race_hier_var) * shared_raw;

    // -------------------------
    // Individual coefficients
    // -------------------------
    beta = prior_means + prior_sds .* beta_raw;

    // Then iterate over only the shared means
    for(i in race_cols) {
        beta[i] = shared_mean + prior_sds[i] * beta_raw[i];
    }
}

model {
    beta_raw ~ std_normal();
    shared_raw ~ std_normal();

    alpha ~ normal(0, sqrt(alpha_var));

    y ~ bernoulli_logit_glm(X, alpha, beta);
}
