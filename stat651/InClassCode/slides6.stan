data {
    int<lower=0> N;
    array[N] int y;
    vector[N] x; //dose
    array[N] int n_vec;
}

parameters {
    real alpha;
    real beta;
}

model {
    vector[N] theta_vec = inv_logit(alpha + beta * x);

    y ~ binomial(n_vec, theta_vec);
    alpha ~ normal(0, sqrt(3));
    beta ~ normal(1, 1);
}