data {
    int<lower=0> N;
    array[N] int y;
    real m;
    real v;
    real s0;
}

parameters {
    real mu;
    real<lower=0> sigma;
}

model {
    y ~ normal(mu, sigma);
    mu ~ normal(m, v);
    sigma ~ cauchy(0, s0);
}