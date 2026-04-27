data {
    int<lower=0> N;
    array[N] int x;
    array[N] int t;
    real alpha;
    real beta;
}

parameters {
    real theta;
}

model {
    real sum_x = sum(x);
    real sum_t = sum(t);
    x ~ exponential(theta);
    theta ~ gamma(alpha, beta);
}