data {
    int<lower=0> N;
    array[N] int y;
}

parameters {
    real theta;
}

model {
    y ~ exponential(theta);
    theta ~ gamma(11, 5500); //From HW2 Key
}