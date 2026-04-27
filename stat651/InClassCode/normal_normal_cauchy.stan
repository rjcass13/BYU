data {                   // Data block
  int<lower=1> n;        // sample size
  array[n] real y;             // response vector of length n
  real m0;               // prior mean for mu
  real<lower=0> sd0;     // prior st.dev for mu
  real<lower=0> s0;      // prior scale for sig
}

parameters {             // Parameters block
  real mu;              
  real<lower=0> sig;
}

model {                  // Model block

  mu ~ normal(m0, sd0);
  sig ~ cauchy(0.0, s0);

  y ~ normal(mu, sig);
}