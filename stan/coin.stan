data {
  int<lower=0> n;
  int<lower=0> m;
  real<lower=0> alpha;
  real<lower=0> beta;
}

parameters {
  real<lower=0, upper=1> theta;
}

model {
  theta ~ beta(alpha, beta); // prior
  m ~ binomial(n, theta); //
}

