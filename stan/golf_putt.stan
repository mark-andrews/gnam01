data {
  int<lower=0> J;
  int n[J];
  int y[J];
  vector[J] x;
  real R;
}

parameters {
  real<lower=0> sigma;
}

model {
  vector[J] p;
  for (j in 1:J){
    p[j] = 2*Phi(asin(R/x[j]) / sigma) - 1;
  }
  y ~ binomial(n, p);
}

generated quantities {
  real sigma_degrees;
  sigma_degrees = (180/pi())*sigma;
}
