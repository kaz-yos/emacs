functions {

}

data {
  // Number of observations
  int<lower=1> N;
  // Whether to evaluate likelihood
  int<lower=0,upper=1> use_lik;
}

transformed data {

}

parameters {

}

transformed parameters {

}

model {
  // Priors

  // Likelihood
  if (use_lik == 1) {

  }
}

generated quantities {
  // For loo
  vector[N] log_lik;
  // For posterior predictive checks
  real y_rep[N];

  for (i in 1:N) {
    // Observation level log likelihood
    log_lik[i] = ;
    // Prediction
    y_rep[i] = ;
  }
}
