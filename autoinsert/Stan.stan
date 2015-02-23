data {
  // Prior alpha
  int alpha;
  // Prior beta
  int beta;  
  // Define variables in data
  // Number of observations (an integer)
  int<lower=0> N;
  // Birthweight outcome (a real vector of length N)
  real y[N];
}

parameters {
  // Define parameters to estimate
  // Population mean (a real number)
  real mu;
  // Population variance (a positive real number)
  real<lower=0> sigmaSq;
}

transformed parameters  {
  // Population standard deviation (a positive real number)
  real<lower=0> sigma;
  // Standard deviation (derived from variance)
  sigma <- sqrt(sigmaSq);
}

model {
  // Prior part of Bayesian inference
  // Flat prior for mu (no need to specify if non-informative)
  // sigma^2 has inverse gamma (alpha = 1, beta = 1) prior
  sigmaSq ~ inv_gamma(alpha, beta);

  // Likelihood part of Bayesian inference
  // Outcome model N(mu, sigma^2) (use SD rather than Var)
  y ~ normal(mu, sigma);    
}
