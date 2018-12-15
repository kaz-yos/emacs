data {
    real<lower=0> alpha;
    real<lower=0> beta;
    int<lower=0> N;
    real y[N];
}

transformed data {

}

parameters {
    real mu;
    real<lower=0> sigma_sq;
}

transformed parameters {
    real<lower=0> sigma;
    sigma <- sqrt(sigma_sq);
}

model {
    sigma_sq ~ inv_gamma(alpha, beta);
    y ~ normal(mu, sigma);
}

generated quantities {

}
