stan_code = '
data {
  int N; // Number of obs
  int y[N]; // Response variable
  vector[N] x; // Covariate
  int N_class; // Number of classes
  int class[N]; // Index for classes
}
parameters {
  vector[N] alpha; // Intercept for each observation
  vector[N_class] delta; // Intercept for each class
  real beta; // Slope associated with x
  real mu_alpha; // Mean intercept
  real<lower = 0> sigma_alpha; // sd between intercepts
  real<lower = 0> sigma_class; // sd between classes
}
transformed parameters {
  vector[N] log_rate; // log rate parameter
  for (i in 1:N) {
    log_rate[i] = alpha[i] + delta[class[i]] + beta * x[i];
  }
}
model {
  // Likelihood
  for (i in 1:N) {
    y[i] ~ poisson_log(log_rate[i]);
    alpha[i] ~ normal(mu_alpha, sigma_alpha);
  }
  // Random effects prior
  for (j in 1:N_class) {
    delta[i] ~ normal(0, sigma_class);
  }
  // Priors
  mu_alpha ~ normal(0, 2);
  beta ~ normal(0, 2);
  sigma_alpha ~ cauchy(0, 2);
  sigma_class ~ cauchy(0, 2);
}
'

# Call in Stan
library(rstan)

# Remove missing values from data first
##CODE HERE


# Create data list
my_data = list(y = ...
               x = ...,
               N = ...,
               N_class = ...,
               class = ...)

# Run Stan
stan_run = stan(data = my_data,
                model_code = stan_code)
