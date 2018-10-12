stan_code = '
data {
  int N; // Number of obs
  vector[N] Ref_EMG; // Response variable
  vector[N] TL; // Covariate
  int N_replicate; // Number of replicates
  int replicate[N]; // replicate index
}
parameters {
  vector[N_replicate] alpha; // Intercept by replicate
  real beta // Slope associated with TL
  real mu_alpha; // Mean intercept
  real<lower = 0> sigma_alpha; // Variability between intercepts
}
model {
  // Likelihood
  for (i in 1:N) {
    y[i] ~ binomial_logit(alpha[replicate[i]] + beta * TL[i]);
  }
  // Priors
  for (j in 1:N_replicate) {
    alpha[j] ~ normal(mu_alpha, sigma_alpha);
  }
  
  mu_alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  residual_sd ~ cauchy(0, 10);
  sigma_alpha ~ cauchy(0, 10);
}
'

# Call in Stan
library(rstan)

# Remove missing values from data first
##CODE HERE


# Create data list
my_data = list(Ref_EMG = ...
               TL = ....,
               N = nrow( ... ),
               replicate = ...)

# Run Stan
stan_run = stan(data = my_data,
                model_code = stan_code)
