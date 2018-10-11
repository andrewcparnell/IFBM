# Simulate and fit a Weibull regression model for fitting in rstan

# Start code
rm(list = ls())
library(rstan)

# Simulate some data
N = 100
x = sort(runif(N, -1, 1))
alpha = 3
beta = 2
shape = 2
y = rweibull(N, shape, exp(alpha + beta*x))
plot(x, y)

# Stan code
stan_code_wb = '
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
  real shape;
}
parameters {
  real alpha;
  real beta;
} 
transformed parameters{
  vector<lower = 0>[N] scale;
  scale = exp(alpha + beta * x);
}
model {
  y ~ weibull(shape, scale);
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
}
generated quantities {
  vector[N] y_rep;
  for (i in 1:N) 
    y_rep[i] = weibull_rng(shape, scale[i]);
}
'

# Run the code
stan_run_wb = stan(data = list(N = N,
                               y = y,
                               x = x,
                               shape = 2),
                    model_code = stan_code_wb)
plot(stan_run_wb)

## PP check
y_rep = extract(stan_run_wb, 'y_rep')$y_rep
pp_check(y, y_rep, ppc_scatter_avg)

