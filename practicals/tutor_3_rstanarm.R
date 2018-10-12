# Taken from classes 7, 8 and 9

# Boiler plate code
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
library(rstanarm)
#options(mc.cores = parallel::detectCores())
library(boot)
library(bayesplot)
library(rstan)
rstan_options(auto_write = TRUE)

## Load in data
dat = read.csv('../data/earnings.csv')
with(dat, plot(x_centered, y))

## Fixed effects model
mod_lm = lm(y ~ x_centered, data = dat)
summary(mod_lm)

## First stan lm
library(rstanarm)
mod_stan = stan_lm(y ~ x_centered, data = dat,
                   prior = R2(location = 0.5, 'mean'))
mod_stan

## Summarise
summary(mod_stan)

## Posterior intervals
posterior_interval(mod_stan)

## Second model
mod_stan_2 = stan_lm(y ~ x_centered, data = dat,
                     prior = R2(location = 0.01, 'mean'))
posterior_interval(mod_stan_2)

## Example stan code
## stan_code = '
## data {
##   int N;
##   vector[N] x;
##   vector[N] y;
## }
## parameters {
##   real intercept;
##   real slope;
##   real<lower=0> residual_sd;
## }
## model {
##   // Likelihood
##   y ~ normal(intercept + slope * x, residual_sd);
##   // Priors
##   intercept ~ normal(0, 100);
##   slope ~ normal(0, 100);
##   residual_sd ~ uniform(0, 100);
## }
## '

## Look at posterior
head(as.data.frame(mod_stan), 3)


# Class 8 -----------------------------------------------------------------

## Simulate some data - method 1
N = 10
x = 1:N
y = rnorm(N, mean = -2 + 0.4 * x, sd = 1)

## Method 2
eps = rnorm(N, mean = 0, sd = 1)
y = -2 + 0.4 * x + eps

## Simulate binomial data
y = rbinom(N, size = 1, prob = -2 + 0.4 * x)

## logit transformation
-2 + 0.4 * x
exp(-2 + 0.4 * x)/(1 + exp(-2 + 0.4 * x))

# Do it the R way
library(boot)
p = inv.logit(-2 + 0.4 * x)
y = rbinom(N, size = 1, prob = p)
y

## Simulate from a Poisson
lambda = exp(-2 + 0.4 * x)
y = rpois(N, lambda)
y

## Earnings model
earnings = read.csv('data/earnings.csv')
earnings$white = as.integer(earnings$eth == 3)
mod_1 = stan_lm(y ~ x_centered + white, data = earnings,
                prior = R2(location = 0.5, 'mean'))

## Output from Stan
round(as.data.frame(summary(mod_1)), 2)

## Plot
plot(mod_1)

## Second model
mod_2 = stan_lm(y ~ x_centered + white, data = earnings,
                prior = R2(location = 0.5, 'mean'),
                prior_intercept = normal(0, 10))

## Output from model
round(as.data.frame(summary(mod_2)), 2)

## Look at posterior
post = as.data.frame(mod_2)
head(post)

## 
mcmc_areas(post,
           pars = c("x_centered", "white", "sigma"))


## rstan code
stan_code = '
data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x1;
  vector[N] x2;
}
parameters {
  real alpha;
  real beta1;
  real beta2;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + x1 * beta1  + x2 * beta2, sigma);
}
'

## Run it through rstan
stan_run = stan(data = list(N = nrow(earnings), 
                            y = earnings$y,
                            x1 = earnings$x, 
                            x2 = earnings$white),
                model_code = stan_code)

## Plot output
plot(stan_run)

## Look at trace
mcmc_trace(post, pars = c('white', 'x_centered', 'sigma'),
           facet_args = list(nrow = 3))

## Intervals
mcmc_intervals(post)

## Histograms
mcmc_hist(post)

## Simulat from PP
y_sim = rnorm(nrow(earnings), 
              post$`(Intercept)`[1] + 
                post$x_centered[1] * earnings$x_centered + 
                post$white[1] * earnings$white, 
              sd = post$sigma[1])
plot(earnings$y, y_sim, las = 1)
abline(a = 0, b = 1, col = 'red')

## Quick way
y_rep = posterior_predict(mod_2)
y_rep_mean = apply(y_rep, 2, 'mean')
plot(earnings$y, y_rep_mean)
abline(a = 0, b = 1, col = 'red')

## Even quicker way
pp_check(mod_2)

## Willow tit data
swt = read.csv('../data/swt.csv')
head(swt)

## Stan glm
mod_3 = stan_glm(rep.1 ~ forest, 
                 data = swt,
                 family = binomial(link = 'logit'),
                 prior = normal(0, 1),
                 prior_intercept = normal(0, 5))

## Plot it
plot(mod_3)

## Look at summary
round(as.data.frame(summary(mod_3)), 2)

## Use posterior to get fitted values
post = as.data.frame(mod_3)
plot(swt$forest, swt$rep.1)
points(swt$forest, 
       inv.logit(mean(post$`(Intercept)`) + 
                   mean(post$forest)*swt$forest ),
       col = 'red')


# Class 9 -----------------------------------------------------------------

## Willow tit data again
swt = read.csv('../data/swt.csv')
head(swt)

## Get total sum with the data
sum_fun = function(x) {
  s = ifelse(is.na(x[1]),0,x[1]) + ifelse(is.na(x[2]),0,x[2]) + ifelse(is.na(x[3]),0,x[3])
  N = ifelse(is.na(x[1]),0,1) + ifelse(is.na(x[2]),0,1) + ifelse(is.na(x[3]),0,1)
  return(c(s,N))
}
swt$y = apply(swt[,1:3],1,sum_fun)[1,]
swt$N = apply(swt[,1:3],1,sum_fun)[2,]

## First mixed effects model in rstanarm
mod_1 = stan_glmer(cbind(y, N) ~  (forest | alt),
                   data = swt,
                   family = binomial(link = 'logit'),
                   prior = normal(0, 0.1),
                   prior_intercept = normal(0, 5))

## Output 
round(as.data.frame(summary(mod_1)), 2)

## Plot output
plot(mod_1)

## Histograms of intercepts
mcmc_hist(as.data.frame(mod_1), regex_pars = 'b\\[\\(Intercept',
          facet_args = list(nrow = 3))

## Histograms of coefficients
mcmc_hist(as.data.frame(mod_1), regex_pars = 'b\\[forest')

## PP check
y_rep = posterior_predict(mod_1)
y_rep_mean = apply(y_rep, 2, 'mean')
plot(swt$y, y_rep_mean)
abline(a = 0, b = 1, col = 'red')

## Quick version
pp_check(mod_1)

## OD Poisson model
swt$obs <- 1:nrow(swt)
mod_2 = stan_glmer(y ~ forest + (1 | obs),
                   family = poisson, data = swt)
mcmc_hist(as.data.frame(mod_2), regex_pars = 'forest')

## PP check
pp_check(mod_2)

## Simulate ordinal data
N = 100
alpha = -1
beta = 0.2
sigma = 0.51
set.seed(123)
x = runif(N, 0, 10)
cuts = c(-0.5, 0.5)
z = rnorm(N, alpha + beta * (x - mean(x)), sigma)
y = findInterval(z, cuts)
dat = data.frame(y = as.factor(y),
                 x = x)

## Plot the latent space
plot(x, z, col = y + 1)

## Fit using polr
mod_3 <- stan_polr(y ~ x, 
                   data = dat,
                   prior = R2(0.25, 'mean'), 
                   prior_counts = dirichlet(1))

## Look at histograms
mcmc_hist(as.data.frame(mod_3))

