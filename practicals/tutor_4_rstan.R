# Code from classes 10, 11 and 12

# Clear the workspace and call in packages
rm(list = ls())
setwd("~/GitHub/IFBM/practicals")
library(rstan)
library(bayesplot)
library(loo)
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
#options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Class 10 ----------------------------------------------------------------

## Stan code for simple lr
stan_code = '
data {
  int N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> residual_sd;
} 
model {
  // Likelihood
  y ~ normal(intercept + slope * x, residual_sd);
  // Priors
  intercept ~ normal(0, 100);
  slope ~ normal(0, 100);
  residual_sd ~ uniform(0, 100);
}
'

## run on the earnings data
earnings = read.csv('../data/earnings.csv')
library(rstan)
#options(mc.cores = parallel::detectCores())
stan_run = stan(data = list(N = nrow(earnings), 
                            y = earnings$y,
                            x = earnings$x_centered),
                model_code = stan_code)

## Print and plot
print(stan_run)
plot(stan_run)

## Different version
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
## transformed parameters {
##   vector[N] fits;
##   for (i in 1:N) {
##     fits[i] = intercept + slope * x[i];
##   }
## }
## model {
##   // Likelihood
##   y ~ normal(fits, residual_sd);
##   // Priors
##   intercept ~ normal(0, 100);
##   slope ~ normal(0, 100);
##   residual_sd ~ uniform(0, 100);
## }
## '

## First mixed model
stan_code_mm = '
data {
  int N;
  int N_eth;
  vector[N] x;
  vector[N] y;
  int eth[N];
}
parameters {
  real intercept[N_eth];
  real slope;
  real mean_intercept;
  real<lower=0> residual_sd;
  real<lower=0> sigma_slope;
} 
model {
  // Likelihood
  for (i in 1:N) {
    y[i] ~ normal(intercept[eth[i]] + slope * x[i], residual_sd);
  }
  // Priors
  slope ~ normal(0, 0.1);
  for (j in 1:N_eth) {
    intercept[j] ~ normal(mean_intercept, sigma_slope);
  }
  mean_intercept ~ normal(11, 2);
  sigma_slope ~ cauchy(0, 10);
  residual_sd ~ cauchy(0, 10);
}
'

## Fit
stan_run_2 = stan(data = list(N = nrow(earnings), 
                              y = earnings$y,
                              x = earnings$x_centered,
                              eth = earnings$eth,
                              N_eth = length(unique(earnings$eth))),
                  model_code = stan_code_mm)

## Print and plot
print(stan_run_2)
plot(stan_run_2)

## Look at the posterior
post = as.data.frame(stan_run_2)
head(post)

## Second mixed model
stan_code_3 = '
data {
  int N;
  int N_pred;
  vector[N] x;
  vector[N] y;
  vector[N_pred] x_pred;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> residual_sd;
} 
model {
  // Likelihood
  y ~ normal(intercept + slope * x, residual_sd);
  // Priors
  intercept ~ normal(0, 100);
  slope ~ normal(0, 100);
  residual_sd ~ uniform(0, 100);
}
generated quantities {
  vector[N_pred] y_pred;
  for (j in 1:N_pred) 
    y_pred[j] = intercept + slope * x_pred[j];
}
'

## Fit new model
stan_run_3 = stan(data = list(N = nrow(earnings), 
                              N_pred = 5,
                              y = earnings$y,
                              x = earnings$x_centered,
                              x_pred = seq(-3,3, length = 5)),
                  model_code = stan_code_3,
                  control = list(adapt_delta = 0.9))

## Look at posterior
preds = extract(stan_run_3, 'y_pred')
head(preds$y_pred)

## Posterior predictive version
stan_code_4 = '
data {
  int N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> residual_sd;
} 
model {
  // Likelihood
  y ~ normal(intercept + slope * x, residual_sd);
  // Priors
  intercept ~ normal(0, 100);
  slope ~ normal(0, 100);
  residual_sd ~ uniform(0, 100);
}
generated quantities {
  vector[N] y_pred;
  for (j in 1:N) 
    y_pred[j] = normal_rng(intercept + slope * x[j], residual_sd);
}
'

## Over-dispered Poisson
stan_code_od_pois = '
data {
  int<lower=0> N;
  int<lower=0> N_trt;
  int<lower=0> y[N];
  int trt[N];
}
parameters {
  real beta_trt[N_trt];
  real trt_mean;
  real<lower=0> trt_sd;
}
model {
  for (i in 1:N) 
    y[i] ~ poisson_log(beta_trt[trt[i]]);
  
  // Priors on coefficients
  for(j in 1:N_trt)
    beta_trt[j] ~ normal(trt_mean, trt_sd);
  
  trt_mean ~ normal(0, 10);
  trt_sd ~ cauchy(0, 5);
}
'

## Fit stan_lmer to prostate data
library(loo)
prostate = read.csv('../data/prostate.csv')
mod_1 = stan_lmer(lpsa ~ lcavol+ ( 1| gleason), data = prostate)
mod_2 = stan_lmer(lpsa ~ (lcavol | gleason), data = prostate)

## Look at loo
loo_1 = loo(mod_1)
loo_2 = loo(mod_2)
compare_models(loo_1, loo_2)

## Look at waic
library(loo)
prostate = read.csv('../data/prostate.csv')
waic_1 = waic(mod_1)
waic_2 = waic(mod_2)
compare_models(waic_1, waic_2)

# Class 11 ----------------------------------------------------------------

## ---- eval = FALSE-------------------------------------------------------
##   '
##   ...
##   y ~ normal(intercept + slope * x, residual_sd);
##   ...
##   intercept ~ normal(0, 100);
##   slope ~ normal(0, 100);
##   residual_sd ~ cauchy(0, 10);
##   '

## Stupid code with extra layers
stan_code = '
data {
  int N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> residual_sd;
  real mu_intercept;
  real mu_slope;
  real sd_intercept;
  real sd_slope;
} 
model {
  // Likelihood
  y ~ normal(intercept + slope * x, residual_sd);
  // Priors
  intercept ~ normal(mu_intercept, sd_intercept);
  slope ~ normal(mu_slope, sd_slope);
  residual_sd ~ cauchy(0, 10);
  mu_intercept ~ normal(0, 2);
  mu_slope ~ normal(0, 2);
  sd_intercept ~ cauchy(0, 1);
  sd_slope ~ cauchy(0, 1);
}
'

## Fit the stupid model to the earnings data
earnings = read.csv('../data/earnings.csv')
stan_run = stan(data = list(N = nrow(earnings), 
                            y = earnings$y,
                            x = earnings$x_centered),
                model_code = stan_code)

## Plot run
plot(stan_run)

## Create fitted values plot
dat = read.csv('../data/earnings.csv')
par(mfrow=c(1,2))
plot(jitter(dat$height_cm), dat$earn, xlab = 'Height (cm)', ylab = 'Earnings ($)')
plot(jitter(dat$height_cm), log(dat$earn), xlab = 'Height (cm)', ylab = 'log(Earnings ($))')
par(mfrow=c(1,1))

## back to linear regression 
stan_code_2 = '
data {
  int N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> residual_sd;
}
model {
  // Likelihood
  y ~ normal(intercept + slope * x, residual_sd);
  // Priors
  intercept ~ normal(0, 100);
  slope ~ normal(0, 100);
  residual_sd ~ cauchy(0, 10);
}
'
stan_run_2 = stan(data = list(N = nrow(earnings), 
                              y = earnings$y,
                              x = earnings$x_centered),
                  model_code = stan_code_2)

## Plot output
plot(stan_run_2)

## Summarise it
stan_run_2_summ = summary(stan_run_2)
round(stan_run_2_summ$summary, 2)

## Extract slope and residual sd
slope = stan_run_2_summ$summary['slope',1]
sig = stan_run_2_summ$summary['residual_sd',1]

## Next model with random intercepts
stan_code_3 = '
data {
  int N;
  int N_eth;
  vector[N] x;
  vector[N] y;
  int eth[N];
}
parameters {
  real intercept[N_eth];
  real slope;
  real mean_intercept;
  real<lower=0> residual_sd;
  real<lower=0> sigma_intercept;
} 
model {
  // Likelihood
  for (i in 1:N) {
    y[i] ~ normal(intercept[eth[i]] + slope * x[i], residual_sd);
  }
  // Priors
  slope ~ normal(0, 0.1);
  for (j in 1:N_eth) {
    intercept[j] ~ normal(mean_intercept, sigma_intercept);
  }
  mean_intercept ~ normal(11, 2);
  sigma_intercept ~ cauchy(0, 10);
  residual_sd ~ cauchy(0, 10);
}
'

## Fit the model
stan_run_3 = stan(data = list(N = nrow(earnings), 
                              y = earnings$y,
                              x = earnings$x_centered,
                              eth = earnings$eth,
                              N_eth = length(unique(earnings$eth))),
                  model_code = stan_code_3)

## Plot the model
plot(stan_run_3)

## Summarise the model
stan_run_3_summ = summary(stan_run_3)
round(stan_run_3_summ$summary, 2)

## Extract useful parts
slope = stan_run_3_summ$summary['slope',1]
sig = stan_run_3_summ$summary['residual_sd',1]

## Next model with random intercepts and slopes
stan_code_4 = '
data {
  int N;
  int N_eth;
  vector[N] x;
  vector[N] y;
  int eth[N];
}
parameters {
  real intercept[N_eth];
  real slope[N_eth];
  real<lower=0> residual_sd;
  real mean_intercept;
  real<lower=0> sigma_intercept;
  real mean_slope;
  real<lower=0> sigma_slope;
} 
model {
  // Likelihood
  for (i in 1:N) {
    y[i] ~ normal(intercept[eth[i]] + slope[eth[i]] * x[i], residual_sd);
  }
  // Priors
  for (j in 1:N_eth) {
    intercept[j] ~ normal(mean_intercept, sigma_intercept);
    slope[j] ~ normal(mean_slope, sigma_slope);
  }
  mean_intercept ~ normal(11, 2);
  sigma_intercept ~ cauchy(0, 10);
  mean_slope ~ normal(0, 1);
  sigma_slope ~ cauchy(0, 10);
  residual_sd ~ cauchy(0, 10);
}
'

## Fit random intercept/slope model
stan_run_4 = stan(data = list(N = nrow(earnings), 
                              y = earnings$y,
                              x = earnings$x_centered,
                              eth = earnings$eth,
                              N_eth = length(unique(earnings$eth))),
                  model_code = stan_code_4)

## Plot sumarrise
plot(stan_run_4)
stan_run_4_summ = summary(stan_run_4)
round(stan_run_4_summ$summary, 2)

## Model with double random effects
stan_code_5 = '
data {
  int N;
  int N_eth;
  int N_age;
  vector[N] x;
  vector[N] y;
  int eth[N];
  int age[N];
}
parameters {
  matrix[N_eth, N_age] intercept;
  matrix[N_eth, N_age] slope;
  real<lower=0> residual_sd;
  real mean_intercept;
  real<lower=0> sigma_intercept;
  real mean_slope;
  real<lower=0> sigma_slope;
} 
model {
  // Likelihood
  for (i in 1:N) {
    y[i] ~ normal(intercept[eth[i], age[i]] + slope[eth[i], age[i]] * x[i], residual_sd);
  }
  // Priors
  for (j in 1:N_eth) {
    for (k in 1:N_age) {
      intercept[j,k] ~ normal(mean_intercept, sigma_intercept);
      slope[j,k] ~ normal(mean_slope, sigma_slope);
    }
  }
  mean_intercept ~ normal(11, 2);
  sigma_intercept ~ cauchy(0, 10);
  mean_slope ~ normal(0, 1);
  sigma_slope ~ cauchy(0, 10);
  residual_sd ~ cauchy(0, 10);
}
'

## Fit the model
stan_run_5 = stan(data = list(N = nrow(earnings), 
                              y = earnings$y,
                              x = earnings$x_centered,
                              eth = earnings$eth,
                              age = earnings$age,
                              N_eth = length(unique(earnings$eth)),
                              N_age = length(unique(earnings$age))),
                  model_code = stan_code_5)

## Plot and summarise
plot(stan_run_5)
stan_run_5_summ = summary(stan_run_5)
round(stan_run_5_summ$summary, 2)

## Create a better plot
par_means = colMeans(as.matrix(stan_run_5))
par_names = names(par_means)
intercept_means = matrix(par_means[grep('intercept\\[', par_names)], 
                         4, 3)
slope_means = matrix(par_means[grep('slope\\[', par_names)], 
                     4, 3)
age_grp_names = c('18-34','35-49','50-64')
eth_names = c('Blacks','Hispanics','Whites','Others')
par(mfrow=c(4,3))
for(i in 1:4) {
  for(j in 1:3) {
    curr_dat = subset(earnings, 
                      earnings$eth == i & earnings$age == j)
    plot(curr_dat$height_cm, log(curr_dat$earn), main = paste(eth_names[i], age_grp_names[j]), ylab = 'log(earnings)', xlab = 'Height (cm)')
    lines(earnings$height_cm, intercept_means[i,j] + slope_means[i,j]*(earnings$height_cm - mean (earnings$height_cm)), col = i)    
  }
}
par(mfrow=c(1,1))

## Final model - multivariate
stan_code_6 = '
data {
  int N;
  int N_eth;
  int N_age;
  vector[N] x;
  vector[N] y;
  int eth[N];
  int age[N];
}
parameters {
  vector[2] beta[N_eth, N_age];
  real<lower=0> residual_sd;
  vector[2] mean_beta;
  cov_matrix[2] Sigma_beta;
} 
model {
  // Likelihood
  for (i in 1:N) {
    y[i] ~ normal(beta[eth[i], age[i], 1] + beta[eth[i], age[i], 2] * x[i], residual_sd);
  }
  // Priors
  for (j in 1:N_eth) {
    for (k in 1:N_age) {
      beta[j,k] ~ multi_normal(mean_beta, Sigma_beta);
    }
  }
  for (l in 1:2) {
    mean_beta[l] ~ normal(0, 10);
  }
  residual_sd ~ cauchy(0, 10);
}
'

## Fit the model if you've got a while to wait!
stan_run_6 = stan(data = list(N = nrow(earnings),
                              y = earnings$y,
                              x = earnings$x_centered,
                              eth = earnings$eth,
                              age = earnings$age,
                              N_eth = length(unique(earnings$eth)),
                              N_age = length(unique(earnings$age))),
                model_code = stan_code_6,
                control = list(adapt_delta = 0.9))
plot(stan_run_6)

## Demonstrate loo model
stan_code_loo = '
data {
  int N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> residual_sd;
} 
model {
  // Likelihood
  y ~ normal(intercept + slope * x, residual_sd);
  // Priors
  intercept ~ normal(0, 20);
  slope ~ normal(0, 1);
  residual_sd ~ cauchy(0, 10);
}
generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | intercept + slope * x[i], residual_sd);
  }
}
'

## Run the loo model
stan_run_loo = stan(data = list(N = nrow(earnings), 
                                y = earnings$y,
                                x = earnings$x_centered),
                    model_code = stan_code_loo)
log_lik <- extract_log_lik(stan_run_loo)

## Get loo
loo(log_lik)
# or waic
waic(log_lik)

# Class 12 ----------------------------------------------------------------

## Load in whitefly data
wf = read.csv('../data/whitefly.csv')
head(wf)

## Plot it
barplot(table(wf$imm), 
        main = 'Number of immature whiteflies')

## First model
stan_code = '
data {
  int<lower=0> N;
  int<lower=0> N_trt;
  int<lower=0> y[N];
  int trt[N];
}
parameters {
  real beta_trt[N_trt];
  real trt_mean;
  real<lower=0> trt_sd;
}
model {
  for (i in 1:N) 
  y[i] ~ poisson_log(beta_trt[trt[i]]);
  
  // Priors on coefficients
  for(j in 1:N_trt)
    beta_trt[j] ~ normal(trt_mean, trt_sd);
  
  trt_mean ~ normal(0, 10);
  trt_sd ~ cauchy(0, 5);
}
'

## Fit it
stan_run = stan(data = list(N = nrow(wf), 
                            N_trt = length(unique(wf$trt)),
                            y = wf$imm,
                            trt = wf$trt),
                model_code = stan_code)

## Plot output
plot(stan_run)

## Fitted values and posterior predictive
pars = extract(stan_run, pars = 'beta_trt')$beta_trt
beta_means = apply(pars,2,'mean')
y_sim_mean = exp(beta_means[wf$trt])
y_sim = rpois(nrow(wf), y_sim_mean)
hist(wf$imm, breaks = seq(0,max(wf$imm)))
hist(y_sim, breaks = seq(0,max(wf$imm)), 
     add = TRUE, col = 'gray')

## Poisson OD model
stan_code = '
data {
  int<lower=0> N;
  int<lower=0> N_trt;
  int<lower=0> y[N];
  int trt[N];    
}
parameters {
  real<lower=0, upper=1> q_0;
  real beta_trt[N_trt];
  real trt_mean;
  real<lower=0> trt_sd;
}
model {
  for(j in 1:N_trt)
    beta_trt[j] ~ normal(trt_mean, trt_sd);
  trt_mean ~ normal(0, 10);
  trt_sd ~ cauchy(0, 5);
  
  for (i in 1:N) {
    if (y[i] == 0)
      target += log_sum_exp(bernoulli_lpmf(1 | q_0),
bernoulli_lpmf(0 | q_0)
  + poisson_log_lpmf(y[i] | beta_trt[trt[i]]));
    else
      target += bernoulli_lpmf(0 | q_0) + poisson_log_lpmf(y[i] | beta_trt[trt[i]]);
  }
}
'

## Stan run
stan_run = stan(data = list(N = nrow(wf), 
                            N_trt = length(unique(wf$trt)),
                            y = wf$imm,
                            trt = wf$trt),
                model_code = stan_code)

## Plot
plot(stan_run)

## Posterior predictive
beta_means = apply(extract(stan_run, 
                           pars = 'beta_trt')$beta_trt,
                   2,'mean')
q_0_mean = mean(extract(stan_run, pars = 'q_0')$q_0)
y_sim_mean = exp(beta_means[wf$trt])
rZIP = function(mean, q_0) {
  pois = rpois(length(mean), mean)
  pois[runif(length(mean))<q_0] = 0
  return(pois)
}
y_sim = rZIP(y_sim_mean, q_0_mean)

## plot the PP
hist(wf$imm, breaks = seq(0,max(wf$imm)))
hist(y_sim, breaks = seq(0,max(wf$imm)), 
     add = TRUE, col = rgb(0.75,0.75,0.75,0.4))

## Hurdle model
stan_code = '
data {
  int<lower=0> N;
  int<lower=0> N_trt;
  int<lower=0> y[N];
  int trt[N];    
}
parameters {
  real<lower=0, upper=1> q_0;
  real beta_trt[N_trt];
  real trt_mean;
  real<lower=0> trt_sd;
}
model {
  for(j in 1:N_trt)
    beta_trt[j] ~ normal(trt_mean, trt_sd);
  trt_mean ~ normal(0, 10);
  trt_sd ~ cauchy(0, 5);
  
  for (i in 1:N) {
    if (y[i] == 0)
      target += log(q_0);
    else
      target += log1m(q_0) + poisson_log_lpmf(y[i] | beta_trt[trt[i]])
    - poisson_lccdf(0 | exp(beta_trt[trt[i]]));
  }
}
'

## Fit the hurdle
stan_run = stan(data = list(N = nrow(wf), 
                            N_trt = length(unique(wf$trt)),
                            y = wf$imm,
                            trt = wf$trt),
                model_code = stan_code)

## Look at output
plot(stan_run)

## Create posterior predictive
beta_means = apply(extract(stan_run, 
                           pars = 'beta_trt')$beta_trt,
                   2,'mean')
q_0_mean = mean(extract(stan_run, pars = 'q_0')$q_0)
y_sim_mean = exp(beta_means[wf$trt])
rZIP = function(mean, q_0) {
  pois = rpois(length(mean), mean)
  pois[runif(length(mean))<q_0] = 0
  return(pois)
}
y_sim = rZIP(y_sim_mean, q_0_mean)

## Plot it out
hist(wf$imm, breaks = seq(0,max(wf$imm)))
hist(y_sim, breaks = seq(0,max(wf$imm)), 
     add = TRUE, col = rgb(0.75,0.75,0.75,0.4))

## Multinomial model
pollen = read.csv('../data/pollen.csv')
head(pollen)

## Plot the data
N = rowSums(pollen[,3:ncol(pollen)])
par(mfrow=c(1,2))
plot(pollen$GDD5, pollen$Pinus.D)
plot(pollen$GDD5, pollen$Pinus.D/N)
par(mfrow=c(1,1))

## Multinomial code
stan_code = '
data {
  int<lower=1> n;
  int<lower=1> K;
  int<lower=0> y[n,K];
  real x1[n];
  real x2[n];
}
parameters {
  vector[K] alpha; 
  vector[K] beta;
  vector[K] gamma;
  real alpha_mean;
  real beta_mean;
  real gamma_mean;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_gamma;
}
transformed parameters {
  vector[K] theta[n];
  simplex[K] p[n]; 
  
  for(i in 1:n){
    theta[i] =  alpha + beta*x1[i] + gamma*x2[i];
  }
  for(i in 1:n){
    p[i] = softmax(theta[i]);
  }
}
model {
  for(k in 1:K) {
    alpha[k] ~ normal(alpha_mean,sigma_alpha);
    beta[k] ~ normal(beta_mean,sigma_beta);
    gamma[k] ~ normal(gamma_mean,sigma_gamma);
  }
  alpha_mean ~ normal(0, 10);
  beta_mean ~ normal(0, 10);
  gamma_mean ~ normal(0, 10);
  sigma_alpha ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);
  
  for(i in 1:n)
    y[i] ~ multinomial(p[i]);
}
'

## Fit the multinomial model but not a great model
# pollen2 = pollen[sample(1:nrow(pollen), 200),]
# stan_run = stan(data = list(K = 3,
#                             n = nrow(pollen2), 
#                             y = pollen2[,3:5],
#                             x1 = scale(pollen2$GDD5)[,1],
#                             x2  = scale(pollen2$MTCO)[,1]),
#                 model_code = stan_code)
