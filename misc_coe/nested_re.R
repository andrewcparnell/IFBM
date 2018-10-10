# Simulate and fit some nest random effects data

rm(list = ls())
set.seed(123)

library(lme4)
library(lattice)

# Model is:
# y_{ijk} = alpha_j + gamma_{jk} + beta*x_{ijk} + epsilon_{ijk}
# where j = facility, k = cage, i = observation
# so alpha_j is random intercept for facility ~N(0, sigma_alpha^2)
# gamma_{jk} is random nested intercept for cage within facility ~ N(0, sigma_{j,gamma}^2)
# epsilon is the residual ~ N(0, sigma^2)

# We have J = 6 facilities, K = 4 cages, and N = 30 observation in each
N = 30
J = 6
K = 4

# Total number of observations
NN = N*J*K

# Standard deviations
sigma = 0.05 # Residual standard deviation
sigma_j_gamma = runif(J, 0.5, 1.5) # How much the cages vary within facility
sigma_alpha = runif(1, 0.5, 1.5) # Variability between facilities

# Simulate the random effects
gamma = matrix(rnorm(J*K, mean = 0, sd = sigma_j_gamma),
               ncol = K, nrow = J, byrow = FALSE)

mu_alpha = 3
beta = 3

alpha = rnorm(J, mu_alpha, sigma_alpha)

x = runif(NN)
y = rep(NA, length = NN)
dat = data.frame(y = y,
                 x = x,
                 facility = y,
                 cage = y)
count = 1
for(i in 1:N) {
  for(j in 1:J) {
    for(k in 1:K) {
      y[count] = rnorm(1, mean = alpha[j] + gamma[j,k] + beta*x[count], sigma)
      dat[count,] = c(y[count], x[count], j, k)
      count = count + 1
    }
  }
}

# Plot
xyplot(y ~ x|facility, dat, type='p',
       layout=c(1,6))
xyplot(y ~ x|cage, dat, type='p',
       layout=c(1,4))

# Fit
mod_1 = lmer(y ~ x + (1|facility), data = dat)
summary(mod_1)
mod_2 = lmer(y ~ x + (1|facility) + (1|cage), data = dat)
summary(mod_2)
mod_3 = lmer(y ~ x + (1|facility/cage), data = dat)
summary(mod_3)
ranef(mod_3)
dotplot(ranef(mod_3))

# library(rstanarm)
# mod_4 = stan_lmer(y ~ x + (1|facility/cage), data = dat)
# summary(mod_4)

# Change the data slightly to remove one facility/cage combination
dat$facility_fac = factor(dat$facility)
dat$cage_fac = factor(dat$cage)
mod_3a = lmer(y ~ x + (1|facility_fac/cage_fac), data = dat)
summary(mod_3a)

# Remove factor 1 cage 1
dat_sub = subset(dat,
                 (facility_fac != 1 | cage_fac != 1) &
                   (facility_fac != 2 | cage_fac != 1) &
                   (facility_fac != 3 | cage_fac != 1) &
                   (facility_fac != 4 | cage_fac != 1) &
                   (facility_fac != 5 | cage_fac != 1))
# dat_sub = subset(dat,
#                  facility_fac != 1)

with(dat_sub, table(facility_fac, cage_fac))

mod_4 = lmer(y ~ x + (1|facility_fac/cage_fac), data = dat_sub)
summary(mod_4)
