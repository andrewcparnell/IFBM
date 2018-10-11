# Create posterior confidence intervals for different groups from a regression model

# Starting code
rm(list = ls())
library(rstanarm)
library(ggplot2)

# Use the earnings data
earnings = read.csv('https://raw.githubusercontent.com/andrewcparnell/IFBM/master/data/earnings.csv')

# Run a model through stan_lmer
mod = stan_lmer(y ~ x_centered + (1 | age), 
                data = earnings)

# Plot the output
plot(mod)
plot(mod, pars = 'b')

# Extract the posterior
posterior = as.data.frame(mod)

# Create a new data frame with better x values in it
x_pred = with(earnings, 
              rep(seq(min(x_centered), 
                      max(x_centered),
                      length = 100), 3))
age_pred = rep(1:3, each = 100)
nd = data.frame(x_centered = x_pred,
                age = age_pred)

# Get predictions
y_pred = posterior_linpred(mod, 
                           newdata = nd, 
                           draws = 1000)
y_pred_med = apply(y_pred, 2, quantile, p = 0.5)
y_pred_low = apply(y_pred, 2, quantile, p = 0.975)
y_pred_high = apply(y_pred, 2, quantile, p = 0.025)

df = data.frame(x = x_pred,
                age = age_pred,
                y = y_pred_med,
                y_low = y_pred_low,
                y_high = y_pred_high)

# Create a nice plot
ggplot(earnings, aes(x = x_centered, y = y,
                     col = as.factor(age))) +
  labs(x = 'Height in cm (centered)',
       y = 'log(earnings) ($)') + 
  geom_point() + 
  theme(legend.position = 'None') +
  facet_grid(age ~ .) + 
  geom_line(data = df, aes(x = x, y = y)) + 
  geom_line(data = df, aes(x = x, y = y_low), 
            linetype = 2) + 
  geom_line(data = df, aes(x = x, y = y_high), 
                           linetype = 2)
