# All the code from day 2 of IFBM course

# Boiler plate code
rm(list = ls())
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01, las=1)

# set the working directory - change this as appropriate - needs to be the slides folder
setwd('~/GitHub/IFBM/Slides')

# Call in some packages
library(lattice)
library(lme4)

# Start code --------------------------------------------------------------

## Load in the earnings data and plot
dat = read.csv('../data/earnings.csv')
with(dat, plot(jitter(x_centered, 1), 
               y, xlab = 'Height (centered)',
               ylab = 'log(Earnings)', pch = 19))

## Fit a linear regression
summary(lm(y ~ x_centered, data = dat))

## Plot the output
with(dat, plot(jitter(x_centered, 1), y, xlab = 'Height (centered)',
               ylab = 'log(Earnings)', pch = 19))
lines(dat$x_centered, lm(y ~ x_centered, 
                         data = dat)$fitted.values)

## Create a better plot
eth_names = c('Blacks','Hispanics','Whites','Others')
with(dat, plot(x_centered, y, xlab = 'Height (centered)',
               ylab = 'log(Earnings)', type = 'n'))
with(dat, points(jitter(x_centered, 2), y, col = eth, 
                 pch = 19))
legend('bottomright', eth_names, col = 1:4, pch = 19)

## Try a fixed effects model
summary(lm(y ~ x_centered + as.factor(eth), data = dat))

## First mixed model!
mm_1 = lmer(y ~ x_centered + (1 | eth), data = dat)
summary(mm_1)

## Look at coefficients
coef(mm_1)

## Plot residuals
plot(mm_1)
qqnorm(residuals(mm_1))
qqline(residuals(mm_1))

## Confidence intervals
confint(mm_1, level = 0.5)

## Plot of random effects
dotplot(ranef(mm_1))

## Better plot
fitted_values = predict(mm_1)
with(dat, plot(x_centered, y, xlab = 'Height (centered)',
               ylab = 'log(Earnings)', type = 'n'))
legend('bottomright', eth_names, col = 1:4, pch = 19)
with(dat, points(x_centered, fitted_values, col = eth, pch = 19))

## Change the optimiser
mm_2 = lmer(y ~ x_centered + (x_centered | eth), 
            data = dat, 
            control = lmerControl(optimizer = "Nelder_Mead"))
summary(mm_2)

## Plot new model and get coefficients
plot(mm_2)
coef(mm_2)

## New model with differing coefficients
mm_3 = lmer(y ~ 1 + (x_centered - 1 | eth), data = dat, 
            control = lmerControl(optimizer = "Nelder_Mead"))
summary(mm_3)

## Compare models
anova(mm_1, mm_2, mm_3)

## Model with two random effects
mm_4 = lmer(y ~ x_centered + (1 | eth) + (1 | age), data = dat, 
            control = lmerControl(optimizer = "Nelder_Mead"))
summary(mm_4)

## Compare all these models
anova(mm_1, mm_3, mm_4)

## ------------------------------------------------------------------------
swt = read.csv('../data/swt.csv')
head(swt)

## Function to sum up rep.1, rep.2, and rep.3
sum_fun = function(x) {
  s = ifelse(is.na(x[1]),0,x[1]) + ifelse(is.na(x[2]),0,x[2]) + ifelse(is.na(x[3]),0,x[3])
  N = ifelse(is.na(x[1]),0,1) + ifelse(is.na(x[2]),0,1) + ifelse(is.na(x[3]),0,1)
  return(c(s,N))
}
y = apply(swt[,1:3],1,sum_fun)[1,]
N = apply(swt[,1:3],1,sum_fun)[2,]
x = swt$forest - mean(swt$forest)

## Fit glm
summary(glm(cbind(y, N) ~ x, family = binomial(link = logit)))

## Show lattice plot
xyplot(y/N ~ x|alt, swt, type='p',
       layout=c(1,3), index.cond = function(x,y)max(y))

## Fit first binomial glmm
glmm_1 = glmer(cbind(y, N) ~ x + (1 | alt),
               family = binomial, data = swt)
summary(glmm_1)

## ---- fig.height = 6-----------------------------------------------------
dotplot(ranef(glmm_1))

## ---- fig.height = 6-----------------------------------------------------
p_est = predict(glmm_1, type = 'response')
plot(x, y/N, col = swt$alt, las = 1)
points(x, p_est, col = swt$alt, pch = 19)
legend('topleft', c('high', 'low', 'medium'), pch = 19, col=1:3)

## ------------------------------------------------------------------------
glmm_2 = glmer(cbind(y, N) ~ x + (x | alt),
               family = binomial, data = swt)
summary(glmm_2)

## ---- fig.height = 6-----------------------------------------------------
dotplot(ranef(glmm_2))

## ------------------------------------------------------------------------
anova(glmm_1, glmm_2)

## ---- fig.height = 6-----------------------------------------------------

horseshoe = read.csv('../data/horseshoe.csv')
horseshoe$weight_corr = horseshoe$weight - mean(horseshoe$weight)
colnames(horsehoe)[1] = 'color'
xyplot(satell ~ weight_corr|as.factor(color), horseshoe, 
       type='p',layout=c(1,4))

## ------------------------------------------------------------------------
summary(glm(satell ~ weight_corr, data = horseshoe,family = poisson(link = log)))

## ------------------------------------------------------------------------
glmm_3 = glmer(satell ~ weight_corr + (1 | color),
               family = poisson, data = horseshoe)
summary(glmm_3)

## ------------------------------------------------------------------------
horseshoe$obs <- 1:nrow(horseshoe)
glmm_4 = glmer(satell ~ weight + (1 | obs),
               family = poisson, data = horseshoe,
               control = glmerControl(optimizer = "Nelder_Mead"))
summary(glmm_4)

## ------------------------------------------------------------------------
anova(glmm_3, glmm_4)


