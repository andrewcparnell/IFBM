a = 1
b = 0.5
x = seq(-3,3,length = 100)

lambda = exp(a + b* x)

y = rpois(length(x), lambda)
y2 = rnegbin(length(x), theta = 100, mu = lambda)

plot(x, y)
plot(x, y2)

library(MASS)
mod_pois = glm(y ~ x, family = poisson())
mod_nb = glm.nb(y ~ x) # Gives error

mod_pois_2 = glm(y2 ~ x, family = poisson())
mod_nb_2 = glm.nb(y2 ~ x)
summary(mod_nb_2)
