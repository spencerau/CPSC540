library(tidyverse)
library(brms)
library(tidybayes)
library(betareg)

# Simulate data for linear regression
set.seed(540)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 5 + 2 * x1 - 1.25 * x2 + rnorm(n)

df <- data.frame(x1,x2,y)

# Linear regression using lm/glm (Frequentist)
linear_lm <- lm(y ~ x1 + x2, 
                data = df)
# linear_glm <- glm(y ~ x1 + x2,
#                   family = gaussian(), data = df)

summary(linear_lm)

# Linear regression using lm/glm (Frequentist)
linear_lm <- lm(y ~ x1 * x2, # x1 + x2 + interaction(x1, x2)
                data = df)
linear_lm2 <- lm(y ~ x1 + x2 + x1:x2, 
                 data = df)

summary(linear_lm)

# Linear regression using brm (Bayesian)
linear_brm <- brm(y ~ x1 + x2, family = gaussian(), data = df)
summary(linear_brm)


# Simulate some data for logistic regression
set.seed(540)
n <- 100
x1 <- rnorm(n)
prob <- 1 / (1 + exp(-1 - 2 * x1 + -0.2 * x2))  # Logit link
y <- rbinom(n, size = 1, prob = prob)

df <- data.frame(y,x1,x2)

# Logistic regression using glm (Frequentist)
logistic_glm <- glm(y ~ x1 + x2, family = binomial(), data = df)
summary(logistic_glm)

# Logistic regression using brm (Bayesian)
logistic_brm <- brm(y ~ x1 + x2, family = bernoulli(), data = df)
summary(logistic_brm)


# Simulate some data for Poisson regression
set.seed(540)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
lambda <- exp(1 + 0.5 * x1 -0.25 * x2)  # Poisson rate (log link)
y <- rpois(n, lambda)

df <- data.frame(x1,x2,y)
# Poisson regression using glm (Frequentist)
poisson_glm <- glm(y ~ x1 + x2, family = poisson(), data = df)
summary(poisson_glm)