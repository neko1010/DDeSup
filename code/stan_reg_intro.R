library(rstan)
library(gdata)
library(bayesplot)

setwd('C:/Users/nicho/OneDrive/BSU/DDeSuP/src/bayes/intro/CC-Stan-intro-master')

## reading sea ice data - strings not set as factors (categorical)
seaice <- read.csv('../data/seaice.csv', stringsAsFactors = FALSE)

head(seaice)

## explicitly defining col names
colnames(seaice) <- c('year', 'extent_north', 'extent_south')


## RQ - Is sea ice declining over time?

plot(extent_north ~ year, pch = 20, data = seaice)

## linear model for data
lm1 <- lm(extent_north ~ year, data = seaice)
summary(lm1)
abline(lm1, col = 'red')

## TRYING WTIH STAN

## indexing the data from 1-30 beginning w/ 1978
x <- I(seaice$year - 1978)
y <- seaice$extent_north
N <- length(seaice$year)

lm2 <- lm(y~x)
summary (lm2)

## key stats
lm_alpha <- summary(lm2)$coeff[1]  # the intercept
lm_beta <- summary(lm2)$coeff[2]  # the slope
lm_sigma <- sigma(lm2)  # the residual error

## defining stan data as a list - why not a vector? does it matter?
stan_data <- list(N = N, x = x, y = y)


## writing the stan model
write("// Stan model for simple linear regression

      data {
      int < lower = 1 > N; // Sample size
      vector[N] x; // Predictor
      vector[N] y; // Outcome
      }
      
      parameters {
      real alpha; // Intercept
      real beta; // Slope (regression coefficients)
      real < lower = 0 > sigma; // Error SD
      }
      
      model {
      y ~ normal(alpha + x * beta , sigma);
      }
      
      generated quantities {
      } // The posterior predictive distribution",

      "stan_model1.stan")

stanc('stan_model1.stan')


stan_model1 <- ('stan_model1.stan')

## running the model
fit <- stan(file = stan_model1, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)

## Rhat vals indicate whether the data have converged (established stationarity) or not - if near 1, then yes
fit

## exploring the posterior
posterior <- extract(fit)
str(posterior)


## comparing with the lm object - (almost?) identical
plot(y ~ x, pch = 20)

abline(lm1, col = 1, lty = 2, lw = 3)
abline( mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)

## visualizing the variablity
plot(y ~ x, pch = 20)

for (i in 1:500) {
  abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}

abline(mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)
