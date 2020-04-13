# Resources; https://ourcodingclub.github.io/2018/04/17/stan-intro.html

#------------------------------------------------------------------
#load libraries
library(rstan)
library(gdata)
library(bayesplot)

#------------------------------------------------------------------

# simulate some data


b0 <- 2.3
b1 <- 1.5

x_1 <- rnorm(100,4,3)
y_1 <- rnorm(100, b0 + x_1*b1,1)

lm1 <- lm(y_1~x_1)
lm1

#------------------------------------------------------------------

# prepare data for stan model
x <- I(x_1)
y <- y_1
N <- length(y_1)

stan_data <- list(N = N, x = x, y = y)

#------------------------------------------------------------------

# writing out a stan model

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

      "stan_code/stan_model1.stan")


# review stan model
stanc("stan_code/stan_model1.stan")
stan_model1 <- "stan_code/stan_model1.stan"
#------------------------------------------------------------------

# fit stan model

fit <- stan(file = stan_model1, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)

# check r-hat statistics for conversion
fit

posterior <- extract(fit)
head(posterior)

plot(posterior$alpha, type = "l")
plot(posterior$beta, type = "l")
plot(posterior$sigma, type = "l")

plot(density(posterior$alpha))
abline(v=mean(posterior$alpha))
