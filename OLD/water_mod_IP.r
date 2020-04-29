library(raster)
library(sf)
library(rstanarm)
library(xtable)


setwd("C:/Users/nicholaskolarik/DDeSuP/")


s.sbst <- raster::stack( "./data/2017_IP_stack.tif")
names(s.sbst) <- c('GSW', 'Elevation', 'Aspect', 'Slope', 'TPI', 'TWI', 'Hydric', 'Temp', 'Precip')
plot(s.sbst)

## convert to points to get (x,y) for each pixel
points <- rasterToPoints(s.sbst)
##convert to df
df.sbst <- as.data.frame(points)
## NA to 0 in hydric layer
df.sbst$Hydric[is.na(df.sbst$Hydric)] <- 0

## scaling only certain columns (omitting GSW and hydric (binary))
## https://stackoverflow.com/questions/49924695/scale-only-certain-columns-r
df.sbst[c(4, 5, 6, 7, 8, 10, 11)] <- lapply(df.sbst[c(4, 5, 6, 7, 8, 10, 11)], function(x) c(scale(x)))


## Dropping all models with 0 weight; dropping hydric(7) looks like just a function of soil under water unmeasured
ip.mod <- stan_glm(GSW ~ TPI + TWI + Elevation, data = df.sbst, family = "binomial" (link = "logit") )
summary(ip.mod)

## Dropping all models with 0 weight; dropping hydric(7) looks like just a function of soil under water unmeasured
ip.mod.topo <- stan_glm(GSW ~ TPI + TWI , data = df.sbst, family = "binomial" (link = "logit") )


## Leave one outs
Sys.time()
loo.ip.mod <- loo(ip.mod)
Sys.time()
loo.topo <- loo(ip.mod.topo)
Sys.time()

loo_compare(loo.ip.mod, loo.topo)
loo_list <- list(loo.ip.mod, loo.topo)
loo_model_weights(loo_list)

## Beta plots
plot(ip.mod, pars = 'beta')
plot(ip.mod.topo, pars = 'beta')


plot(ip.mod, pars = "beta")
plot(ip.mod.topo, pars = "beta")
#plot(model1, pars = "beta")


## Assessing the model fit 
## RMSE
rmse <- function(y, ypred) {
  rmse = sqrt(mean((y - ypred)^2))
  print("RMSE: ", rmse)
  return(rmse)
}

# MAE function:
mae <- function(y, ypred) {
  mae = (mean(abs(y - ypred)))
  print("MAE: ", mae)
  return(mae)
}

vars <- names(df.sbst)[4:11]
rmses <- c()
maes <- c()

## HARD CODE IT AND GET IT DONE
## IP model
ip.mod.yhat <- posterior_predict(ip.mod) ## posterior predict draws predictions from the posterior
ip.mod.yhat <- apply(ip.mod.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("MODEL: ")
rmse(df.sbst$GSW, ip.mod.yhat)
mae(df.sbst$GSW, ip.mod.yhat)



## everything 
model1 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TPI + TWI + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()


## dropping hydric
model2 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TPI + TWI + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()

## dropping hydric, tpi
model3 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TWI + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()

## dropping hydric, TPI, and Aspect
model4 <- stan_glm(GSW ~ Elevation + Slope + TWI + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()


plot(model1, pars = 'beta')
plot(model2, pars = 'beta')
plot(model3, pars = 'beta') ## plots seem better! 
plot(model4, pars = 'beta')

loo.mod1 <- loo(model1)
loo.mod2 <- loo(model2)
loo.mod3 <- loo(model3)
loo.mod4 <- loo(model4)

mod.compare <- loo_compare(loo.mod1, loo.mod2, loo.mod3, loo.mod4, loo.ip.mod)

loo.list2 <- list(loo.mod1, loo.mod2, loo.mod3, loo.mod4, loo.ip.mod)

loo_model_weights(loo.list2)
