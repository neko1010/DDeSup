library(raster)
library(sf)
library(rstanarm)
library(xtable)


setwd("C:/Users/nicholaskolarik/DDeSuP/")

## Load the stack
s <- raster::stack("./data/2017_stack.tif")
names(s) <- c('GSW', 'Elevation', 'Aspect', 'Slope', 'TPI', 'TWI', 'Hydric', 'Temp', 'Precip')
#plot(s)


## Forcing Island Park reservoir
bb <- extent(2250000, 2350000, 16100000, 16200000)
s.sbst <- crop(s, bb)
plot(s.sbst)

writeRaster(s.sbst, "./data/2017_IP_stack.tif")

## convert to points to get (x,y) for each pixel
points <- rasterToPoints(s.sbst)
##convert to df
df.sbst <- as.data.frame(points)
## NA to 0 in hydric layer
df.sbst$Hydric[is.na(df.sbst$Hydric)] <- 0

## scaling only certain columns (omitting GSW and hydric (binary))
## https://stackoverflow.com/questions/49924695/scale-only-certain-columns-r
df.sbst[c(4, 5, 6, 7, 8, 10, 11)] <- lapply(df.sbst[c(4, 5, 6, 7, 8, 10, 11)], function(x) c(scale(x)))

Sys.time()

## ALL VARS INDIVIDUALLY
##Elevation only see above "model"
elev.mod <- stan_glm(GSW~Elevation, data = df.sbst, family = "binomial" (link =  "logit"))
Sys.time()
##Aspect
asp.mod <- stan_glm(GSW~Aspect, data = df.sbst, family = "binomial" (link =  "logit"))
Sys.time()
##Slope
slope.mod <- stan_glm(GSW~Slope, data = df.sbst, family = "binomial" (link =  "logit"))
Sys.time()
##TPI
tpi.mod <- stan_glm(GSW~TPI, data = df.sbst, family = "binomial" (link =  "logit"))
Sys.time()
##TWI
twi.mod <- stan_glm(GSW~TWI, data = df.sbst, family = "binomial" (link =  "logit"))
Sys.time()
##Hydric
hydric.mod <- stan_glm(GSW~Hydric, data = df.sbst, family = "binomial" (link =  "logit"))
Sys.time()
##Temp
temp.mod <- stan_glm(GSW~Temp, data = df.sbst, family = "binomial" (link =  "logit"))
Sys.time()
##Precip
precip.mod <- stan_glm(GSW~Precip, data = df.sbst, family = "binomial" (link =  "logit"))

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

##BROKEN
### list of models to calculate fit metrics
#models <- c(elev.mod, asp.mod, slope.mod, tpi.mod, twi.mod, hydric.mod, temp.mod, precip.mod)
#
### column names as strings
#vars <- names(df.sbst)[3:11]
#
### Loop through them and save fit objects
#for (model in models){
#  
#  yhat <- posterior_predict(model) ## posterior predict draws predictions from the posterior
#  yhat <- apply(yhat, 2, median) ## identifies median prediction from the modeli
#  
#  ## index starts at 2 
#  i<- 2 
#  ##assign function to same each object 
#  assign(paste0(vars[i], "_rmse"), rmse(df.sbst$GSW, yhat))
#  assign(paste0(vars[i], "_mae"), mae(df.sbst$GSW, yhat))
#  i <- i + 1
#}

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


# Elevation
elev.yhat <- posterior_predict(elev.mod) ## posterior predict draws predictions from the posterior
elev.yhat <- apply(elev.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("Elevation: ")
rmse(df.sbst$GSW,elev.yhat)
mae(df.sbst$GSW, elev.yhat) 

## concatnenate to for the xtable
rmses <- c(rmses, rmse(df.sbst$GSW,elev.yhat))
maes <- c(maes, mae(df.sbst$GSW, elev.yhat))

## Aspect
asp.yhat <- posterior_predict(asp.mod) ## posterior predict draws predictions from the posterior
asp.yhat <- apply(asp.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("Aspect: ")
rmse(df.sbst$GSW, asp.yhat)
mae(df.sbst$GSW, asp.yhat)

## concatnenate to for the xtable
rmses <- c(rmses, rmse(df.sbst$GSW,asp.yhat))
maes <- c(maes, mae(df.sbst$GSW, asp.yhat))

## Slope
slope.yhat <- posterior_predict(slope.mod) ## posterior predict draws predictions from the posterior
slope.yhat <- apply(slope.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("Slope: ")
rmse(df.sbst$GSW, slope.yhat)
mae(df.sbst$GSW, slope.yhat)

## concatnenate to for the xtable
rmses <- c(rmses, rmse(df.sbst$GSW, slope.yhat))
maes <- c(maes, mae(df.sbst$GSW, slope.yhat))

## TPI
tpi.yhat <- posterior_predict(tpi.mod) ## posterior predict draws predictions from the posterior
tpi.yhat <- apply(tpi.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("TPI: ")

rmse(df.sbst$GSW, tpi.yhat)
mae(df.sbst$GSW, tpi.yhat)

## concatnenate for the xtable
rmses <- c(rmses, rmse(df.sbst$GSW, tpi.yhat))
maes <- c(maes, mae(df.sbst$GSW, tpi.yhat))

## TWI
twi.yhat <- posterior_predict(twi.mod) ## posterior predict draws predictions from the posterior
twi.yhat <- apply(twi.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("TWI: ")
rmse(df.sbst$GSW, twi.yhat)
mae(df.sbst$GSW, twi.yhat)

## concatnenate for the xtable
rmses <- c(rmses, rmse(df.sbst$GSW, twi.yhat))
maes <- c(maes, mae(df.sbst$GSW, twi.yhat))

## Hydric
hydric.yhat <- posterior_predict(hydric.mod) ## posterior predict draws predictions from the posterior
hydric.yhat <- apply(hydric.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("Hydric: ")
rmse(df.sbst$GSW, hydric.yhat)
mae(df.sbst$GSW, hydric.yhat)

## concatnenate for the xtable
rmses <- c(rmses, rmse(df.sbst$GSW, hydric.yhat))
maes <- c(maes, mae(df.sbst$GSW, hydric.yhat))

## Temp
temp.yhat <- posterior_predict(temp.mod) ## posterior predict draws predictions from the posterior
temp.yhat <- apply(temp.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("Temp: ")
rmse(df.sbst$GSW, temp.yhat)
mae(df.sbst$GSW, temp.yhat)

## concatnenate for the xtable
rmses <- c(rmses, rmse(df.sbst$GSW, temp.yhat))
maes <- c(maes, mae(df.sbst$GSW, temp.yhat))

##Precip
precip.yhat <- posterior_predict(precip.mod) ## posterior predict draws predictions from the posterior
precip.yhat <- apply(precip.yhat, 2, median) ## identifies median prediction from the model

## Find the residual mean squared error and Mean Absolute Error (MAE) for the model's fit (in sample prediction)?
print("Precip: ")
rmse(df.sbst$GSW, precip.yhat)
mae(df.sbst$GSW, precip.yhat)

## concatnenate for the xtable
rmses <- c(rmses, rmse(df.sbst$GSW, precip.yhat))
maes <- c(maes, mae(df.sbst$GSW, precip.yhat))

## table contruction
fit.table <- data.frame(cbind(rmses,maes))
rownames(fit.table) <- vars
colnames(fit.table) <- c("RMSE", "MAE")

## assemble the df for the xtable
print.xtable(xtable(fit.table, digits = 7), type = "html", file = './docs/fit.html')

## Leave one outs
Sys.time()
loo.elev <- loo(elev.mod)
Sys.time()
loo.asp <- loo(asp.mod)
Sys.time()
loo.slp <- loo(slope.mod)
Sys.time()
loo.tpi <- loo(tpi.mod)
Sys.time()
loo.twi <- loo(twi.mod)
Sys.time()
loo.hydric <- loo(hydric.mod)
Sys.time()
loo.temp <- loo(temp.mod)
Sys.time()
loo.precip <- loo(precip.mod)
Sys.time()

compare_models(loo.elev, loo.asp, loo.slp, loo.tpi, loo.twi, loo.hydric, loo.temp, loo.precip, detail = T)
loo_list <- list(loo.elev, loo.asp, loo.slp, loo.tpi, loo.twi, loo.hydric, loo.temp, loo.precip)
loo_model_weights(loo_list)


par(mfrow=c(4,2))
plot(elev.mod, pars = 'beta')
plot(asp.mod, pars = 'beta')
plot(slope.mod, pars = 'beta')
plot(tpi.mod, pars = 'beta')
plot(twi.mod, pars = 'beta')
plot(hydric.mod, pars = 'beta')
plot(temp.mod, pars = 'beta')
plot(precip.mod, pars = 'beta')