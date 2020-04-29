library(raster)
library(sf)
library(rstanarm)
library(xtable)


setwd("C:/Users/nicholaskolarik/DDeSuP/")


s.sbst <- raster::stack( "./data/2017_IP_stack.tif")
names(s.sbst) <- c('GSW', 'Elevation', 'Aspect', 'Slope', 'TPI', 'TWI', 'Hydric', 'Temp', 'Precip')
plot(s.sbst)

## visualizing colinear variables
pairs(s.sbst)

## convert to points to get (x,y) for each pixel
points <- rasterToPoints(s.sbst)
##convert to df
df.sbst <- as.data.frame(points)
## NA to 0 in hydric layer
df.sbst$Hydric[is.na(df.sbst$Hydric)] <- 0

## scaling only certain columns (omitting GSW and hydric (binary))
## https://stackoverflow.com/questions/49924695/scale-only-certain-columns-r
df.sbst[c(4, 5, 6, 7, 8, 10, 11)] <- lapply(df.sbst[c(4, 5, 6, 7, 8, 10, 11)], function(x) c(scale(x)))


## everything 
model1 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TPI + TWI + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()


## dropping hydric only
model2 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TPI + TWI + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()

## dropping hydric, tpi
model3 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TWI + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()

## dropping hydric, TPI, and correlated -> slope, temp, precip
model4 <- stan_glm(GSW ~ Elevation + Aspect + TWI ,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()

## checking fit
mod1_r2 <- median(bayes_R2(model1, df.sbst$GSW))
mod2_r2 <- median(bayes_R2(model2, df.sbst$GSW))
mod3_r2 <- median(bayes_R2(model3, df.sbst$GSW))
mod4_r2 <- median(bayes_R2(model4, df.sbst$GSW))


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
