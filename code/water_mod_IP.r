library(raster)
library(sf)

library(rstanarm)


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

## Everything
#model1 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TPI + TWI + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"))

## no elevation, everything else -> why is Hydric not included in model output?
#mod <- stan_glm(GSW ~ Aspect + Slope + TPI + TWI + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit")) #10 min

#Sys.time()
## Everything - Dropping TWI and TPI -> SLOPE Positive?
#anand.mod <- stan_glm(GSW ~ Elevation + Aspect + Slope + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"))
#plot(anand.mod, pars = 'beta')
#Sys.time()
 
## swapping slope and TPI per anand's request - NOT GREAT
#anand.mod2 <- stan_glm(GSW ~ Elevation + Aspect + TPI + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"))
#plot(anand.mod2, pars = 'beta')
#Sys.time()

## Dropping Elevation, aspect, precip per beta plot from anand.mod; but now temp param is positive?
#another.mod <- stan_glm(GSW ~ Slope + Hydric + Temp, data = df.sbst, family = "binomial"(link = "logit"))
#plot(another.mod, pars = "beta")
#Sys.time()

## Dropping elev, aspect, tpi, and precip (all possible 0 effects) - Weird -> slope positive!
#nick.mod <- stan_glm(GSW ~ Slope + TWI + Hydric + Temp ,data = df.sbst, family = "binomial" (link =  "logit")) 
#plot(nick.mod, pars = 'beta')
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
plot(model, pars = 'beta')
plot(asp.mod, pars = 'beta')
plot(slope.mod, pars = 'beta')
plot(tpi.mod, pars = 'beta')
plot(twi.mod, pars = 'beta')
plot(hydric.mod, pars = 'beta')
plot(temp.mod, pars = 'beta')
plot(precip.mod, pars = 'beta')

## everything 9:24
model1 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TPI + TWI + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"), QR =T )
Sys.time()
