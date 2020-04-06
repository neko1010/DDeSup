library(raster)
library(sf)

library(rstanarm)

## Using the stack output from rsmpl_stack.r to model surface water occurrences. See rsmpl_stack.r for any questions on how data were stacked

setwd("C:/Users/nicholaskolarik/DDeSuP/")

## Load the stack
#s <- raster::stack("./data/2017_stack.tif")

## Pairs plots to check for colinear vars
#pairs(na.omit(s))

## ELEVATION AND T AND PRECIP are highly correlated- as to be expected by how PRISM data are derived

## NEED TO SUBSET

## bounds for subset - 1/100 of full area
#sbst <- s@extent/10
#s.sbst <- crop(s, sbst)

## Load the SUBSET
s.sbst <- raster::stack("./data/2017_sbst.tif")
names(s.sbst) <- c('GSW', 'Elevation', 'Aspect', 'Slope', 'TPI', 'TWI', 'Hydric', 'Temp', 'Precip')
plot(s.sbst)
df.sbst <- na.omit(as.data.frame(s.sbst))

##Elevation ony
model <- stan_glm(GSW~Elevation, data = df.sbst, family = "binomial" (link =  "logit"))

## Everything
model1 <- stan_glm(GSW ~ Elevation + Aspect + Slope + TPI + TWI + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit"))

## no elevation, everything else -> why is Hydric not included in model output?
mod <- stan_glm(GSW ~ Aspect + Slope + TPI + TWI + Hydric + Temp + Precip,data = df.sbst, family = "binomial" (link =  "logit")) #10 min


