library(raster)

setwd("C:/Users/nicholaskolarik/DDeSuP/")
d <- raster("./data/topo/ID_DEM_10m.tif")
plot(d)

## setting values of 0 to NA
values(d)[values(d) == 0] = NA
plot(d)

## reduce extent -> omit NA vals with trim
d.trim <- trim(d)
plot(d.trim)
extent(d.trim)

## write to disk
##location
#loc <- './data/topo/ID_DEM_10m.tif'
writeRaster(d.trim, loc, overwrite = TRUE)
