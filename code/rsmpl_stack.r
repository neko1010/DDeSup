library(sf)
library(raster)
library(fasterize)

####

setwd("C:/Users/nicholaskolarik/DDeSuP/")

## Projection
idaho.cent <- "+proj=tmerc +lat_1d=41.66666666666666 +lon_0=-114 +k=0.9999473679999999 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs "

## Teton/Fremont/Madison Counties as example study area
## HUC10 units
tefremad <- read_sf("./data/High_divide/te_fre_mad/shp/te_fre_mad_huc10.shp")
## project it
tefremad.prj <- st_transform(tefremad, st_crs(idaho.cent))


##Load outcome data
# GSW layer
gsw <- raster("./data/GSWE_2017.tif")
gsw.prj <- projectRaster(gsw, crs = idaho.cent, method = 'ngb')
#gsw.prj2 <- projectRaster(gsw, crs = test)

## Predictors
# Elevation
elev <- raster("./data/DEM_30m_prj.tif")
elev.prj <- projectRaster(elev, crs = idaho.cent)
#writeRaster(elev.prj, "./data/DEM_30m_prjIDSP.tif")
elev.prj.tfm <- crop(elev.prj, tefremad.prj)

# Slope
slope <- raster("./data/id_slope.tif")
slope.tfm <- crop(slope, tefremad.prj)

# Aspect
aspect <- raster("./data/id_aspect.tif")
aspect.tfm <- crop(aspect, tefremad.prj)

# TPI
tpi <- raster("./data/id_tpi.tif")
tpi.tfm <- crop(tpi, tefremad.prj)

# TWI
twi <- raster("./data/id_twi.tif")
twi.tfm <- crop(twi, tefremad.prj)

# Timw lag (last year)

# Hydric
hydric <- read_sf("./data/Hydric_Soils.shp")
hydric.prj <- st_transform(hydric, crs = idaho.cent)
## as raster
hydric.ras <- fasterize(hydric.prj, slope.tfm)
hydric.ras.tfm <- crop(hydric.ras, tefremad.prj)

## Using annual 30 yr normals for now -> have normals for each month if we want selected months
# Precip
precip <- raster("./data/PRISM_precip/PRISM_ppt_30yr_normal_800mM2_annual_asc.asc")
## crop to study area
precip.tfw <- crop(precip, gsw)
## project
precip.tfw.prj <- projectRaster(precip.tfw, crs = idaho.cent)


# Temp
temp <- raster("./data/PRISM_t/PRISM_tmean_30yr_normal_800mM2_annual_asc.asc")
## crop to study area
temp.tfw <- crop(temp, gsw)
## project
temp.tfw.prj <- projectRaster(temp.tfw, crs = idaho.cent)

## Loop through layers, resample (if necessary), stack - > should have done all the projections here?
layers <- c(elev.prj.tfm, aspect.tfm, slope.tfm, tpi.tfm, twi.tfm, hydric.ras.tfm)

## need to consider how these are resampled -> ngb for categorical 
rsmpl <- c(temp.tfw.prj, precip.tfw.prj)
gsw.rsmpl <- projectRaster(gsw.prj, elev.prj.tfm, method = 'ngb')
layers <- c(gsw.rsmpl, layers)
for (i in rsmpl) {
  layers <- c(layers, projectRaster(i, elev.prj.tfm, method = "bilinear"))
  print(layers)
}

## stack the layers!
s <- stack(layers)
names(s) <- c('GSW', 'Elevation', 'Aspect', 'Slope', 'TPI', 'TWI', 'Hydric', 'Temp', 'Precip')
plot(s)
writeRaster(s, "./data/2016_stack.tif")#, overwrite = T)
