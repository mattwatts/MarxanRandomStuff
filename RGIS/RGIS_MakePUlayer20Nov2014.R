# Author: Matt Watts
# Date: 20 November 2014
# Purpose: Prepare some data to develop mapotron for improved display of planning unit layers in Marxan.net apps
# Data: Shape of Australia extracted from Global Maps data ne_10m_admin_0_countries.shp
#       The AU shape file is here: marxan.net/data/AU.zip
#       The Planning unit layers generated are here: marxan.net/data/AU_pulayers.zip

library(raster)
library(maptools)
library(rgeos)
library(rgdal)

download.file("marxan.net/data/AU.zip", destfile, method, quiet = FALSE, mode = "w",
              cacheOK = TRUE,
              extra = getOption("download.file.extra"))

# set the working directory containing the raster data
setwd("/Users/matt/Documents/R/RGIS")

# read the vector which is the outline of Australia
v.au <- readOGR(".","AU")

# peek at the data
projection(v.au)
extent(v.au)
plot(v.au)

MakePULayer <- function(iCol,iRow)
{
  # create vector grid
  r.augrid <- raster(ncol=iCol,nrow=iRow)
  projection(r.augrid) <- projection(v.au)
  extent(r.augrid) <- extent(v.au)
  v.augrid <- rasterToPolygons(r.augrid)
  
  # clip the vector grid
  v.pulayer_au <- gIntersection(v.augrid,v.au,byid=TRUE)
  
  # process the PUID's and save the pulayer: PUID's as one-based integers
  row.names(v.pulayer_au) <- as.character(rep(1:length(row.names(v.pulayer_au))))
  PUID <- as.integer(sapply(slot(v.pulayer_au, "polygons"), function(x) slot(x, "ID")))
  PUID <- as.data.frame(PUID)
  v.pulayer_au_shp <- SpatialPolygonsDataFrame(v.pulayer_au,data=PUID)
  writeOGR(v.pulayer_au_shp,".",paste0("pulayer_au_",length(row.names(v.pulayer_au)),"_shp"),driver="ESRI Shapefile")
  
  # display the planning unit layer
  plot(v.pulayer_au.shp)
}

# make the planning unit layers at a variety of spatial scales
MakePULayer(10,10)
MakePULayer(50,50)
MakePULayer(100,100)
MakePULayer(200,200)
MakePULayer(300,300)
