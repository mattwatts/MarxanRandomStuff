# Author: Matt Watts
# Date: 20 November 2014
# Purpose: Prepare some data to develop mapotron for
#          displaying planning unit layers on Marxan.net
# Data: Shape of Australia extracted from Global Maps data
#       ne_10m_admin_0_countries.shp
#           http://marxan.net/courses/MarxanIntroCourse.zip
#

library(raster)
library(maptools)
library(rgeos)
library(rgdal)

# set the working directory containing the raster data
setwd("/Users/matt/Documents/R/RGIS")

# read the vector
v.au <- readOGR(".","AU")

projection(v.au)
extent(v.au)

plot(v.au)

# create vector grids
r.augrid <- raster(ncol=100,nrow=100)
projection(r.augrid) <- projection(v.au)
extent(r.augrid) <- extent(v.au)
v.augrid <- rasterToPolygons(r.augrid)

#plot(v.augrid)

# clip the vector grid
v.pulayer_au <- gIntersection(v.augrid,v.au,byid=TRUE)

#plot(v.pulayer_au)

projection(v.pulayer_au)
extent(v.pulayer_au)

# process the PUID's and save the pulayer: PUID's as one-based integers
row.names(v.pulayer_au) <- as.character(rep(1:length(row.names(v.pulayer_au))))
PUID <- as.integer(sapply(slot(v.pulayer_au, "polygons"), function(x) slot(x, "ID")))
PUID <- as.data.frame(PUID)
v.pulayer_au.shp <- SpatialPolygonsDataFrame(v.pulayer_au,data=PUID)
writeOGR(v.pulayer_au.shp,".",paste0("pulayer_au_",length(row.names(v.pulayer_au)),".shp"),driver="ESRI Shapefile")

plot(v.pulayer_au.shp)


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
  v.pulayer_au.shp <- SpatialPolygonsDataFrame(v.pulayer_au,data=PUID)
  writeOGR(v.pulayer_au.shp,".",paste0("pulayer_au_",length(row.names(v.pulayer_au)),".shp"),driver="ESRI Shapefile")
  
  plot(v.pulayer_au.shp)
}

MakePULayer(10,10)
MakePULayer(50,50)
MakePULayer(100,100)
MakePULayer(200,200)
MakePULayer(300,300)
