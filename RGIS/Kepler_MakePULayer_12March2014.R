# Author: Matt Watts
# Date: 12 March 2015
# Purpose: Create a planning unit layer for Marxan. This R code is to be abstracted and embedded in a Kepler component.
# Data: Study area outline as a polygon
#       Grid cell size

library(raster)
library(maptools)
library(rgeos)
library(rgdal)

# get the study area outline from disk (note: we grab a polygon with the outline of Austalian continent)
download.file("marxan.net/data/AU.zip", destfile, method, quiet = FALSE, mode = "w",
              cacheOK = TRUE,
              extra = getOption("download.file.extra"))

# read the vector which is the outline of the study area
v.outline <- readOGR(".","AU")

MakePULayer <- function(iCol,iRow)
{
  # create vector grid
  r.pugrid <- raster(ncol=iCol,nrow=iRow)
  projection(r.pugrid) <- projection(v.outline)
  extent(r.pugrid) <- extent(v.outline)
  v.pugrid <- rasterToPolygons(r.pugrid)
  
  # clip the vector grid
  v.pulayer <- gIntersection(v.pugrid,v.outline,byid=TRUE)
  
  # create the planning unit ID's as one-based integers
  row.names(v.pulayer) <- as.character(rep(1:length(row.names(v.pulayer))))
  PUID <- as.integer(sapply(slot(v.pulayer, "polygons"), function(x) slot(x, "ID")))
  PUID <- as.data.frame(PUID)
  v.pulayer_shp <- SpatialPolygonsDataFrame(v.pulayer,data=PUID)
  
  # write the planning unit layer to disk
  writeOGR(v.pulayer_shp,".",paste0("pulayer_",length(row.names(v.pulayer)),"_shp"),driver="ESRI Shapefile")
}

# make the planning unit layers at appropriate of spatial scales
iGridCellSize <- 100
MakePULayer(iGridCellSize,iGridCellSize)
