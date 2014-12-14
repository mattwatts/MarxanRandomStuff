# Author: Matt Watts
# Date: 15 October 2014
# Purpose: geoprocessing in R for Day 1, Introduction to Marxan course
#          Based upon course materials from
#              Task-oriented instructions for geospatial analysis in R, by HAWTHORNE L. BEYER, REBECCA RUNTING, JUTTA BEHER
#          Also based upon geoprocessing code from Jeff Hanson.
# Data: the datasets used in this code are accessible at 
#           http://marxan.net/courses/MarxanIntroCourse.zip
#
# Copyright the University of Queensland 2014
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE Version 3
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU AFFERO GENERAL PUBLIC LICENSE Version 3 for more details.
#
# See the license here http://www.gnu.org/licenses/agpl.txt

library(raster)
library(maptools)
library(rgeos)

# set the working directory containing the raster data
setwd("/Users/matt/Documents/R/RGIS/ARCGIS_data")

# read the rasters
r.cost <- raster("data/cost/cost")
r.reserve <- raster("data/reserve/reserve")
r.bird <- raster("data/species/Birds/bird_raster")
r.tasinvis <- raster("data/species/tasinvis")

# read the vectors
v.tas <- readShapeSpatial("data/tas.shp")

# create a vector grid over tas with 2500 polygons
r.tasgrid <- raster(ncol=50,nrow=50)
extent(r.tasgrid) <- extent(v.tas)
projection(r.tasgrid) <- projection(r.tasinvis)
projection(v.tas) <- projection(r.tasgrid)
v.pulayer <- rasterToPolygons(r.tasgrid)

# clip the vector grid to just tas
v.tas.simplify1k <- gSimplify(v.tas, 1000, topologyPreserve=FALSE)
v.pulayer_tas <- gIntersection(v.pulayer,v.tas.simplify1k,byid=TRUE)

# calculate cost of each pu: takes around 27 minutes
values.cost <- extract(r.cost, v.pulayer_tas, fun=mean, na.rm=TRUE)
save(values.cost,file="values.cost.Rdata")

# calculate amount of reserve in each pu: takes around 27 minutes
values.reserve <- extract(r.reserve, v.pulayer_tas, sum, na.rm=TRUE)
save(values.reserve,file="values.reserve.Rdata")

# calculate amount of bird in each pu: takes around 27 minutes
values.bird <- extract(r.bird, v.pulayer_tas, sum, na.rm=TRUE)
save(values.bird,file="values.bird.Rdata")

# calculate amount of each veg type in each pu
# make raster stack with one raster for each veg type
rastSTK=list()
uniVEC=unique(r.tasinvis)
for (i in seq_along(uniVEC)) {
  rastSTK[[i]]=Which(r.tasinvis==uniVEC[i])
}
rastSTK=stack(rastSTK)
names(rastSTK)=paste0("VEG_",uniVEC)
# extract values for raster stack: takes around 14.5 minutes
values.tasinvis=extract(rastSTK, v.pulayertas, sum, na.rm=TRUE)
save(values.tasinvis,file="values.tasinvis.Rdata")

vegnames <- names(rastSTK)
save(vegnames,file="vegnames.Rdata")

# process the PUID's and save the pulayer: PUID's as one-based integers
row.names(v.pulayer_tas) <- as.character(rep(1:length(row.names(v.pulayer_tas))))
PUID <- as.integer(sapply(slot(v.pulayer_tas, "polygons"), function(x) slot(x, "ID")))
PUID <- as.data.frame(PUID)
v.pulayer_tas.shp <- SpatialPolygonsDataFrame(v.pulayer_tas,data=PUID)

writeSpatialShape(v.pulayer_tas.shp,"pulayer_tas.shp")

# process the geoprocessed data for cost, reserve, tasinvis, bird
# and convert to a Marxan dataset
load("values.cost.Rdata")
load("values.reserve.Rdata")
load("values.bird.Rdata")
load("values.tasinvis.Rdata")
load("vegnames.Rdata")

# clean up the NA's in the values which are artifacts from geoprocessing
for (i in 1:dim(PUID)[1])
{
  if (is.na(values.cost[i]))
  {
    values.cost[i] <- 0.1 # make the smallest cost a bit larger than zero
  }
  if (is.na(values.reserve[i]))
  {
    values.reserve[i] <- 0  
  }
  if (is.na(values.bird[i]))
  {
    values.bird[i] <- 0  
  }
  for (j in 1:dim(values.tasinvis)[2])
  {
    if (is.na(values.tasinvis[i,j]))
    {
      values.tasinvis[i,j] <- 0  
    }    
  }
}

# compute pu status
pu.status <- values.reserve
reserved <- max(values.reserve) / 2 # pu reserved if more than 50% is reserved
for (i in 1:dim(PUID)[1])
{
  if (values.reserve[i] > reserved)
  {
    pu.status[i] <- 2
  } else {
    pu.status[i] <- 0
  }
}

# generate pu.dat
write('id,cost,status',file="pu.dat")
for (i in 1:dim(PUID)[1])
{
  write(paste(PUID[i,1],values.cost[i],pu.status[i],sep=","),
        file="pu.dat",append=TRUE)
}

# generate spec.dat
write('id,prop,spf,name',file="spec.dat")
# veg types are first
for (i in 1:dim(values.tasinvis)[2])
{
  write(paste(i,0.3,1,vegnames[i],sep=","),
        file="spec.dat",append=TRUE)
}
# bird is next
write(paste(dim(values.tasinvis)[2]+1,0.3,1,"Bird",sep=","),
      file="spec.dat",append=TRUE)

# generate puvsp.dat
write('species,pu,amount',"puvsp.dat")
for (i in 1:dim(PUID)[1])
{
  # veg types are first
  for (j in 1:dim(values.tasinvis)[2])
  {
    if (values.tasinvis[i,j] > 0)
    {
      write(paste(j,PUID[i,1],values.tasinvis[i,j],sep=","),
            "puvsp.dat",append=TRUE)
    }
  }
  # bird is next
  if (values.bird[i] > 0)
  {
    write(paste(dim(values.tasinvis)[2]+1,PUID[i,1],values.bird[i],sep=","),
          "puvsp.dat",append=TRUE)
  }
}

# generate sporder.dat
write('species,pu,amount',"sporder.dat")
# veg types are first
for (j in 1:dim(values.tasinvis)[2])
{
  for (i in 1:dim(PUID)[1])
  {
    if (values.tasinvis[i,j] > 0)
    {
      write(paste(j,PUID[i,1],values.tasinvis[i,j],sep=","),
            "sporder.dat",append=TRUE)
    }
  }
}
# bird is next
for (i in 1:dim(PUID)[1])
{
  if (values.bird[i] > 0)
  {
    write(paste(dim(values.tasinvis)[2]+1,PUID[i,1],values.bird[i],sep=","),
          "sporder.dat",append=TRUE)
  }  
}

