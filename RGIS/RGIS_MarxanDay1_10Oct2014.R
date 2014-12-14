# Author: Matt Watts
# Date: 10 Oct 2014
# Purpose: geoprocessing in R for Day 1, Introduction to Marxan course
#          Based upon course materials from
#              Task-oriented instructions for geospatial analysis in R
#              HAWTHORNE L. BEYER, REBECCA RUNTING, JUTTA BEHER
#              ARC Centre of Excellence for Environmental Decisions, Centre for Biodiversity & Conservation Science, University of Queensland, Brisbane QLD 4072, Australia
#          Also based upon geoprocessing code from Jeff Hanson.
# Data used: the datasets used in this code are accessible at 
#                http://marxan.net/courses/MarxanIntroCourse.zip

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
r.cost <- raster("data/cost/cost")
# display a summary of the rasters (to check they have loaded)
r.cost
r.reserve
r.bird
r.tasinvis # tasinvis had coord. ref. information we want to use for new pulayer

# readOGR rgdal, loads the coordinate system as well
#     can read KML also
#     might be slower with large datasets
#library(rgdal)
#v.tas <- readOGR("data","tas",verbose=FALSE)

# read the vectors
v.tas <- readShapeSpatial("data/tas.shp")
v.pulayer <- readShapeSpatial("data/Layers_Backup/pulayer_tas.shp")
# display a summary of the rasters (to check they have loaded)
v.tas # vtas has extent information we want to use for new pulayer
extent(v.tas)
v.pulayer

# plot
#plot(r.cost)
#plot(r.reserve)
#plot(r.bird)
#plot(r.tasinvis)
#plot(v.tas)
#plot(v.pulayer) # plot is really slow for drawing the planning unit layer

# clip polygons
#gintersect tests if they intersect, package rgeos
#?gIntersects
#??gintersection
#gIntersection does clipping

# this works
# create a vector grid over tas with 2500 polygons
x <- raster(ncol=50,nrow=50)
extent(x) <- extent(v.tas)
projection(x) <- projection(r.tasinvis)
projection(v.tas) <- projection(x)
putest2 <- rasterToPolygons(x)
# plot
#plot(putest2)
row.names(putest2)
gIsValid(putest2)

# now we need to clip/cookie cup the vector grid to just tas
#v.tas.simplify100 <- gSimplify(v.tas, 100, topologyPreserve=FALSE)
v.tas.simplify1k <- gSimplify(v.tas, 1000, topologyPreserve=FALSE)
#plot(v.tas.simplify100)
# plot
#plot(v.tas.simplify1k)
gIsValid(v.tas.simplify1k)
# works
# gI <- gIntersects(putest2,v.tas.simplify1k,byid=TRUE)
# works: YES! y is our clipped pu layer for tas
y <- gIntersection(putest2,v.tas.simplify1k,byid=TRUE)
# plot
#plot(y)
row.names(y)
gIsValid(y)

# coerce y into SpatialPolygonsDataFrame with PUID
# v.pulayer is SpatialPolygonsDataFrame with PUID
# y is SpatialPolygons with no PUID
#getSpPPolygonsIDSlots(y) 
#length(y)
#length(getSpPPolygonsIDSlots(y))

PUID <- sapply(slot(y, "polygons"), function(x) slot(x, "ID"))
length(PUID)
PUID <- as.data.frame(PUID)
dim(PUID)
row.names(PUID) <- row.names(y)

#y.pulayer <- SpatialPolygonsDataFrame(y,data=as.data.frame(row.names(y)))
#y.pulayer <- SpatialPolygonsDataFrame(y)
#y.pulayer <- SpatialPolygonsDataFrame(y,data=as.data.frame(row.names(getSpPPolygonsIDSlots(y))))
#y.pulayer <- SpatialPolygonsDataFrame(y,data=as.data.frame(sapply(slot(y, "polygons"), function(x) slot(x, "ID"))))
y.pulayer <- SpatialPolygonsDataFrame(y,data=PUID)

# summarize raster values for each polygon
row.names(v.pulayer)
# this works, takes a long time to run

# calculate cost of each pu
#values.cost <- extract(r.cost, v.pulayer, fun=mean, na.rm=TRUE)
# takes around 27 minutes
values.cost <- extract(r.cost, y, fun=mean, na.rm=TRUE)
values.cost
save(values.cost,file="values.cost.Rdata")

# calculate amount of reserve in each pu
# takes around 27 minutes
values.reserve <- extract(r.reserve, y, sum, na.rm=TRUE)
values.reserve
save(values.reserve,file="values.reserve.Rdata")

# calculate amount of bird in each pu
#values.bird <- intersect(v.pulayer,r.bird)
#values.bird <- intersect(y,r.bird)
#values.bird <- intersect(y.pulayer,r.bird)
# takes around 27 minutes
values.bird <- extract(r.bird, y, sum, na.rm=TRUE)
values.bird
save(values.bird,file="values.bird.Rdata")

# calculate amount of each veg type in each pu
# make raster stack
classRST <- r.tasinvis
#puPLY <- v.pulayer
puPLY <- y
rastSTK=list()
uniVEC=unique(classRST)
for (i in seq_along(uniVEC)) {
  rastSTK[[i]]=Which(classRST==uniVEC[i])
}
rastSTK=stack(rastSTK)
names(rastSTK)=paste0("VEG_",uniVEC)
# extract values
# takes around 14.5 minutes
values.tasinvis=extract(rastSTK, puPLY, sum, na.rm=TRUE) # all pus
#countsDF=extract(rastSTK, puPLY[1:10,], sum, na.rm=TRUE) # just first 10 pus
values.tasinvis
save(values.tasinvis,file="values.tasinvis.Rdata")

# process the geoprocessed data: cost, reserve, bird, tasinvis
# convert the geoprocessed data to a Marxan dataset

load("values.cost.Rdata")
values.cost
dim(values.cost)
head(values.cost)
values.cost[1,1]
values.cost[658,1]
values.cost[1643,1]
values.cost[1653,1]

load("values.reserve.Rdata")
values.reserve
max(values.reserve)
dim(values.reserve)
head(values.reserve)

load("values.bird.Rdata")
values.bird
dim(values.bird)
head(values.bird)

load("values.tasinvis.Rdata")
values.tasinvis
dim(values.tasinvis)
head(values.tasinvis)

# clean up the NA's
for (i in 1:dim(PUID)[1]){
  print(i)
}

# process the PUID's and save the pulayer

iCount <- length(row.names(y))

#row.names(y)[1] <- 1
#row.names(y)[iCount] <- iCount

for (i in 1:iCount)
{
  row.names(y)[i] <- i
}
#row.names(y) <- as.integer(row.names(y))

PUID <- as.integer(sapply(slot(y, "polygons"), function(x) slot(x, "ID")))
#length(PUID)
PUID <- as.data.frame(PUID)
#dim(PUID)
#row.names(PUID) <- row.names(y)

y.pulayer <- SpatialPolygonsDataFrame(y,data=PUID)

#y.pulayer
#plot(y.pulayer)

#names(y.pulayer)
#y.pulayer$PUID
#row.names(y.pulayer)
#length(y.pulayer$PUID)
#length(row.names(y.pulayer))
#y.pulayer$PUID[1] <- as.integer(1)
#row.names(y.pulayer)[1] <- as.integer(1)

writeSpatialShape(y.pulayer,"pulayer_tas.shp")

#values.tasinvis <- intersect(v.pulayer,r.tasinvis)
#values.bird <- intersect(v.pulayer[1],r.bird)
#values.bird <- intersect(r.bird,v.pulayer[1])

#sp.tas <- SpatialPolygons(v.tas@polygons,v.tas@proj4string)
#sp.tas <- SpatialPolygons(v.tas)
#?SpatialPolygons
#r.tas <- rasterize(v.tas@polygons)
#library(sp)
#poly.tas <- readShapePoly("data/tas.shp")
# this works
#sp.tas <- SpatialPolygons(poly.tas@polygons)
#?rasterize
#r.tas <- rasterize(sp.tas,raster(ncols=100,nrows=100))
#plot(r.tas)
#v.tas

#poly.pulayer <- readShapePoly("data/Layers_Backup/pulayer_tas.shp")
#sp.pulayer <- SpatialPolygons(poly.pulayer@polygons)
#plot(sp.pulayer)

#r.pulayer <- rasterize(sp.pulayer,raster(ncols=100,nrows=100),field="PUID")
#plot(sp.pulayer)
#plot(r.pulayer)

#crosstab()
#?crosstab

# extract a single planning unit
#v.puid1 <- v.pulayer[v.pulayer$PUID==1,]
#v.pulayer[v.pulayer$PUID==2,]
#v.pulayer[v.pulayer$PUID==1,]
#v.pulayer@data

#plot(v.puid1)

#r.puid1 <- rasterize(v.puid1,raster(ncols=100,nrows=100))
#plot(r.puid1)

# this works, gives cost of puid 1: 1320.75
#values.puid1.cost <- extract(r.cost, v.puid1, fun=mean, na.rm=TRUE)

#sumfn <- function(x){
#  sum(x==1)
#}
#extract(r.reserve, v.puid1, fun=sumfn)

#values.puid1.reserve <- extract(r.reserve, v.puid1, fun=sum, na.rm=TRUE)

#extract(r.reserve, v.pulayer, fun=sum, na.rm=TRUE)
#extract(r.bird, v.pulayer, fun=sum, na.rm=TRUE)

#sumfnvector <- function(x){
#  
#}
#extract(r.tasinvis, v.puid1, fun=sumfnvector)

#extent(r.bird)
#extent(v.pulayer)

#?length

#values.puid1.bird <- intersect(r.puid1,r.bird)
