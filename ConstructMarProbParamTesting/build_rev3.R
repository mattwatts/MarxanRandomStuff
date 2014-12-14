# Author: Matt Watts
# Date: 10 Dec 2014
# Purpose: dataset construction, MarProb
# Data: 
#       "/Users/matt/Documents/zzz/GISdata"
#       

library(raster)
library(maptools)
library(rgeos)
library(rgdal)
library(foreign)
library(sqldf)

###################
# make scenarios with only the coastal planning units
# load the coastal table

setwd("/Users/matt/Documents/zzz")


coastal <- read.dbf("GISdata/habitat_new_project.dbf")
head(coastal)
coastal <- sqldf("SELECT PUID,Coastal from coastal")
# there are some NA values for the coastal fields that we make 0
coastal[is.na(coastal)] <- 0
write.dbf(coastal,"GISdata/coastal.dbf")

head(coastal$Coastal)

# prepare all_regions
dir.create("marxan_rev3")
dir.create("marxan_rev3/all_regions")
dir.create("marxan_rev3/all_regions/input")
dir.create("marxan_rev3/all_regions/output")
dir.create("marxan_rev3/all_regions/pulayer")
for (iRegion in 1:8)
{
  dir.create(paste0("marxan_rev3/region",iRegion))
  dir.create(paste0("marxan_rev3/region",iRegion,"/input"))
  dir.create(paste0("marxan_rev3/region",iRegion,"/output"))
  dir.create(paste0("marxan_rev3/region",iRegion,"/pulayer"))
}

# make pulayers with only coastal PUID's
pulayer_all <- readOGR("marxan/all_regions/pulayer","pulayer_simp1k")
puoutline_ <- readOGR("marxan/all_regions/pulayer","puoutline_simp1k")

# build pulayer for all regions
writeOGR(pulayer_all[(coastal$Coastal > 0),],"marxan_rev3/all_regions/pulayer","pulayer",driver="ESRI Shapefile")

# load the ecoregions
ecoregions <- read.csv("GISdata/ecoregion.csv")
ecoregions <- sqldf("SELECT PUID,Eco from ecoregions")
coast_eco <- sqldf("SELECT * from coastal LEFT JOIN ecoregions USING(PUID)")
coast_eco$COEC <- as.integer(coast_eco$Coastal) * as.integer(coast_eco$Eco)
Eco <- coast_eco$COEC

# build the 8 planning unit layers
for (iRegion in 1:8)
{
  region_pulayer <- pulayer_all[Eco==iRegion,]
  writeOGR(region_pulayer,paste0("marxan_rev3/region",iRegion,"/pulayer"),"pulayer",driver="ESRI Shapefile")
}

# make pu.dat's with only coastal PUID's
pudat1 <- read.csv("marxan/all_regions/input/pu1.dat")
pudat2 <- read.csv("marxan/all_regions/input/pu2.dat")
pudat3 <- read.csv("marxan/all_regions/input/pu3.dat")
pudat4 <- read.csv("marxan/all_regions/input/pu4.dat")

pudat1$prob <- pudat1$prob / max(pudat1$prob)
pudat2$prob <- pudat2$prob / max(pudat2$prob)
pudat3$prob <- pudat3$prob / max(pudat3$prob)
pudat4$prob <- pudat4$prob / max(pudat4$prob)

write.table(pudat1[(coastal$Coastal > 0),],file="marxan_rev3/all_regions/input/pu1.dat",row.names=FALSE,sep=",",quote=FALSE)
write.table(pudat2[(coastal$Coastal > 0),],file="marxan_rev3/all_regions/input/pu2.dat",row.names=FALSE,sep=",",quote=FALSE)
write.table(pudat3[(coastal$Coastal > 0),],file="marxan_rev3/all_regions/input/pu3.dat",row.names=FALSE,sep=",",quote=FALSE)
write.table(pudat4[(coastal$Coastal > 0),],file="marxan_rev3/all_regions/input/pu4.dat",row.names=FALSE,sep=",",quote=FALSE)


hist(pudat1$prob[(pudat1$prob > 0)],breaks=100)

rpu1 <- pudat1[Eco==1,]
hist(rpu1$prob[(rpu1$prob > 0)],breaks=100)
rpu2 <- pudat1[Eco==2,]
hist(rpu2$prob[(rpu2$prob > 0)],breaks=100)
rpu3 <- pudat1[Eco==3,]
hist(rpu3$prob[(rpu3$prob > 0)],breaks=100)
rpu1 <- pudat1[Eco==1,]
hist(pudat1$prob[(pudat1$prob > 0)],breaks=100)
rpu1 <- pudat1[Eco==1,]
hist(pudat1$prob[(pudat1$prob > 0)],breaks=100)
rpu1 <- pudat1[Eco==1,]
hist(pudat1$prob[(pudat1$prob > 0)],breaks=100)
rpu7 <- pudat1[Eco==7,]
hist(rpu7$prob[(rpu7$prob > 0)],breaks=100)

for (iRegion in 1:8)
{
  rpu1 <- pudat1[Eco==iRegion,]
  rpu1$prob <- rpu1$prob / max(rpu1$prob)
  write.table(rpu1,file=paste0("marxan_rev3/region",iRegion,"/input/pu1.dat"),row.names=FALSE,sep=",",quote=FALSE)
  
  rpu2 <- pudat2[Eco==iRegion,]
  rpu2$prob <- rpu2$prob / max(rpu2$prob)
  write.table(rpu2,file=paste0("marxan_rev3/region",iRegion,"/input/pu2.dat"),row.names=FALSE,sep=",",quote=FALSE)
  
  rpu3 <- pudat3[Eco==iRegion,]
  rpu3$prob <- rpu3$prob / max(rpu3$prob)
  write.table(rpu3,file=paste0("marxan_rev3/region",iRegion,"/input/pu3.dat"),row.names=FALSE,sep=",",quote=FALSE)
  
  rpu4 <- pudat4[Eco==iRegion,]
  rpu4$prob <- rpu4$prob / max(rpu4$prob)
  write.table(rpu4[Eco==iRegion,],file=paste0("marxan_rev3/region",iRegion,"/input/pu4.dat"),row.names=FALSE,sep=",",quote=FALSE)
}

# make puvspr.dat with only coastal PUID's
putable <- read.dbf("GISdata/habitat_new.dbf")
(featurenames <- c("G_Posid","G_Coralgn","G_Caves","Coast","s0to60","s60to200","S200to2k",
                   "h0to60","h60to200","h200to2k",
                   "PelagDeep_","WCOSS_EM","AREA","COSTGFCM","COSTSAUP"))
costgfcm <- read.dbf("GISdata/pu_x_costgfcm1k.dbf")
costsaup <- read.dbf("GISdata/pu_x_costsaup1k.dbf")
costgfcm <- sqldf("SELECT PUID,MEAN from costgfcm")
colnames(costgfcm)[2] <- "COSTGFCM"
costsaup <- sqldf("SELECT PUID,MEAN from costsaup")
colnames(costsaup)[2] <- "COSTSAUP"
pucost <- sqldf("SELECT * from costgfcm LEFT JOIN costsaup USING(PUID)")
putable <- sqldf("SELECT * from putable LEFT JOIN pucost USING(PUID)")
putable[is.na(putable)] <- 0
putable$AREA <- putable$AREA/max(putable$AREA)
putable$COSTGFCM <- putable$COSTGFCM/max(putable$COSTGFCM)
putable$COSTSAUP <- putable$COSTSAUP/max(putable$COSTSAUP)
# build the puvsp.dat
write('species,pu,amount',file="marxan_rev3/all_regions/input/puvsp.dat")
iRowCount <- dim(putable)[1]
iColCount <- length(featurenames)
for (i in 1:iRowCount) # for each planning unit
{
  if (coastal$Coastal[i] > 0)
  {
    for (j in 1:iColCount) # for each conservation feature
    {
      if (putable[i,featurenames[j]] > 0)
      {
        write(paste(j,                          # species id
                    putable$PUID[i],            # pu id
                    putable[i,featurenames[j]], # amount
                    sep=","),
              file="marxan_rev3/all_regions/input/puvsp.dat",
              append=TRUE)
      }
    }
  }
}
# build the puvsp_sporder.dat
write('species,pu,amount',file="marxan_rev3/all_regions/input/puvsp_sporder.dat")
iRowCount <- dim(putable)[1]
iColCount <- length(featurenames)
for (j in 1:iColCount) # for each conservation feature
{
  for (i in 1:iRowCount) # for each planning unit
  {
    if (coastal$Coastal[i] > 0)
    {
      if (putable[i,featurenames[j]] > 0)
      {
        write(paste(j,                          # species id
                    putable$PUID[i],            # pu id
                    putable[i,featurenames[j]], # amount
                    sep=","),
              file="marxan_rev3/all_regions/input/puvsp_sporder.dat",
              append=TRUE)
      }
    }
  }
}
# build the 8 puvsp.dat and puvsp_sporder.dat files
for (iRegion in 1:8)
{
  write('species,pu,amount',file=paste0("marxan_rev3/region",iRegion,"/input/puvsp.dat"))
  write('species,pu,amount',file=paste0("marxan_rev3/region",iRegion,"/input/puvsp_sporder.dat"))
}
# build the 8 puvsp.dat files
iRowCount <- dim(putable)[1]
iColCount <- length(featurenames)
for (i in 1:iRowCount) # for each planning unit
{
  if (Eco[i] > 0)
  {
    for (j in 1:iColCount) # for each conservation feature
    {
      if (putable[i,featurenames[j]] > 0)
      {
        write(paste(j,                          # species id
                    putable$PUID[i],            # pu id
                    putable[i,featurenames[j]], # amount
                    sep=","),
              file=paste0("marxan_rev3/region",Eco[i],"/input/puvsp.dat"),
              append=TRUE)
      }
    }
  }
}
# build the 8 puvsp_sporder.dat files
iRowCount <- dim(putable)[1]
iColCount <- length(featurenames)
for (j in 1:iColCount) # for each conservation feature
{
  for (i in 1:iRowCount) # for each planning unit
  {
    if (Eco[i] > 0)
    {
      if (putable[i,featurenames[j]] > 0)
      {
        write(paste(j,                          # species id
                    putable$PUID[i],            # pu id
                    putable[i,featurenames[j]], # amount
                    sep=","),
              file=paste0("marxan_rev3/region",Eco[i],"/input/puvsp_sporder.dat"),
              append=TRUE)
      }
    }
  }
}
# spec.dat
file.copy("ParameterTesting_rev2/p/files/all_regions/input/spec.dat","marxan_rev3/all_regions/input/spec.dat",overwrite=TRUE)
file.copy("ParameterTesting_rev2/p/files/all_regions/input/spec.dat","marxan_rev3/all_regions/input/conservation_features.csv",overwrite=TRUE)
for (iRegion in 1:8)
{
  file.copy("marxan_rev3/all_regions/input/spec.dat",paste0("marxan_rev3/region",iRegion,"/input/spec.dat"),overwrite=TRUE)
  file.copy("marxan_rev3/all_regions/input/spec.dat",paste0("marxan_rev3/region",iRegion,"/input/conservation_features.csv"),overwrite=TRUE)
}
# input.dat
file.copy("ParameterTesting_rev2/p/files/all_regions/input1.dat","marxan_rev3/all_regions/input1.dat",overwrite=TRUE)
file.copy("ParameterTesting_rev2/p/files/all_regions/input2.dat","marxan_rev3/all_regions/input2.dat",overwrite=TRUE)
file.copy("ParameterTesting_rev2/p/files/all_regions/input3.dat","marxan_rev3/all_regions/input3.dat",overwrite=TRUE)
file.copy("ParameterTesting_rev2/p/files/all_regions/input4.dat","marxan_rev3/all_regions/input4.dat",overwrite=TRUE)
for (iRegion in 1:8)
{
  file.copy("marxan_rev3/all_regions/input1.dat",paste0("marxan_rev3/region",iRegion,"/input1.dat"),overwrite=TRUE)
  file.copy("marxan_rev3/all_regions/input2.dat",paste0("marxan_rev3/region",iRegion,"/input2.dat"),overwrite=TRUE)
  file.copy("marxan_rev3/all_regions/input3.dat",paste0("marxan_rev3/region",iRegion,"/input3.dat"),overwrite=TRUE)
  file.copy("marxan_rev3/all_regions/input4.dat",paste0("marxan_rev3/region",iRegion,"/input4.dat"),overwrite=TRUE)
}

# prepare the .RData file with planning units and status
# generate .Rdata files for web apps

sWorkDir <- "/Users/matt/Documents/zzz/ParameterTesting_rev3/p/files"
setwd(sWorkDir)
getwd()

# load the simplified pulayers and puoutline
pulayer_all <- readOGR("all_regions/pulayer","pulayer")
puoutline_ <- readOGR("all_regions/pulayer","puoutline_simp1k")
pulayer_1 <- readOGR("region1/pulayer","pulayer")
pulayer_2 <- readOGR("region2/pulayer","pulayer")
pulayer_3 <- readOGR("region3/pulayer","pulayer")
pulayer_4 <- readOGR("region4/pulayer","pulayer")
pulayer_5 <- readOGR("region5/pulayer","pulayer")
pulayer_6 <- readOGR("region6/pulayer","pulayer")
pulayer_7 <- readOGR("region7/pulayer","pulayer")
pulayer_8 <- readOGR("region8/pulayer","pulayer")

class(pulayer_all)

# load status for each region
pudat_ <- read.csv("all_regions/input/pu1.dat")
rall_pustatus_ <- unlist(sqldf("SELECT status from pudat_"))

plot(pulayer_all)
library(PBSmapping)

pulayer_all <- SpatialPolygons2PolySet(pulayer_all)
puoutline_ <- SpatialPolygons2PolySet(puoutline_)
pulayer_1 <- SpatialPolygons2PolySet(pulayer_1)
pulayer_2 <- SpatialPolygons2PolySet(pulayer_2)
pulayer_3 <- SpatialPolygons2PolySet(pulayer_3)
pulayer_4 <- SpatialPolygons2PolySet(pulayer_4)
pulayer_5 <- SpatialPolygons2PolySet(pulayer_5)
pulayer_6 <- SpatialPolygons2PolySet(pulayer_6)
pulayer_7 <- SpatialPolygons2PolySet(pulayer_7)
pulayer_8 <- SpatialPolygons2PolySet(pulayer_8)

plotPolys(pulayer_all)


pudat_ <- read.csv("region1/input/pu1.dat")
r1pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("region2/input/pu1.dat")
r2pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("region3/input/pu1.dat")
r3pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("region4/input/pu1.dat")
r4pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("region5/input/pu1.dat")
r5pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("region6/input/pu1.dat")
r6pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("region7/input/pu1.dat")
r7pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("region8/input/pu1.dat")
r8pustatus_ <- unlist(sqldf("SELECT status from pudat_"))

# save the .RData file with planning unit layers and status vectors
save(pulayer_all,pulayer_1,pulayer_2,pulayer_3,pulayer_4,
     pulayer_5,pulayer_6,pulayer_7,pulayer_8,
     puoutline_,
     rall_pustatus_,r1pustatus_,r2pustatus_,r3pustatus_,r4pustatus_,
     r5pustatus_,r6pustatus_,r7pustatus_,r8pustatus_,
     file="/Users/matt/Documents/zzz/ParameterTesting_rev3/p/files/pulayer_.RData")



#############
# code to look at the "wrong file" map

library(rgdal) # readOGR
library(raster)
library(maptools)
library(rgeos)

v.wrong_file <- readOGR("GISdata/wrong file","wrong_file")

# peek at the data
(projection(v.wrong_file))
(puextent <- extent(v.wrong_file))
plot(v.wrong_file)

head(v.wrong_file)

# frequency histogram of the values
hist(v.wrong_file$settle,breaks=100)

# make the colour ramp
(rmax <- max(v.wrong_file$settle))
(rmin <- min(v.wrong_file$settle))
iLength <- length(v.wrong_file$settle)
blueramp <- colorRampPalette(c("white","blue"))(101)
colours <- rep(blueramp[1],length(iLength))
for (j in 1:iLength)
{
  colours[j] <- blueramp[round(100 / rmax * v.wrong_file$settle[j])+1]
  if (v.wrong_file$settle[j] == 0) { colours[j] <- "white" }
}

# plot the values with no border
plot(v.wrong_file,col=colours,border=NA)
