# Author: Matt Watts
# Date: 1 Dec 2014
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

# set the working directory containing the GIS data
setwd("/Users/matt/Documents/zzz")

# read the vectors
v.habitat_new <- readOGR("GISdata","habitat_new")
#v.cost_saup <- readOGR("GISdata/Cost data","cost_saup")
v.cost_saup_proj <- readOGR("GISdata","cost_saup_proj")
#v.cost_gfcm <- readOGR("GISdata/Cost data","test_pu_cost")
v.cost_gfcm_proj <- readOGR("GISdata","cost_gfcm_proj")
#v.reserve1 <- readOGR("GISdata/Med_MPA","MPA_national_Project1")
#v.reserve2 <- readOGR("GISdata/Med_MPA","Natura2000_Project1")
#v.reserve3 <- readOGR("GISdata/Med_MPA","SPAMIs_Project")

# peek at the data
projection(v.habitat_new)
(puextent <- extent(v.habitat_new))
#plot(v.habitat_new)
projection(v.cost_saup)
extent(v.cost_saup)
#plot(v.cost_saup)
projection(v.cost_gfcm)
extent(v.cost_gfcm)
#plot(v.cost_gfcm)
projection(v.cost_gfcm_proj)
cost1extent <- extent(v.cost_gfcm_proj)
#plot(v.cost_gfcm_proj)
projection(v.cost_saup_proj)
extent(v.cost_saup_proj)
plot(v.cost_saup_proj)
projection(v.reserve1)
extent(v.reserve1)
plot(v.reserve1)
projection(v.reserve2)
extent(v.reserve2)
plot(v.reserve2)
projection(v.reserve3)
extent(v.reserve3)
plot(v.reserve3)

# for rendering maps for the publication, retain aspect ratio
# extract aspect ration from puextent
# aspect ratio width:height, x:y
# compute the aspect ratio for all regions and for each region
puextent
(iXpixels <- xmax(puextent) - xmin(puextent))
(iYpixels <- ymax(puextent) - ymin(puextent))
(iAspectRatio <- iXpixels / iYpixels)

#(6683750 - 2880217)/1000
#nrow(v.habitat_new)
#ncol(v.habitat_new)
#head(v.habitat_new$PUID)
#tail(v.habitat_new$PUID)
#v.habitat_new$PUID[1] # the PUID
#v.habitat_new$Impact[1] # the probability, 0=0,149=1.0

# frequency distribution of non-zero probabilities
nonzero <- (v.habitat_new$Impact > 0)
hist(v.habitat_new$Impact[nonzero],breaks=100)

#names(v.habitat_new)[1]
#v.habitat_new["G_Posid"]

#PUID <- v.habitat_new$PUID
#G_Posid <- v.habitat_new$G_Posid
#G_Posid[G_Posid>0]

# read the planning unit layer table
putable <- read.dbf("GISdata/habitat_new.dbf")
#head(putable)
#dim(putable)
#colnames(putable)
#head(putable["G_Posid"])
#putable[1,"G_Posid"]
#putable[3,"G_Posid"]
#putable$MPA
#length(putable$MPA)
#sum(putable$MPA)
#putable$PUID[1]
#putable$PUID[2]

# define the feature names we will extract to the Marxan dataset
# we also include the 3 cost fields for reporting purposes (but they won't have targets)
(featurenames <- c("G_Posid","G_Coralgn","G_Caves","Coast","s0to60","s60to200","S200to2k",
                  "h0to60","h60to200","h200to2k",
                  "PelagDeep_","WCOSS_EM","AREA","COSTGFCM","COSTSAUP"))

# double check the feature names with list
checknames <- c("G_Posid","G_Coralgn","G_Caves","Coast","s0to60","s60to200","S200to2k",
                "h0to60","h60to200","h200to2k",
                "PelagDeep_","WCOSS_EM")

#for (name in featurenames)
#{
#  print(name)
#  print(head(putable[name]))
#}
#length(featurenames)

# read the cost tables
costgfcm <- read.dbf("GISdata/pu_x_costgfcm1k.dbf")
costsaup <- read.dbf("GISdata/pu_x_costsaup1k.dbf")
#colnames(costgfcm)
#colnames(costsaup)

# prepart the cost tables to be joined
costgfcm <- sqldf("SELECT PUID,MEAN from costgfcm")
colnames(costgfcm)[2] <- "COSTGFCM"
costsaup <- sqldf("SELECT PUID,MEAN from costsaup")
colnames(costsaup)[2] <- "COSTSAUP"

# join costs to putable so we can use them as features for reporting
putable <- sqldf("SELECT * from putable LEFT JOIN pucost USING(PUID)")
# there are some NA values for the cost fields that we make 0
putable[is.na(putable)] <- 0
# normalise the cost fields
putable$AREA <- putable$AREA/max(putable$AREA)
putable$COSTGFCM <- putable$COSTGFCM/max(putable$COSTGFCM)
putable$COSTSAUP <- putable$COSTSAUP/max(putable$COSTSAUP)

# create directories for Marxan dataset and populate with default parameters and files
dir.create("marxan")
dir.create("marxan/all_regions")
dir.create("marxan/all_regions/input")
dir.create("marxan/all_regions/output")
dir.create("marxan/all_regions/pulayer")
file.copy("input1.dat","marxan/all_regions",overwrite=TRUE)
file.copy("input2.dat","marxan/all_regions",overwrite=TRUE)
file.copy("input3.dat","marxan/all_regions",overwrite=TRUE)
file.copy("input4.dat","marxan/all_regions",overwrite=TRUE)
file.copy("MarOpt_v243_Mac64","marxan/all_regions")
file.copy("MarOpt_v243_Linux64","marxan/all_regions")
file.copy("boundaries/bound.dat","marxan/all_regions/input",overwrite=TRUE)
# make the core directories and populate them

# build the pu.dat
pudat <- sqldf("SELECT PUID,MPA,Impact,AREA from putable")
pucost <- sqldf("SELECT * from costgfcm LEFT JOIN costsaup USING(PUID)")
pudat <- sqldf("SELECT * from pudat LEFT JOIN pucost USING(PUID)")
#head(pudat)
# there are some NA values for the cost fields that we make 0
pudat[is.na(pudat)] <- 0
# normalise the prob and cost fields
pudat$Impact <- pudat$Impact/max(pudat$Impact)
pudat$AREA <- pudat$AREA/max(pudat$AREA)
pudat$COSTGFCM <- pudat$COSTGFCM/max(pudat$COSTGFCM)
pudat$COSTSAUP <- pudat$COSTSAUP/max(pudat$COSTSAUP)
# set MPA's as status 2
pudat$MPA[(pudat$MPA==1)] <- 2
# tidy column names
colnames(pudat)[1] <- "id"
colnames(pudat)[2] <- "status"
colnames(pudat)[3] <- "prob"
# write pu.csv to cross check
write.table(pudat,file="pu.csv",row.names=FALSE,sep=",",quote=FALSE)
# pu1.dat is cost 1 (GFCM)
pudat1 <- pudat
pudat1$cost <- (0.001 * pudat1$AREA) + pudat1$COSTGFCM
pudat1 <- sqldf("SELECT id,status,prob,cost from pudat1")
write.table(pudat1,file="marxan/all_regions/input/pu1.dat",row.names=FALSE,sep=",",quote=FALSE)
# pu2.dat is cost 2 (SAUP)
pudat2 <- pudat
pudat2$cost <- (0.001 * pudat2$AREA) + pudat2$COSTSAUP
pudat2 <- sqldf("SELECT id,status,prob,cost from pudat2")
write.table(pudat2,file="marxan/all_regions/input/pu2.dat",row.names=FALSE,sep=",",quote=FALSE)
# pu3.dat is cost 1 (GFCM) with no MPA'S
pudat3 <- pudat1
pudat3$status <- 0
write.table(pudat3,file="marxan/all_regions/input/pu3.dat",row.names=FALSE,sep=",",quote=FALSE)
# pu4.dat is cost 2 (SAUP) with no MPA'S
pudat4 <- pudat2
pudat4$status <- 0
write.table(pudat4,file="marxan/all_regions/input/pu4.dat",row.names=FALSE,sep=",",quote=FALSE)

# frequency distribution of non-zero probabilities
nonzerop <- (pudat$prob > 0)
hist(pudat$prob[nonzerop],breaks=100)

# build the spec.dat
write('id,prop,spf,name,ptarget1d',file="marxan/all_regions/input/spec.dat")
iColCount <- length(featurenames)
for (j in (1:iColCount)) # for each conservation feature
{
  if (j < 13)
  {
    # conservation features
    rProp <- 0.4
    rSPF <- 1
    rPtarget1d <- 0.9
  } else {
    # cost features: fields 13, 14, 15
    # we also include the 3 cost fields for reporting purposes (but they won't have targets)
    rProp <- 0
    rSPF <- 0
    rPtarget1d <- 0
  }
  write(paste(j,               # species id
              rProp,           # prop
              rSPF,            # spf
              featurenames[j], # species name
              rPtarget1d,
              sep=","),
        file="marxan/all_regions/input/spec.dat",
        append=TRUE)
}

# build the conservation_features.csv
file.copy("marxan/all_regions/input/spec.dat","marxan/all_regions/input/conservation_features.csv",overwrite=TRUE)

#putable[1,"AREA"]
#putable[1,"COSTGFCM"]
#putable[1,"COSTSAUP"]

# build the puvsp.dat
write('species,pu,amount',file="marxan/all_regions/input/puvsp.dat")
iRowCount <- dim(putable)[1]
iColCount <- length(featurenames)
for (i in 1:iRowCount) # for each planning unit
{
  for (j in 1:iColCount) # for each conservation feature
  {
    if (putable[i,featurenames[j]] > 0)
    {
      write(paste(j,                          # species id
                  putable$PUID[i],            # pu id
                  putable[i,featurenames[j]], # amount
                  sep=","),
            file="marxan/all_regions/input/puvsp.dat",
            append=TRUE)
    }
  }
}

# build the puvsp_sporder.dat
write('species,pu,amount',file="marxan/all_regions/input/puvsp_sporder.dat")
iRowCount <- dim(putable)[1]
iColCount <- length(featurenames)
for (j in 1:iColCount) # for each conservation feature
{
  for (i in 1:iRowCount) # for each planning unit
  {
    if (putable[i,featurenames[j]] > 0)
    {
      write(paste(j,                          # species id
                  putable$PUID[i],            # pu id
                  putable[i,featurenames[j]], # amount
                  sep=","),
            file="marxan/all_regions/input/puvsp_sporder.dat",
            append=TRUE)
    }
  }
}

# build the planning unit layer
writeOGR(v.habitat_new,"marxan/all_regions/pulayer","pulayer",driver="ESRI Shapefile")
# drop all the fields from the planning unit layer except the PUID
pulayer.dbf <- sqldf("SELECT PUID from putable")
write.dbf(pulayer.dbf,"marxan/all_regions/pulayer/pulayer.dbf")
# read the planning unit layer with fields dropped
pulayer <- readOGR("marxan/all_regions/pulayer","pulayer")

# load the ecoregions
ecoregions <- read.csv("GISdata/ecoregion.csv")
ecoregions <- sqldf("SELECT PUID,Eco from ecoregions")
#head(ecoregions)
#max(ecoregions$Eco)

Eco <- ecoregions$Eco
#Eco[1]
#Eco[10000]
#(Eco[1] == 8)
#pudat1[1,]

#pu1 <- sqldf("SELECT PUID from ecoregions WHERE Eco=1")
#pu2 <- sqldf("SELECT PUID from ecoregions WHERE Eco=2")
#pu3 <- sqldf("SELECT PUID from ecoregions WHERE Eco=3")
#pu4 <- sqldf("SELECT PUID from ecoregions WHERE Eco=4")
#pu5 <- sqldf("SELECT PUID from ecoregions WHERE Eco=5")
#pu6 <- sqldf("SELECT PUID from ecoregions WHERE Eco=6")
#pu7 <- sqldf("SELECT PUID from ecoregions WHERE Eco=7")
#pu8 <- sqldf("SELECT PUID from ecoregions WHERE Eco=8")

#iRegion <- 1
#file.copy("input1.dat",paste0("region",iRegion,"/marxan"),overwrite=TRUE)
#file.copy("input2.dat",paste0("region",iRegion,"/marxan"),overwrite=TRUE)
#file.copy(paste0("boundaries/bound_r",iRegion,".dat"),paste0("region",iRegion,"/marxan/input/bound.dat"),overwrite=TRUE)

# build Marxan datasets for each of the 8 ecoregions
for (iRegion in 1:8)
{
  dir.create(paste0("marxan/region",iRegion))
  dir.create(paste0("marxan/region",iRegion,"/input"))
  dir.create(paste0("marxan/region",iRegion,"/output"))
  dir.create(paste0("marxan/region",iRegion,"/pulayer"))
  file.copy("input1.dat",paste0("marxan/region",iRegion),overwrite=TRUE)
  file.copy("input2.dat",paste0("marxan/region",iRegion),overwrite=TRUE)
  file.copy("input3.dat",paste0("marxan/region",iRegion),overwrite=TRUE)
  file.copy("input4.dat",paste0("marxan/region",iRegion),overwrite=TRUE)
  file.copy("MarOpt_v243_Mac64",paste0("marxan/region",iRegion))
  file.copy("MarOpt_v243_Linux64",paste0("marxan/region",iRegion))
  file.copy(paste0("boundaries/bound_r",iRegion,".dat"),paste0("marxan/region",iRegion,"/input/bound.dat"),overwrite=TRUE)
}
# build the (8 * 4) = 32 pu.dat files
for (iRegion in 1:8)
{
  write('id,status,prob,cost',file=paste0("marxan/region",iRegion,"/input/pu1.dat"))
  write('id,status,prob,cost',file=paste0("marxan/region",iRegion,"/input/pu2.dat"))
  write('id,status,prob,cost',file=paste0("marxan/region",iRegion,"/input/pu3.dat"))
  write('id,status,prob,cost',file=paste0("marxan/region",iRegion,"/input/pu4.dat"))
  for (i in 1:length(Eco))
  {
    if (Eco[i]==iRegion)
    {
      write(paste(pudat1[i,1],pudat1[i,2],pudat1[i,3],pudat1[i,4],sep=","),
            file=paste0("marxan/region",iRegion,"/input/pu1.dat"),append=TRUE)
      write(paste(pudat2[i,1],pudat2[i,2],pudat2[i,3],pudat2[i,4],sep=","),
            file=paste0("marxan/region",iRegion,"/input/pu2.dat"),append=TRUE)
      write(paste(pudat3[i,1],pudat3[i,2],pudat3[i,3],pudat3[i,4],sep=","),
            file=paste0("marxan/region",iRegion,"/input/pu3.dat"),append=TRUE)
      write(paste(pudat4[i,1],pudat4[i,2],pudat4[i,3],pudat4[i,4],sep=","),
            file=paste0("marxan/region",iRegion,"/input/pu4.dat"),append=TRUE)
    }
  }
}

#iRegion <- 1
#file.copy("marxan/all_regions/input/spec.dat",paste0("marxan/region",iRegion,"/input/spec.dat"),overwrite=TRUE)

# build the 8 spec.dat files
for (iRegion in 1:8)
{
  file.copy("marxan/all_regions/input/spec.dat",paste0("marxan/region",iRegion,"/input/spec.dat"),overwrite=TRUE)
  file.copy("marxan/all_regions/input/spec.dat",paste0("marxan/region",iRegion,"/input/conservation_features.csv"),overwrite=TRUE)
  #file.copy("marxan/all_regions/input/pu1.dat",paste0("marxan/region",iRegion,"/input/pu1.csv"),overwrite=TRUE)
  #file.copy("marxan/all_regions/input/pu2.dat",paste0("marxan/region",iRegion,"/input/pu2.csv"),overwrite=TRUE)
  #file.copy("marxan/all_regions/input/pu3.dat",paste0("marxan/region",iRegion,"/input/pu3.csv"),overwrite=TRUE)
  #file.copy("marxan/all_regions/input/pu4.dat",paste0("marxan/region",iRegion,"/input/pu4.csv"),overwrite=TRUE)
}
# build the 8 puvsp.dat and puvsp_sporder.dat files
for (iRegion in 1:8)
{
  write('species,pu,amount',file=paste0("marxan/region",iRegion,"/input/puvsp.dat"))
  write('species,pu,amount',file=paste0("marxan/region",iRegion,"/input/puvsp_sporder.dat"))
}
# build the 8 puvsp.dat files
iRowCount <- dim(putable)[1]
iColCount <- length(featurenames)
for (i in 1:iRowCount) # for each planning unit
{
  for (j in 1:iColCount) # for each conservation feature
  {
    if (putable[i,featurenames[j]] > 0)
    {
      write(paste(j,                          # species id
                  putable$PUID[i],            # pu id
                  putable[i,featurenames[j]], # amount
                  sep=","),
            file=paste0("marxan/region",Eco[i],"/input/puvsp.dat"),
            append=TRUE)
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
    if (putable[i,featurenames[j]] > 0)
    {
      write(paste(j,                          # species id
                  putable$PUID[i],            # pu id
                  putable[i,featurenames[j]], # amount
                  sep=","),
            file=paste0("marxan/region",Eco[i],"/input/puvsp_sporder.dat"),
            append=TRUE)
    }
  }
}
# build the 8 planning unit layers
for (iRegion in 1:8)
{
  region_pulayer <- pulayer[Eco==iRegion,]
  writeOGR(region_pulayer,paste0("marxan/region",iRegion,"/pulayer"),"pulayer",driver="ESRI Shapefile")
}

# now run all the (9 * 4) = 36 scenarios
sWorkDir <- "/Users/matt/Documents/zzz"

setwd(paste0(sWorkDir,"/marxan/all_regions"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")
setwd(paste0(sWorkDir,"/marxan/region1"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")
setwd(paste0(sWorkDir,"/marxan/region2"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")
setwd(paste0(sWorkDir,"/marxan/region3"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")
setwd(paste0(sWorkDir,"/marxan/region4"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")
setwd(paste0(sWorkDir,"/marxan/region5"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")
setwd(paste0(sWorkDir,"/marxan/region6"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")
setwd(paste0(sWorkDir,"/marxan/region7"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")
setwd(paste0(sWorkDir,"/marxan/region8"))
system("./MarOpt_v243_Mac64 -s input1.dat")
system("./MarOpt_v243_Mac64 -s input2.dat")
system("./MarOpt_v243_Mac64 -s input3.dat")
system("./MarOpt_v243_Mac64 -s input4.dat")

setwd(sWorkDir)
getwd()

# generate .Rdata files for web apps

sWorkDir <- "/Users/matt/Documents/zzz"
setwd(sWorkDir)
getwd()

# load the simplified pulayers and puoutline
pulayer_all <- readOGR("marxan/all_regions/pulayer","pulayer_simp1k")
puoutline_ <- readOGR("marxan/all_regions/pulayer","puoutline_simp1k")
pulayer_1 <- readOGR("marxan/region1/pulayer","pulayer_simp1k")
pulayer_2 <- readOGR("marxan/region2/pulayer","pulayer_simp1k")
pulayer_3 <- readOGR("marxan/region3/pulayer","pulayer_simp1k")
pulayer_4 <- readOGR("marxan/region4/pulayer","pulayer_simp1k")
pulayer_5 <- readOGR("marxan/region5/pulayer","pulayer_simp1k")
pulayer_6 <- readOGR("marxan/region6/pulayer","pulayer_simp1k")
pulayer_7 <- readOGR("marxan/region7/pulayer","pulayer_simp1k")
pulayer_8 <- readOGR("marxan/region8/pulayer","pulayer_simp1k")

class(pulayer_all)

# load status for each region
pudat_ <- read.csv("marxan/all_regions/input/pu1.dat")
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


pudat_ <- read.csv("marxan/region1/input/pu1.dat")
r1pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("marxan/region2/input/pu1.dat")
r2pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("marxan/region3/input/pu1.dat")
r3pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("marxan/region4/input/pu1.dat")
r4pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("marxan/region5/input/pu1.dat")
r5pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("marxan/region6/input/pu1.dat")
r6pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("marxan/region7/input/pu1.dat")
r7pustatus_ <- unlist(sqldf("SELECT status from pudat_"))
pudat_ <- read.csv("marxan/region8/input/pu1.dat")
r8pustatus_ <- unlist(sqldf("SELECT status from pudat_"))

# save the .RData file with planning unit layers and status vectors
save(pulayer_all,pulayer_1,pulayer_2,pulayer_3,pulayer_4,
     pulayer_5,pulayer_6,pulayer_7,pulayer_8,
     puoutline_,
     rall_pustatus_,r1pustatus_,r2pustatus_,r3pustatus_,r4pustatus_,
     r5pustatus_,r6pustatus_,r7pustatus_,r8pustatus_,
     file="/Users/matt/Documents/zzz/ParameterTesting/p/files/pulayer_.RData")

r2pustatus_[1]
r2pustatus_[2]
length(r2pustatus_)

# experiment with web apps

sRegions <<- c("All regions",
               "Alboran Sea","Algero-Provencal Basin","Tyrrhenian Sea","Tunisian Sea",
               "Adriatic Sea","Ionian Sea","Aegean Sea","Levantine Sea")
length(sRegions)
sRegions[1]
sRegions[9]

# "Run 1","Run 2","Run 3","Run 4","Run 5","Run 6","Run 7","Run 8","Run 9","Run 10"

GenerateRunNames <- function(j)
{
    theruns <- c()
    for (i in 1:j)
    {
        theruns <- c(theruns,paste0("Run ",i))
    }
    return(theruns)
}
GenerateRunNames(10)

as.character(seq(1,10))

round(2.4)
round(2.5)

floor(2.8)

# the coordinates of the plot region measured as a fraction of the figure region
plotPolys(x,plt=c(x1,x2,y1,y2))

# plot ssoln

sMarxanDir <- "/Users/matt/Documents/zzz/ParameterTesting/p/files/all_regions"
swhichparam <- "BLM"
iwhichmap <- 1
pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))

colourpalette <- c("white","green")
tempputable <- sqldf("SELECT PUID from pu_table")
sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_ssoln.csv")
solution_table <- read.csv(sFilename)

colnames(solution_table)[1] <- "PUID"
colnames(solution_table)[2] <- "SSOLN2"
solution_table$SSOLN2 <- as.integer(solution_table$SSOLN2)
values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
values_ <- sqldf("SELECT SSOLN2 from values_")
blueramp <- colorRampPalette(c("white","blue"))(5)
colours <- rep(blueramp[1],nrow(values_))
rMax <- max(values_)
for (j in 1:nrow(values_))
{
  colours[j] <- blueramp[5]
  if (values_[j,] < rMax) { colours[j] <- blueramp[4] }
  if (values_[j,] < (0.7*rMax)) { colours[j] <- blueramp[3] }
  if (values_[j,] < (0.3*rMax)) { colours[j] <- blueramp[2] }
  if (values_[j,] == 0) { colours[j] <- "white" }
  if ((fusempas == TRUE) && (pustatus[j] == 2)) { colours[j] <- "#40E0D0" } # Turquoise
  if (pustatus[j] == 3) { colours[j] <- "gray60" }
}

values_[1,]
length(colours)

# output table, PW

sMarxanDir <- "/Users/matt/Documents/zzz/ParameterTesting/p/files/all_regions"
swhichparam <- "PW"
iwhichmap <- 1
thetable <- read.csv(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv"),stringsAsFactors=FALSE)
head(thetable)

#colnames(thetable)[4] <- "shortfall"
thetable <- sqldf("SELECT PW, cost, Probability1D from thetable")
thetable$PW <- as.character(thetable$PW)
iColumns <- 3

# display cost map
# Error in values_[j, ] : incorrect number of dimensions

colourpalette <- c("white","green")
tempputable <- sqldf("SELECT PUID from pu_table")
sFilename <- paste0(sMarxanDir,"/input/pu",iinputdat,".dat")
solution_table <- read.csv(sFilename)
colnames(solution_table)[1] <- "PUID"
values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
values_ <- unlist(sqldf("SELECT cost from values_"))
blueramp <- colorRampPalette(c("white","blue"))(12)[2:12]
colours <- rep(blueramp[1],length(values_))
rMax <- max(values_)
for (j in 1:length(values_))
{
  colours[j] <- blueramp[round(10 / rMax * values_[j])+1]
  if (values_[j,] == 0) { colours[j] <- "white" }
  if (fusempas == TRUE) { if (pustatus[j] == 2) { colours[j] <- "#40E0D0" } } # Turquoise
  if (pustatus[j] == 3) { colours[j] <- "gray60" }
}

