# Author: Matt Watts
# Date: 6 June 2014
# Project: 
# Purpose: construct Marxan intput files for Scenario 2 to 4 from tabular data computed with
# ArcMap 10 TabulateAreas

library(foreign) # for read.dbf
library(sqldf)   # for sqldf

sWorkDir <- "/Users/matthewwatts/Documents/zzz/"
sPuTable <- "_20140529_gda94AustAlbers/_20140529_gda94AustAlbers.dbf"

setwd(sWorkDir)

# PU_Kimb_20140529_gda94AustAlbers has the planning unit layer
pu_table <- read.dbf(paste0(sWorkDir,sPuTable))
pu_table <- sqldf("SELECT PU_ID, Hectares from pu_table") # extract pu id and area
colnames(pu_table)[2] <- "COST"
colnames(pu_table)

# scenario2_tabarea_tables has the tabarea files
# load the conservation feature tabarea files
# Bathymetry
pu_x_bath10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_bath10m.dbf"))
colnames(pu_x_bath10m_table)

# Secondary Compartments
pu_x_seccomp10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_seccomp10m.dbf"))
colnames(pu_x_seccomp10m_table)

# Geomorphology Shelf
pu_x_geomorphsh10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_geomorphsh10m.dbf"))
colnames(pu_x_geomorphsh10m_table)

# Islands
# Note: we are deleting 401,402 from pu_x_islands10m.dbf, and replacing them with
# 401,402,403 from pu_x_kimbislands.dbf
#pu_x_islands10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_islands10m.dbf"))
pu_x_islands10m_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_kimbislands.dbf"))
colnames(pu_x_islands10m_table)

# Mangroves
pu_x_mang10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_mang10m.dbf"))
colnames(pu_x_mang10m_table)

# Coastal Habitat Mudflats
pu_x_chmud10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_chmud10m.dbf"))
colnames(pu_x_chmud10m_table)

# Coastal Habitat Sandflats
pu_x_chsandf10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_chsandf10m.dbf"))
colnames(pu_x_chsandf10m_table)

# Coastal Habitat Saltmarsh
pu_x_chsaltm10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_chsaltm10m.dbf"))
colnames(pu_x_chsaltm10m_table)

# Coastal Habitat Seagrass
pu_x_chseag10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_chseag10m.dbf"))
colnames(pu_x_chseag10m_table)

# Coastal Habitat Fringing Coral Reef
pu_x_chfrcor10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_chfrcor10m.dbf"))
colnames(pu_x_chfrcor10m_table)

# Coastal Habitat Beach
pu_x_chbeach10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_chbeach10m.dbf"))
colnames(pu_x_chbeach10m_table)

# Geomorphic estuary habitats
pu_x_geomorphest10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_geomorphest10m.dbf"))
colnames(pu_x_geomorphest10m_table)

# Biodiversity hotspots
pu_x_biohot_table <- read.dbf(paste0(sWorkDir,"scenario3_tabarea_tables/pu_x_biohot.dbf"))
colnames(pu_x_biohot_table)

# Turtle nesting beaches
pu_x_turtnb_table <- read.dbf(paste0(sWorkDir,"scenario3_tabarea_tables/pu_x_turtnb.dbf"))
colnames(pu_x_turtnb_table)

# Cultural values and sites
pu_x_heaco_table <- read.dbf(paste0(sWorkDir,"scenario3_tabarea_tables/pu_x_heaco.dbf"))
colnames(pu_x_heaco_table)

# load scenario 4 conservation features
pu_x_timwill_conservation_features_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_timwill_conservation_features.dbf"))
colnames(pu_x_timwill_conservation_features_table)

# load scenario 4 cost features
# 901, 902, 903: Fishing camps general range of fishing
pu_x_recfishcamps_cost_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_recfishcamps_cost.dbf"))
colnames(pu_x_recfishcamps_cost_table)

# 1200: Aquaculture exclustion area
pu_x_pearling_cost_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_pearling_cost.dbf"))
colnames(pu_x_pearling_cost_table)

# 1401, 1402, 1403
pu_x_prawntrawl_cost_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_prawntrawl_cost.dbf"))
colnames(pu_x_prawntrawl_cost_table)

# 1501, 1502, 1503, 1504
pu_x_kgbf_cost_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_kgbf_cost.dbf"))
colnames(pu_x_kgbf_cost_table)

# 1601, 1602, 1603, 1604
pu_x_pfamackim_cost_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_pfamackim_cost.dbf"))
colnames(pu_x_pfamackim_cost_table)

# 1701
pu_x_pfandskim_cost_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_pfandskim_cost.dbf"))
colnames(pu_x_pfandskim_cost_table)

# 1801, 1802, 1803
pu_x_cto_cost_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_cto_cost.dbf"))
colnames(pu_x_cto_cost_table)

# 1901
pu_x_twctoanchorage_cost_table <- read.dbf(paste0(sWorkDir,"scenario4_tabarea_tables/pu_x_twctoanchorage_cost.dbf"))
colnames(pu_x_twctoanchorage_cost_table)

# join the conservation feature tables to the pu table
pu_join <- sqldf("SELECT * from pu_table LEFT JOIN pu_x_bath10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_seccomp10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_geomorphsh10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_islands10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_mang10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_chmud10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_chsandf10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_chsaltm10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_chseag10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_chfrcor10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_chbeach10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_geomorphest10m_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_biohot_table USING(PU_ID)")

# recfishcamps is a cost feature. we insert it here so our feature codes are linear
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_recfishcamps_cost_table USING(PU_ID)")

# join the rest of the conservation features
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_turtnb_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_heaco_table USING(PU_ID)")

# pu_x_pearling_cost_table is a cost feature. we insert it here so our feature codes are linear
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_pearling_cost_table USING(PU_ID)")

# join the last of the conservation features
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_timwill_conservation_features_table USING(PU_ID)")

# join the cost feature tables to the conservation features
#pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_recfishcamps_cost_table USING(PU_ID)")
#pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_pearling_cost_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_prawntrawl_cost_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_kgbf_cost_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_pfamackim_cost_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_pfandskim_cost_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_cto_cost_table USING(PU_ID)")
pu_join <- sqldf("SELECT * from pu_join LEFT JOIN pu_x_twctoanchorage_cost_table USING(PU_ID)")

colnames(pu_join)

# write joined table to disk
write.dbf(pu_join,paste0(sWorkDir,"scenario4_tabarea_tables/pu_join.dbf"))

# join the cost features table to the pu table
pu_cost <- sqldf("SELECT * from pu_table LEFT JOIN pu_x_recfishcamps_cost_table USING(PU_ID)")
pu_cost <- sqldf("SELECT * from pu_cost LEFT JOIN pu_x_pearling_cost_table USING(PU_ID)")
pu_cost <- sqldf("SELECT * from pu_cost LEFT JOIN pu_x_prawntrawl_cost_table USING(PU_ID)")
pu_cost <- sqldf("SELECT * from pu_cost LEFT JOIN pu_x_kgbf_cost_table USING(PU_ID)")
pu_cost <- sqldf("SELECT * from pu_cost LEFT JOIN pu_x_pfamackim_cost_table USING(PU_ID)")
pu_cost <- sqldf("SELECT * from pu_cost LEFT JOIN pu_x_pfandskim_cost_table USING(PU_ID)")
pu_cost <- sqldf("SELECT * from pu_cost LEFT JOIN pu_x_cto_cost_table USING(PU_ID)")
pu_cost <- sqldf("SELECT * from pu_cost LEFT JOIN pu_x_twctoanchorage_cost_table USING(PU_ID)")

colnames(pu_cost)

# write joined table to disk
write.dbf(pu_cost,paste0(sWorkDir,"scenario4_tabarea_tables/pu_cost.dbf"))

# create the pu.dat file and write it to disk
pudat <- cbind(pu_table,rep(0,dim(pu_table)[1]))
colnames(pudat)[1] <- "id"
colnames(pudat)[2] <- "cost"
colnames(pudat)[3] <- "status"
write.csv(pudat,paste0(sWorkDir,"/marxan_scenario4a/input/pu.dat"),quote=FALSE,row.names=FALSE)

# find the dimensions of the joined matrices
iRowCount <- dim(pu_join)[1]
iColCount <- dim(pu_join)[2]
iCostRowCount <- dim(pu_cost)[1]
iCostColCount <- dim(pu_cost)[2]

# load the species names
specname <- read.csv(paste0(sWorkDir,"conservation_feature_names_scenario4a.csv"))
colnames(specname)
specname$spname
specname$target
#colnames(pu_join)
# "MARXA_101"
#  123456789
#j <- 3
#substr(colnames(pu_join)[j],7,nchar(colnames(pu_join)[3]))
# "MCODE_1001"
#  1234567890
#j <- 74
#substr(colnames(pu_join)[j],7,nchar(colnames(pu_join)[j]))

# write the spec.dat file to disk
write('id,prop,spf,name',file=paste0(sWorkDir,"/marxan_scenario4a/input/spec.dat"))
for (j in (3:iColCount)) # for each conservation feature
{
  write(paste(substr(colnames(pu_join)[j],7,nchar(colnames(pu_join)[j])), # species id
        specname$target[j-2],1 * specname$spf[j-2],              # prop, spf
        as.character(specname$spname)[j-2],                               # species name
        sep=","),
        file=paste0(sWorkDir,"/marxan_scenario4a/input/spec.dat"),
        append=TRUE)
}

# write the cost features file to disk
write('id',file=paste0(sWorkDir,"/marxan_scenario4a/cost_features.csv"))
write('AREA',file=paste0(sWorkDir,"/marxan_scenario4a/cost_features.csv"),append=TRUE)
for (j in (3:iCostColCount)) # for each conservation feature
{
  write(substr(colnames(pu_cost)[j],7,nchar(colnames(pu_cost)[j])), # cost id
        file=paste0(sWorkDir,"/marxan_scenario4a/cost_features.csv"),
        append=TRUE)
}

# write the puvsp.dat to disk
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario4a/input/puvsp.dat"))
for (i in (1:iRowCount)) # for each planning unit
{
  for (j in (3:iColCount)) # for each conservation feature
  {
    if (is.na(pu_join[i,j]) != TRUE)
    {
      if (pu_join[i,j] > 0)
      {
        write(paste(substr(colnames(pu_join)[j],7,nchar(colnames(pu_join)[j])), # species id
                    pu_join[i,1],                                               # pu id
                    (pu_join[i,j]/10000),                                       # amount (hectares)
                    sep=","),
              file=paste0(sWorkDir,"/marxan_scenario4a/input/puvsp.dat"),
              append=TRUE)
      }
    }
  }
}

# write the puvsp_sporder.dat to disk
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario4a/input/puvsp_sporder.dat"))
for (j in (3:iColCount)) # for each conservation feature
{
  for (i in (1:iRowCount)) # for each planning unit
  {
    if (is.na(pu_join[i,j]) != TRUE)
    {
      if (pu_join[i,j] > 0)
      {
        write(paste(substr(colnames(pu_join)[j],7,nchar(colnames(pu_join)[j])), # species id
                    pu_join[i,1],                                               # pu id
                    (pu_join[i,j]/10000),                                       # amount (hectares)
                    sep=","),
              file=paste0(sWorkDir,"/marxan_scenario4a/input/puvsp_sporder.dat"),
              append=TRUE)
      }
    }
  }
}

# add the planning units for terrestrial reserves
pudat <- read.csv(paste0(sWorkDir,"/marxan_scenario4a/input/pu.dat"))
pudat$status <- 0

puterrres <- read.csv(paste0(sWorkDir,"/scenario4_tabarea_tables/status_locked_in_terrestrial_reserves.csv"))
#puterrres <- as.matrix(cbind(puterrres[,1],rep(0,dim(puterrres)[1]),puterrres[,2]))
#colnames(puterrres) <- colnames(pudat)

# save pu.dat with terrestrial reserves
#pudat_terrres <- rbind(pudat,puterrres)
#write.csv(pudat_terrres,paste0(sWorkDir,"/marxan_scenario4/input/pu_terrres.dat"),quote=FALSE,row.names=FALSE)

# adjust the status for areas locked in and locked out
pu_scen3status <- read.csv(paste0(sWorkDir,"/scenario4_tabarea_tables/status_locked_in_locked_out_scenario3.csv"))
#pu_icakgbf <- read.csv(paste0(sWorkDir,"/scenario4_tabarea_tables/status_locked_in_identified-cons-areas-kgbf.csv"))
pu_aqua <- read.csv(paste0(sWorkDir,"/scenario4_tabarea_tables/status_locked_out_aquaculture.csv"))
pu_pearling <- read.csv(paste0(sWorkDir,"/scenario4_tabarea_tables/status_locked_out_pearling.csv"))

head(pu_scen3status)
head(pu_icakgbf)
head(pu_aqua)
head(pu_pearling)

head(pudat_terrres)


for (i in 1:nrow(puterrres))
{
    if (puterrres$status[i] > 0)
    {
        iWhich <- which(pudat$id==puterrres$PUID[i])
        pudat$status[iWhich] <- puterrres$status[i]
    }
}
for (i in 1:nrow(pu_scen3status))
{
    if (pu_scen3status$status[i] > 0)
    {
        iWhich <- which(pudat$id==pu_scen3status$PUID[i])
        pudat$status[iWhich] <- pu_scen3status$status[i]
    }
}
#for (i in 1:nrow(pu_icakgbf))
#{
#    iWhich <- which(pudat_terrres$id==pu_icakgbf$PUID[i])
#    pudat$status[iWhich] <- 2
#}
for (i in 1:nrow(pu_aqua))
{
    iWhich <- which(pudat$id==pu_aqua$PUID[i])
    pudat$status[iWhich] <- 3
}
for (i in 1:nrow(pu_pearling))
{
    iWhich <- which(pudat$id==pu_pearling$PUID[i])
    pudat$status[iWhich] <- 3
}
write.csv(pudat,paste0(sWorkDir,"/marxan_scenario4a/input/pu.dat"),quote=FALSE,row.names=FALSE)
pudat$status[2724]

#pu_icakgbf$PUID[1]
#pudat_terrres$id

#dim(puterrres)[1]
#rep(0,dim(puterrres)[1])

#colnames(pudat)
#tail(pudat)
#pudat <- rbind(pudat,puterrres)
#colnames(pudat)
#tail(pudat)

#puterrres[1,1]
#puterrres[2,1]
#puterrres[,1]

#pudat <- sqldf("INSERT INTO pudat (id,cost,status) VALUES (10002,0,2)")

#test <- rbind(pudat,puterrres)
#tail(test)
#for (i in 1:nrow(puterrres))
#{
#    pudat <- sqldf(paste0("INSERT INTO pudat (id,cost,status) VALUES (",puterrres[i,1],",",0,",",puterrres[i,2],")"))
    # INSERT INTO pudat VALUES (value1,value2,value3,...)
#}
#Bensdata/
#PU_Kimb_20140529_gda94AustAlbers_Plus_586SpeciesModelsProportionOuccurance.dbf
#PU_ID is column 3
#species start column 11: oso_hexaca

pu_x_bensdata <- read.dbf(paste0(sWorkDir,"Bensdata/PU_Kimb_20140529_gda94AustAlbers_Plus_586SpeciesModelsProportionOuccurance.dbf"))
dim(pu_x_bensdata)
colnames(pu_x_bensdata)
iRowCount <- dim(pu_x_bensdata)[1]
iColCount <- dim(pu_x_bensdata)[2]


# write the spec.dat file to disk
iSPCode <- 2000
write('id,prop,spf,name',file=paste0(sWorkDir,"/marxan_scenario4a/input/spec_ben.csv"))
for (j in (11:iColCount)) # for each conservation feature
{
  iSPCode <- iSPCode + 1
  
  write(paste(iSPCode,                     # species id
              0.1,1,                       # prop, spf
              colnames(pu_x_bensdata)[j],  # species name
              sep=","),
        file=paste0(sWorkDir,"/marxan_scenario4a/input/spec_ben.csv"),
        append=TRUE)
}


# write the puvsp.dat to disk
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario4a/input/puvsp_ben.dat"))
for (i in (1:iRowCount)) # for each planning unit
{
  iSPCode <- 2000
  
  for (j in (11:iColCount)) # for each conservation feature
  {
    iSPCode <- iSPCode + 1
    
    if (is.na(pu_x_bensdata[i,j]) != TRUE)
    {
      if (pu_x_bensdata[i,j] > 0)
      {
        write(paste(iSPCode, # species id
                    pu_x_bensdata[i,3],                                         # pu id
                    (pu_x_bensdata[i,j]),                                       # amount
                    sep=","),
              file=paste0(sWorkDir,"/marxan_scenario4a/input/puvsp_ben.dat"),
              append=TRUE)
      }
    }
  }
}

# write the puvsp_sporder.dat to disk
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario4a/input/puvsp_ben_sporder.dat"))
iSPCode <- 2000
for (j in (11:iColCount)) # for each conservation feature
{
  iSPCode <- iSPCode + 1
  
  for (i in (1:iRowCount)) # for each planning unit
  {
    if (is.na(pu_x_bensdata[i,j]) != TRUE)
    {
      if (pu_x_bensdata[i,j] > 0)
      {
        write(paste(iSPCode,        # species id
                    pu_x_bensdata[i,3],   # pu id
                    (pu_x_bensdata[i,j]), # amount (hectares)
                    sep=","),
              file=paste0(sWorkDir,"/marxan_scenario4a/input/puvsp_ben_sporder.dat"),
              append=TRUE)
      }
    }
  }
}
