# Author: Matt Watts
# Date: 6 June 2014
# Project: 
# Purpose: construct Marxan intput files for Scenario 2 from tabular data computed with
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
pu_x_islands10m_table <- read.dbf(paste0(sWorkDir,"scenario2_tabarea_tables/pu_x_islands10m.dbf"))
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

colnames(pu_join)

# write joined table to disk
write.dbf(pu_join,paste0(sWorkDir,"scenario2_tabarea_tables/pu_join.dbf"))

# create the pu.dat file and write it to disk
pudat <- cbind(pu_table,rep(0,dim(pu_table)[1]))
colnames(pudat)[1] <- "id"
colnames(pudat)[2] <- "cost"
colnames(pudat)[3] <- "status"
write.csv(pudat,paste0(sWorkDir,"/marxan_scenario2/input/pu.dat"),quote=FALSE,row.names=FALSE)

# find the dimensions of the joined matrix
iRowCount <- dim(pu_join)[1]
iColCount <- dim(pu_join)[2]

# load the species names
specname <- read.csv(paste0(sWorkDir,"conservation_feature_names_scenario2.csv"))

# write the spec.dat file to disk
write('id,prop,spf,name',file=paste0(sWorkDir,"/marxan_scenario2/input/spec.dat"))
for (j in (3:iColCount)) # for each conservation feature
{
  write(paste(substr(colnames(pu_join)[j],7,nchar(colnames(pu_join)[3])), # species id
        0.1,1,                                                            # prop, spf
        as.character(specname$spname)[j-2],                               # species name
        sep=","),
        file=paste0(sWorkDir,"/marxan_scenario2/input/spec.dat"),
        append=TRUE)
}

# write the puvsp.dat to disk
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp.dat"))
for (i in (1:iRowCount)) # for each planning unit
{
  for (j in (3:iColCount)) # for each conservation feature
  {
    if (is.na(pu_join[i,j]) != TRUE)
    {
      if (pu_join[i,j] > 0)
      {
        write(paste(substr(colnames(pu_join)[j],7,nchar(colnames(pu_join)[3])), # species id
                    pu_join[i,1],                                               # pu id
                    (pu_join[i,j]/10000),                                       # amount (hectares)
                    sep=","),
              file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp.dat"),
              append=TRUE)
      }
    }
  }
}

# write the puvsp_sporder.dat to disk
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp_sporder.dat"))
for (j in (3:iColCount)) # for each conservation feature
{
  for (i in (1:iRowCount)) # for each planning unit
  {
    if (is.na(pu_join[i,j]) != TRUE)
    {
      if (pu_join[i,j] > 0)
      {
        write(paste(substr(colnames(pu_join)[j],7,nchar(colnames(pu_join)[3])), # species id
                    pu_join[i,1],                                               # pu id
                    (pu_join[i,j]/10000),                                       # amount (hectares)
                    sep=","),
              file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp_sporder.dat"),
              append=TRUE)
      }
    }
  }
}



