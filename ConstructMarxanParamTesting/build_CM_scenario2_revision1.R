# Author: Matt Watts
# Date: 27 June 2014
# Project: 
# Purpose: Construct Marxan intput files for Scenario 2 from
#              587 conservation features provided in a DBF table
#              15 cost features provided in a series of tab delimited ascii files
#          We also process the planning unit table into a R binary format for fast map rendering at runtime.


library(foreign) # for read.dbf
library(sqldf)   # for sqldf

sWorkDir <- "/Users/matthewwatts/Documents/zzz/"

setwd(sWorkDir)

# load the conservation features table
pu_x_sp2 <- read.csv(paste0(sWorkDir,"species.names_3digs/matrix2.csv"), stringsAsFactors=FALSE)
colnames(pu_x_sp2)
dim(pu_x_sp2)
pu_x_sp2$pu_id[1]
colnames(pu_x_sp2)[3] # field 3 is pu_id
colnames(pu_x_sp2)[4] # field 4 is area
colnames(pu_x_sp2)[7] # field 7 is the first species
colnames(pu_x_sp2)[593] # field 593 is the last species

pu_x_species_table <- read.dbf(paste0(sWorkDir,"species.names_3digs/NWSMarxanSpeciesProportion_no587.dbf"))
colnames(pu_x_species_table)[2] <- "puid"
colnames(pu_x_species_table)
dim(pu_x_species_table)
head(pu_x_species_table)
# field 2 is pu_id
# field 3 is area
# conservation features start at field 6

pu_x_sp2 <- read.dbf(paste0(sWorkDir,"species.names_3digs/NWSMarxanSpeciesProportion_no587.dbf"), stringsAsFactors=FALSE)

pu_x_species_table$puid <- as.integer(pu_x_species_table$pu_id)

pu_x_species_table[,6]

sample <- sqldf("SELECT puid, X0so_hex from pu_x_species_table")
dim(sample)
head(sample)

sample[1,1]
sample[1,2]

sample[,1]
sample[1,][1]
sample[1,][2]

puids1 <- sqldf("SELECT puid from sample WHERE X0so_hex < 0")
dim(puids1)

sample$puid <- as.integer(sample$puid)

pu_ok <- sqldf("SELECT puid from pu_x_species_table WHERE X0so_hex > -100")
dim(pu_ok)
head(pu_ok)

pu_err <- sqldf("SELECT puid from pu_x_species_table WHERE X0so_hex < 0")
dim(pu_err)
sort(as.integer(unlist(pu_err)))

as.integer(pu_err[,1])
unlist(pu_err[1,1])
as.integer(pu_err[1,1])
unlist(pu_err[1,1][1])
pu_err_ <- as.integer(unlist(pu_err))
length(pu_err_)
head(pu_err)

# cost 1 is area
cost1 <- sqldf("SELECT puid,area from pu_x_species_table")
colnames(cost1)[1] <- "puid"
colnames(cost1)

# load the 15 cost tables
cost2 <- read.table(paste0(sWorkDir,"costlayers/afmanp/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost2)[1] <- "puid"
colnames(cost2)[2] <- "afmanp"
cost2 <- sqldf("SELECT puid,afmanp from cost2")
colnames(cost2)

cost3 <- read.table(paste0(sWorkDir,"costlayers/afmanws/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost3)[1] <- "puid"
colnames(cost3)[2] <- "afmanws"
cost3 <- sqldf("SELECT puid,afmanws from cost3")
colnames(cost3)

cost4 <- read.table(paste0(sWorkDir,"costlayers/kimprawn/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost4)[1] <- "puid"
colnames(cost4)[2] <- "kimprawn"
cost4 <- sqldf("SELECT puid,kimprawn from cost4")
colnames(cost4)

cost5 <- read.table(paste0(sWorkDir,"costlayers/ntdemkg/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost5)[1] <- "puid"
colnames(cost5)[2] <- "ntdemkg"
cost5 <- sqldf("SELECT puid,ntdemkg from cost5")
colnames(cost5)

cost6 <- read.table(paste0(sWorkDir,"costlayers/ntfftkg/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost6)[1] <- "puid"
colnames(cost6)[2] <- "ntfftkg"
cost6 <- sqldf("SELECT puid,ntfftkg from cost6")
colnames(cost6)

cost7 <- read.table(paste0(sWorkDir,"costlayers/ntonlkg/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost7)[1] <- "puid"
colnames(cost7)[2] <- "ntonlkg"
cost7 <- sqldf("SELECT puid,ntonlkg from cost7")
colnames(cost7)

cost8 <- read.table(paste0(sWorkDir,"costlayers/ntspkg/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost8)[1] <- "puid"
colnames(cost8)[2] <- "ntspkg"
cost8 <- sqldf("SELECT puid,ntspkg from cost8")
colnames(cost8)

cost9 <- read.table(paste0(sWorkDir,"costlayers/nttrfkg/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost9)[1] <- "puid"
colnames(cost9)[2] <- "nttrfkg"
cost9 <- sqldf("SELECT puid,nttrfkg from cost9")
colnames(cost9)

cost10 <- read.table(paste0(sWorkDir,"costlayers/oilspill/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost10)[1] <- "puid"
colnames(cost10)[2] <- "oilspill"
cost10 <- sqldf("SELECT puid,oilspill from cost10")
colnames(cost10)

cost11 <- read.table(paste0(sWorkDir,"costlayers/relprospec/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost11)[1] <- "puid"
colnames(cost11)[2] <- "relprospec"
cost11 <- sqldf("SELECT puid,relprospec from cost11")
colnames(cost11)

cost12 <- read.table(paste0(sWorkDir,"costlayers/wakgb60/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost12)[1] <- "puid"
colnames(cost12)[2] <- "wakgb60"
cost12 <- sqldf("SELECT puid,wakgb60 from cost12")
colnames(cost12)

cost13 <- read.table(paste0(sWorkDir,"costlayers/wamac10/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost13)[1] <- "puid"
colnames(cost13)[2] <- "wamac10"
cost13 <- sqldf("SELECT puid,wamac10 from cost13")
colnames(cost13)

cost14 <- read.table(paste0(sWorkDir,"costlayers/wamac60/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost14)[1] <- "puid"
colnames(cost14)[2] <- "wamac60"
cost14 <- sqldf("SELECT puid,wamac60 from cost14")
colnames(cost14)

cost15 <- read.table(paste0(sWorkDir,"costlayers/wands10/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost15)[1] <- "puid"
colnames(cost15)[2] <- "wands10"
cost15 <- sqldf("SELECT puid,wands10 from cost15")
colnames(cost15)

cost16 <- read.table(paste0(sWorkDir,"costlayers/wands60/pu.dat"), sep="\t", header=T, stringsAsFactors=FALSE)
colnames(cost16)[1] <- "puid"
colnames(cost16)[2] <- "wands60"
cost16 <- sqldf("SELECT puid,wands60 from cost16")
colnames(cost16)

# join the cost tables
pu_costs <- sqldf("SELECT * from cost1 LEFT JOIN cost2 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost3 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost4 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost5 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost6 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost7 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost8 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost9 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost10 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost11 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost12 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost13 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost14 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost15 USING(puid)")
pu_costs <- sqldf("SELECT * from pu_costs LEFT JOIN cost16 USING(puid)")
dim(pu_costs)
colnames(pu_costs)

head(pu_costs)
head(pu_matrix)

# join the conservation feature table to the cost table
pu_matrix <- sqldf("SELECT * from pu_x_species_table LEFT JOIN pu_costs USING(puid)")

pu_costs[,1] <- as.integer(pu_costs[,1])
write.dbf(pu_costs,paste0(sWorkDir,"marxan_scenario2/pu_costs.dbf"))
write.dbf(pu_matrix,paste0(sWorkDir,"marxan_scenario2/pu_matrix.dbf"))


# construct the spec.dat file

# get the matrix dimensions
iPuCount <- dim(pu_matrix)[1]
iSpCount <- dim(pu_matrix)[2] - 5

# get rid of factors
for (j in (6:iSpCount))
{
  pu_matrix[,j] <- as.numeric(pu_matrix[,j])
}
# make puid an integer
pu_matrix[,2] <- as.integer(pu_matrix[,2])

# construct the puvsp.dat file
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp.dat"))
for (i in (1:iPuCount)) # for each planning unit
{
  iSpId <- 0
  
  for (j in (6:iSpCount)) # for each conservation feature
  {
    iSpId <- iSpId + 1
    
    if (is.na(pu_matrix[i,j]) != TRUE)
    {
      if (pu_matrix[i,j] > 0)
      {
        write(paste(iSpId,           # species id
                    pu_matrix[i,2],  # pu id
                    pu_matrix[i,j],  # amount
                    sep=","),
              file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp.dat"),
              append=TRUE)
      }
    }
  }
}

# construct the puvsp_sporder.dat file
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp_sporder.dat"))
iSpId <- 0
for (j in (3:iSpCount)) # for each conservation feature
{
  iSpId <- iSpId + 1
  
  for (i in (1:iPuCount)) # for each planning unit
  {
    if (is.na(pu_matrix[i,j]) != TRUE)
    {
      if (pu_matrix[i,j] > 0)
      {
        write(paste(iSpId,           # species id
                    pu_matrix[i,2],  # pu id
                    pu_matrix[i,j],  # amount
                    sep=","),
              file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp_sporder.dat"),
              append=TRUE)
      }
    }
  }
}

