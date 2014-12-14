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

# rename the pu_id field in the matrix for consistency with the cost tables
colnames(pu_x_sp2)[3] <- "puid"

# cost 1 is area
cost1 <- sqldf("SELECT puid,area from pu_x_sp2")
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

# save the joined costs table
write.dbf(pu_costs,paste0(sWorkDir,"marxan_scenario2/pu_costs.dbf"))

# join the conservation feature table to the cost table
pu_matrix <- sqldf("SELECT * from pu_x_sp2 LEFT JOIN pu_costs USING(puid)")
dim(pu_matrix)
colnames(pu_matrix)

# check the field integrity after the join
colnames(pu_x_sp2)[3] # field 3 is puid
colnames(pu_x_sp2)[7] # field 7 is the first species
colnames(pu_x_sp2)[593] # field 593 is the last species
colnames(pu_matrix)[3] # field 3 is puid
colnames(pu_matrix)[7] # field 7 is the first species
colnames(pu_matrix)[593] # field 593 is the last species
colnames(pu_matrix)[594] # field 594 is the first cost
colnames(pu_matrix)[609] # field 609 is the last cost

# determine the range of values for the species
write('speciesid,speciesname,min,max,mean,sd,var',file=paste0(sWorkDir,"/marxan_scenario2/species_stats.csv"))
for (i in 7:593)
{
  write(paste(i-6,
              colnames(pu_matrix)[i],
              min(pu_matrix[,i]),
              max(pu_matrix[,i]),
              mean(pu_matrix[,i]),
              sd(pu_matrix[,i]),
              var(pu_matrix[,i]),
              sep=","),
        file=paste0(sWorkDir,"/marxan_scenario2/species_stats.csv"),
        append=TRUE)
}

# determine the range of values for the costs
write('costid,costname,min,max,mean,sd,var',file=paste0(sWorkDir,"/marxan_scenario2/costs_stats.csv"))
for (i in 594:609)
{
  write(paste(i-593,
              colnames(pu_matrix)[i],
              min(pu_matrix[,i]),
              max(pu_matrix[,i]),
              mean(pu_matrix[,i]),
              sd(pu_matrix[,i]),
              var(pu_matrix[,i]),
              sep=","),
        file=paste0(sWorkDir,"/marxan_scenario2/costs_stats.csv"),
        append=TRUE)
}

# save the joined species and costs table
write.dbf(pu_matrix,paste0(sWorkDir,"marxan_scenario2/pu_matrix.dbf"))

# get the matrix dimensions
iPuCount <- dim(pu_matrix)[1]
iPuCount
iColCount <- dim(pu_matrix)[2]
iColCount
iSpCount <- iColCount - 6
iSpCount

# construct the spec.dat file
write('id,prop,spf,name',file=paste0(sWorkDir,"/marxan_scenario2/input/spec.dat"))
iSpId <- 0
for (j in (7:iColCount)) # for each conservation feature
{
  iSpId <- iSpId + 1
  
  if (iSpId < 588)
  {
    # this is a species: use a target
    rProp <- 0.1
    rSPF <- 1
  } else {
    # this is a cost: don't use a target
    rProp <- 0
    rSPF <- 0
  }
  
  write(paste(iSpId,rProp,rSPF,colnames(pu_matrix)[j],sep=","),
        file=paste0(sWorkDir,"/marxan_scenario2/input/spec.dat"),
        append=TRUE)
}

# construct the puvsp.dat file
write('species,pu,amount',file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp.dat"))
for (i in (1:iPuCount)) # for each planning unit
{
  iSpId <- 0
  
  for (j in (7:iColCount)) # for each conservation feature
  {
    iSpId <- iSpId + 1
    
    if (is.na(pu_matrix[i,j]) != TRUE)
    {
      if (pu_matrix[i,j] > 0)
      {
        write(paste(iSpId,           # species id
                    pu_matrix[i,3],  # pu id
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
for (j in (7:iColCount)) # for each conservation feature
{
  iSpId <- iSpId + 1
  
  for (i in (1:iPuCount)) # for each planning unit
  {
    if (is.na(pu_matrix[i,j]) != TRUE)
    {
      rAmount <- pu_matrix[i,j]
        
      if (rAmount > 0)
      {
          write(paste(iSpId,           # species id
                      pu_matrix[i,3],  # pu id
                      rAmount,         # amount
                      sep=","),
                file=paste0(sWorkDir,"/marxan_scenario2/input/puvsp_sporder.dat"),
                append=TRUE)
      }
    }
  }
}

# construct the normalised costs table as an R binary object
pu_cost_norm <- pu_costs
for (i in 2:dim(pu_cost_norm)[2])
{
  pu_cost_norm[,i] <- pu_cost_norm[,i] / max(pu_cost_norm[,i])
  cat(paste0(max(pu_cost_norm[,i]),"\n"))
}

# save the R binary object with normalised costs
save(pu_cost_norm,file=paste0(sWorkDir,"/marxan_scenario2/input/pu_cost_norm.Rdata"))


# construct the costs weightings table
write('Cost.id,Cost.name,Weighting',file=paste0(sWorkDir,"/marxan_scenario2/input/costweight.dat"))
for (i in 2:dim(pu_cost_norm)[2])
{
  write(paste(i-1,colnames(pu_cost_norm)[i],1,sep=","),
        file=paste0(sWorkDir,"/marxan_scenario2/input/costweight.dat"),
        append=TRUE)
  
}

# load the planning unit status from scenario 1
pudat <- read.csv(paste0(sWorkDir,"/SenarioMatt/input/pu.dat"), stringsAsFactors=FALSE)

# compute the planning unit cost
pudat$cost <- 0
for (i in 2:dim(pu_cost_norm)[2])
{
  pudat$cost <- pudat$cost + pu_cost_norm[,i]
}
min(pudat$cost)
max(pudat$cost)

# construct the pu.dat file
write.csv(pudat,paste0(sWorkDir,"/marxan_scenario2/input/pu.dat"),quote=FALSE,row.names=FALSE)

# construct the R binary object the represents planning unit status
pustatus_ <<- unlist(sqldf("SELECT status from pudat"))

# construct the R binary objects that represent the planning unit layer and outline shape files
require(sp)
require(maptools)
require(PBSmapping)

pulayer <- readShapePoly(paste0(sWorkDir,"/SenarioMatt/pulayer/pulayer.shp"))
pulayer_ <- SpatialPolygons2PolySet(pulayer)
puoutline <- readShapePoly(paste0(sWorkDir,"/SenarioMatt/pulayer/puoutline.shp"))
puoutline <- SpatialPolygons2PolySet(puoutline)

# save the R binary object with pulayer, puoutline, and pustatus
save(pulayer_,puoutline,pustatus_,file=paste0(sWorkDir,"/marxan_scenario2/pulayer/pulayer.Rdata"))

# initialise output files for "RunMarxan" app
# this generates output files with default parameter values for the app to load at runtime
GetOutputFileext <- function(sMarxanDir,sParam)
# For the specified Marxan output file, return the file extension (.csv or .txt)
# Scan input.dat for the parameter,
# if value = 1, .dat, tab delimited, no header
# if value = 2, .txt, comma delimited (Qmarxan sets this value)
# if value = 3, .csv, comma delimited
{
  inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
  iParam <- which(regexpr(sParam,inputdat)==1)
  
  iValue <- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])
  
  if (iValue == 1)
  {
    return(".dat")
  }
  if (iValue == 2)
  {
    return(".txt")
  }
  if (iValue == 3)
  {
    return(".csv")
  }
}
PadInt <- function(iRunNumber)
{
  sFilename <- ""
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste0(sFilename,"0")
    }
  }
  sFilename <- paste0(sFilename,iRunNumber)  
}
JoinParallelResults <- function()
{
    # combine the summary tables
    sumtable <- c()
    for (i in 1:10)
    {
        sumtable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_sum.csv"))
        sumtable <- rbind(sumtable,sumtable_)
    }
    for (i in 1:100)
    {
        sumtable[i,1] <- i
    }
    write.csv(sumtable,
              paste0(sMarxanDir,"/output/output_sum.csv"),
              quote=FALSE,row.names=FALSE)

    # detect best solution
    iBest <- which(sumtable[,2]==min(sumtable[,2]))
    if (length(iBest) > 0)
    {
        iBest <- iBest[1]
    }
    
    # rename mv files and solution files
    iSol <- 0
    for (i in 1:10)
    {
        for (j in 1:10)
        {
            iSol <- iSol + 1
        
            file.rename(paste0(sMarxanDir,"/output/output",i,"_mv",PadInt(j),".csv"),
                        paste0(sMarxanDir,"/output/output_mv",PadInt(iSol),".csv"))
        
            file.rename(paste0(sMarxanDir,"/output/output",i,"_r",PadInt(j),".csv"),
                        paste0(sMarxanDir,"/output/output_r",PadInt(iSol),".csv"))
        }
    }

    # copy _mvbest and _best files
    file.copy(paste0(sMarxanDir,"/output/output_mv",PadInt(iBest),".csv"),
              paste0(sMarxanDir,"/output/output_mvbest.csv"),
              overwrite=TRUE)
    file.copy(paste0(sMarxanDir,"/output/output_r",PadInt(iBest),".csv"),
              paste0(sMarxanDir,"/output/output_best.csv"),
              overwrite=TRUE)
    
    # join ssoln files
    ssolntable <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
    colnames(ssolntable)[2] <- "SS1"
    for (i in 2:10)
    {
        ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
        ssolntable <- sqldf("SELECT * from ssolntable LEFT JOIN ssolntable_ USING(planning_unit)")
        colnames(ssolntable)[ncol(ssolntable)] <- paste0("SS",i)
    }
    ssolntable$number <- ssolntable$SS1 + ssolntable$SS2 + ssolntable$SS3 + ssolntable$SS4 + ssolntable$SS5 + ssolntable$SS6 + ssolntable$SS7 + ssolntable$SS8 + ssolntable$SS9 + ssolntable$SS10
    ssolntable <- sqldf("SELECT planning_unit, number from ssolntable")
    write.csv(ssolntable,
                  paste0(sMarxanDir,"/output/output_ssoln.csv"),
                  quote=FALSE,row.names=FALSE)

    # join cluster files: text parse
    outfile <- file(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),"w")
    iRow <- 0
    for (i in 1:10)
    {
        infile <- file(paste0(sMarxanDir,"/output/output",i,"_solutionsmatrix.csv"),"r")
        # read header row
        sLine <- readLines(con=infile,n=1)
  
        # write header row if i == 1
        if (i == 1)
        {
            write(sLine,file=outfile)
        }
    
        for (j in 1:10)
        {
            sLine <- readLines(con=infile,n=1)
            iLen <- nchar(sLine)
            if (j < 10)
            {
                # S1..S9 : remove 3 chars
                sLine <- substr(sLine, 4, iLen)          
            } else {
                # S10 : remove 4 chars
                sLine <- substr(sLine, 5, iLen)
            }
            iRow <- iRow + 1
            write(paste0("S",iRow,",",sLine),file=outfile,append=TRUE)
        }
        close(infile)
    }
    close(outfile)
}
ImportOutputsCsvToShpDbf <- function(sPuShapeFileDbf, sMarxanDir, sPUID)
# Imports the relevant contents of output files to the planning unit shape file dbf.
{
  # load and prepare pu_table
  pu_table <- read.dbf(sPuShapeFileDbf)
  pu_table <- sqldf(paste("SELECT ", sPUID, " from pu_table",sep=""))
  colnames(pu_table)[1] <- "PUID"
                    
  pu_table$PUID <- as.integer(pu_table$PUID)
  
  # load and prepare ssoln_table
  ssoln_table <- read.csv(paste(sMarxanDir,"/output/output_ssoln",GetOutputFileext(sMarxanDir,"SAVESUMSOLN"),sep=""))
  colnames(ssoln_table)[1] <- "PUID"
  colnames(ssoln_table)[2] <- "SSOLN2"
   ssoln_table$SSOLN2 <- as.integer(ssoln_table$SSOLN2)
  
  # join pu_table and ssoln_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table USING(PUID)")
  
  # load and prepare best_table
  best_table <- read.csv(paste(sMarxanDir,"/output/output_best",GetOutputFileext(sMarxanDir,"SAVEBEST"),sep=""))
  best_table$BESTSOLN <- as.integer(best_table$SOLUTION + 1)
  best_table <- sqldf("SELECT PUID, BESTSOLN from best_table")
  
  # join pu_table and best_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table USING(PUID)")
  
  # save the new pu_table
  colnames(pu_table)[1] <- sPUID
  write.dbf(pu_table,sPuShapeFileDbf)  
}

library(foreach)
library(doMC)

registerDoMC(10)  # the number of CPU cores

# default parameter values
rblm <<- 1
sMarxanDir <<- paste0(sWorkDir,"zzz/p/files")

                inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                iBLMparam <- which(regexpr("BLM",inputdat)==1)
                inputdat[iBLMparam] <- paste0("BLM ",rblm)

                randomseeds <- round(runif(10)*100000)
                
                # run Marxan
                foreach(i=1:10) %dopar%
                {
                    # set parameters for multi core
                    iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
                    iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
                    iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                    iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                    iRANDSEEDparam <- which(regexpr("RANDSEED",inputdat)==1)
                    inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
                    inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
                    inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",i)         
                    inputdat[iNUMREPSparam] <- "NUMREPS 10"
                    inputdat[iRANDSEEDparam] <- paste0("RANDSEED ",randomseeds[i])
                    writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input.dat"))
                
                    cat(paste0("getwd ",getwd(),"\n"))
                    setwd(paste0(sMarxanDir,"/core",i))
                    cat(paste0("getwd ",getwd(),"\n"))
                    cat(paste0(".Platform$pkgType ",.Platform$pkgType,"\n"))
                    if (.Platform$pkgType == "mac.binary")
                    {
                        cat("mac\n")
                        system("./MarOpt_v243_Mac64 -s")
                    } else {
                        cat("linux\n")
                        system("./MarOpt_v243_Linux64 -s")
                    }
                }
                
                JoinParallelResults()
                
                # write results to pu dbf
                ImportOutputsCsvToShpDbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),
                                         sMarxanDir,"PU_ID")

# initialise output files for "CalibrationSensitivity" app
# this generates output files with default parameter values for the app to load at runtime
safe_log <- function(rValue)
{
  if (rValue > 0)
  {
    return(log(rValue))
  } else {
    return(-20)
  }
}
sMarxanDir <<- paste0(sWorkDir,"zzz/p/files")
rRampBLMmin <- 0
rRampBLMmax <- 10000000000000
rRampSPFmin <- 0.0001
rRampSPFmax <- 10000000000000
rtargetmin <- 0
rtargetmax <- 1
rcostmin <- 0.0001
rcostmax <- 10000000000000
#swhichparam <- "BLM"
#swhichparam <- "SPF"
#swhichparam <- "Targ"
swhichparam <- "Cost"
ruserblm <- 0
ruserspf <- 10
rusertarg <- 0.1
rusercost <- 0.0001

                # set min, max, interval for value ramping
                if (swhichparam == "BLM")
                {
                    rMinimum <- safe_log(rRampBLMmin)
                    rMaximum <- safe_log(rRampBLMmax)
                    rInterval <- (rMaximum - rMinimum) / 9        
                    rValue <- rRampBLMmin
                }
                if (swhichparam == "SPF")
                {
                    rMinimum <- safe_log(rRampSPFmin)
                    rMaximum <- safe_log(rRampSPFmax)
                    rInterval <- (rMaximum - rMinimum) / 9
                    rValue <- rRampSPFmin
                }
                if (swhichparam == "Targ")
                {
                    rMinimum <- rtargetmin
                    rMaximum <- rtargetmax
                    rInterval <- (rMaximum - rMinimum) / 9
                    rValue <- rtargetmin
                }
                if (swhichparam == "Cost")
                {
                    rMinimum <- safe_log(rcostmin)
                    rMaximum <- safe_log(rcostmax)
                    rInterval <- (rMaximum - rMinimum) / 9
                    rValue <- rcostmin
                }

                # create the ramped value file
                write(paste0('i,',swhichparam),file=paste0(sMarxanDir,"/",swhichparam,".csv"))
                write(paste0(1,",",rValue),file=paste0(sMarxanDir,"/",swhichparam,".csv"),append=TRUE)       
                for (i in 2:10)
                {
                  if (swhichparam == "Targ")
                  {
                      rValue <- rMinimum+((i-1)*rInterval)       # linear ramping for Target
                  } else {
                      rValue <- exp(rMinimum+((i-1)*rInterval))  # exponential ramping for BLM, SPF and Cost
                  }
                  write(paste0(i,",",rValue),file=paste0(sMarxanDir,"/",swhichparam,".csv"),append=TRUE)
                }
        
                # initialise a value summary file
                if (swhichparam == "BLM")
                {
                    write("i,BLM,cost,boundary length",file=paste0(sMarxanDir,"/output/output_BLMsummary.csv"))
                }
                if (swhichparam == "SPF")
                {
                    write("i,SPF,cost,shortfall",file=paste0(sMarxanDir,"/output/output_SPFsummary.csv"))
                }
                if (swhichparam == "Targ")
                {
                    write('i,Targ,cost',file=paste0(sMarxanDir,"/output/output_Targsummary.csv"))
                }
                if (swhichparam == "Cost")
                {
                    write('i,Cost,shortfall,cost',file=paste0(sMarxanDir,"/output/output_Costsummary.csv"))
                }
                
                # load the ramped value file
                VALUEcsv <- read.csv(paste0(sMarxanDir,"/",swhichparam,".csv"))
                
                foreach(i=1:10) %dopar%   
                #i <- 1
                {
                    # read input.dat and edit parameters
                    inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                    iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
                    iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
                    iBLMparam <- which(regexpr("BLM",inputdat)==1)
                    iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                    iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                    iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
                    iPUNAMEparam <- which(regexpr("PUNAME",inputdat)==1)
                    # read spec.dat
                    specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
                    targetfilter <- read.csv(paste(sMarxanDir,"/input/conservation_features.dat",sep=""),stringsAsFactors=FALSE)
                    if (swhichparam == "BLM")
                    {
                        inputdat[iBLMparam] <- paste0("BLM ",VALUEcsv[i,2])                      
                        specdat$spf <- ruserspf
                        specdat$prop <- rusertarg
                    }
                    if (swhichparam == "SPF")
                    {
                        inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
                        specdat$spf <- VALUEcsv[i,2]
                        specdat$prop <- rusertarg
                    }
                    if (swhichparam == "Targ")
                    {
                        inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
                        specdat$spf <- ruserspf
                        specdat$prop <- VALUEcsv[i,2]
                    }
                    if (swhichparam == "Cost")
                    {
                        inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
                        specdat$spf <- ruserspf
                        specdat$prop <- rusertarg
                    }
                    specdat$spf <- specdat$spf * targetfilter$spf
                    specdat$prop <- specdat$prop * targetfilter$spf
                    # save spec.dat
                    write.csv(specdat,paste0(sMarxanDir,"/input/spec",swhichparam,i,".dat"),quote=FALSE,row.names=FALSE)
                    # read pu.dat
                    pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"))
                    if (swhichparam == "Cost")
                    {
                        rCostWeighting <- VALUEcsv[i,2]
                    } else {
                        rCostWeighting <- rusercost
                    }
                    # compute weighted cost
                    costweightings <- read.csv(paste0(sMarxanDir,"/input/costweight.dat"))
                    load(file=paste0(sMarxanDir,"/input/pu_cost_norm.Rdata"))
                    for (j in 1:dim(pudat)[1])
                    {
                        pudat$cost[j] <- sum(unlist(pu_cost_norm[j,2:17]) * costweightings$Weighting) * rCostWeighting
                    }
                    rm(pu_cost_norm)
                    # save pu.dat
                    write.csv(pudat,paste0(sMarxanDir,"/input/pu",swhichparam,i,".dat"),quote=FALSE,row.names=FALSE)                    
                    # edit parameters and save input.dat
                    inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
                    inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
                    inputdat[iSPECNAMEparam] <- paste0("SPECNAME spec",swhichparam,i,".dat")
                    inputdat[iPUNAMEparam] <- paste0("PUNAME pu",swhichparam,i,".dat")
                    inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",swhichparam,i)
                    inputdat[iNUMREPSparam] <- "NUMREPS 10"
                    writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input",swhichparam,i,".dat"))
                }
                
                foreach(i=1:10) %dopar%   
                {
                    # run Marxan
                    setwd(paste0(sMarxanDir,"/core",i))
                    if (.Platform$pkgType == "mac.binary")
                    {
                        cat("mac\n")
                        system(paste0("./MarOpt_v243_Mac64 -s input",swhichparam,i,".dat"))
                    } else {
                        cat("linux\n")
                        system(paste0("./MarOpt_v243_Linux64 -s input",swhichparam,i,".dat"))
                    }
                
                    # read the Marxan summary file
                    sumfile <- read.csv(paste0(sMarxanDir,"/output/output",swhichparam,i,"_sum.csv"))
  
                    # write to the value summary file
                    if (swhichparam == "BLM")
                    {
                        # write the cost and boundary length to the value summary file
                        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,5]),sep=","),
                              file=paste0(sMarxanDir,"/output/output_BLMsummary",i,".csv"))
                    }
                    if (swhichparam == "SPF")
                    {
                        # write the cost and target shortfall to the value summary file
                        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,12]),sep=","),
                              file=paste0(sMarxanDir,"/output/output_SPFsummary",i,".csv"))
                    }
                    if (swhichparam == "Targ")
                    {
                        # write the cost and target to the value summary file
                        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),sep=","),
                              file=paste0(sMarxanDir,"/output/output_Targsummary",i,".csv"))
                    }
                    if (swhichparam == "Cost")
                    {
                        # write the cost and target shortfall to the value summary file
                        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,12]),sep=","),
                              file=paste0(sMarxanDir,"/output/output_Costsummary",i,".csv"))
                    }
                }                
                
                # compose summary table
                for (i in 1:10)
                {
                    if (swhichparam == "BLM")
                    {
                        write(readLines(con=paste0(sMarxanDir,"/output/output_BLMsummary",i,".csv")),
                              file=paste0(sMarxanDir,"/output/output_BLMsummary.csv"),append=TRUE)
                    }
                    if (swhichparam == "SPF")
                    {
                        write(readLines(con=paste0(sMarxanDir,"/output/output_SPFsummary",i,".csv")),
                              file=paste0(sMarxanDir,"/output/output_SPFsummary.csv"),append=TRUE)
                    }
                    if (swhichparam == "Targ")
                    {
                        write(readLines(con=paste0(sMarxanDir,"/output/output_Targsummary",i,".csv")),
                              file=paste0(sMarxanDir,"/output/output_Targsummary.csv"),append=TRUE)
                    }
                    if (swhichparam == "Cost")
                    {
                        write(readLines(con=paste0(sMarxanDir,"/output/output_Costsummary",i,".csv")),
                              file=paste0(sMarxanDir,"/output/output_Costsummary.csv"),append=TRUE)
                    }
                }
                


