# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: dataset construction, MarProb
#          generate starting condition for ParameterTesting app
# Data: 
#       "/Users/matt/Documents/zzz/GISdata"
#       
############################################
# initialise output for ParamaterTesting app
# create and populate "core" directories for the app
# already done but need to do it for 20 cores before deploy to marxan.net
# compute all the starting output files for the app
require(shiny)
require(sp)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
require(vegan)
require(labdsv)
require(xtable)
library(foreach)
library(doMC)

iNumberOfCores <- 10
iRepsPerTest <- 10

registerDoMC(iNumberOfCores)  # the number of CPU cores

safe_log <- function(rValue)
{
  if (rValue > 0)
  {
    return(log(rValue))
  } else {
    return(-20)
  }
}

# we have target locked in spec.dat file so we're not testing target

sMarxanBaseDir <<- "/Users/matt/Documents/zzz/ParameterTesting_rev3/p/files"
rRampBLMmin <- 0
rRampBLMmax <- 10000000000000
rRampSPFmin <- 0.0001
rRampSPFmax <- 10000000000000
rRampPWmin <- 0
rRampPWmax <- 10000000000000
rcostmin <- 0.0001
rcostmax <- 10000000000000
ruserblm <- 0
ruserspf <- 1
ruserpw <- 0
rusercost <- 1

InputDats <- c(1,2,3,4)
iinputdat <- InputDats[1]

RegionDirs <- c("all_regions","region1","region2","region3","region4",
                "region5","region6","region7","region8")

for (iregion in 1:9)
{
  sRegionDir <- RegionDirs[iregion]
  sMarxanDir <- paste0(sMarxanBaseDir,"/",sRegionDir)
  
  theparams <- c("BLM","SPF","PW","Cost")
  for (iparam in 1:4)
  {
    #iparam <- 1
    swhichparam <- theparams[iparam]
    
    # set min, max, interval for value ramping
    if (swhichparam == "BLM")
    {
      rMinimum <- safe_log(rRampBLMmin)
      rMaximum <- safe_log(rRampBLMmax)
      rInterval <- (rMaximum - rMinimum) / (iNumberOfCores - 1)
      rValue <- rRampBLMmin
    }
    if (swhichparam == "SPF")
    {
      rMinimum <- safe_log(rRampSPFmin)
      rMaximum <- safe_log(rRampSPFmax)
      rInterval <- (rMaximum - rMinimum) / (iNumberOfCores - 1)
      rValue <- rRampSPFmin
    }
    if (swhichparam == "PW")
    {
      rMinimum <- safe_log(rRampPWmin)
      rMaximum <- safe_log(rRampPWmax)
      rInterval <- (rMaximum - rMinimum) / (iNumberOfCores - 1)
      rValue <- rRampPWmin
    }
    if (swhichparam == "Cost")
    {
      rMinimum <- safe_log(rcostmin)
      rMaximum <- safe_log(rcostmax)
      rInterval <- (rMaximum - rMinimum) / (iNumberOfCores - 1)
      rValue <- rcostmin
    }
    
    # create the ramped value file
    write(paste0('i,',swhichparam),file=paste0(sMarxanDir,"/",swhichparam,".csv"))
    write(paste0(1,",",rValue),file=paste0(sMarxanDir,"/",swhichparam,".csv"),append=TRUE)       
    for (i in 2:iNumberOfCores)
    {
      rValue <- exp(rMinimum+((i-1)*rInterval))  # exponential ramping for BLM, SPF, PW and Cost
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
    if (swhichparam == "PW")
    {
      write("i,PW,cost,Probability1D",file=paste0(sMarxanDir,"/output/output_PWsummary.csv"))
    }
    if (swhichparam == "Cost")
    {
      write('i,Cost,shortfall,cost',file=paste0(sMarxanDir,"/output/output_Costsummary.csv"))
    }
    
    # load the ramped value file
    VALUEcsv <- read.csv(paste0(sMarxanDir,"/",swhichparam,".csv"))
    
foreach(i=1:iNumberOfCores) %dopar%   
{
  #i <- 1
  # read input.dat and edit parameters
  inputdat <- readLines(paste0(sMarxanDir,"/input",iinputdat,".dat"))
  iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
  iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
  iBLMparam <- which(regexpr("BLM",inputdat)==1)
  iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
  iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
  iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
  iPUNAMEparam <- which(regexpr("PUNAME",inputdat)==1)
  iPWparam <- which(regexpr("PROBABILITYWEIGHTING",inputdat)==1)
  # read spec.dat
  specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
  targetfilter <- read.csv(paste(sMarxanDir,"/input/conservation_features.csv",sep=""),stringsAsFactors=FALSE)
  if (swhichparam == "BLM")
  {
    inputdat[iBLMparam] <- paste0("BLM ",VALUEcsv[i,2])                      
    specdat$spf <- ruserspf
    inputdat[iPWparam] <- paste0("PROBABILITYWEIGHTING ",ruserpw)
  }
  if (swhichparam == "SPF")
  {
    inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
    specdat$spf <- VALUEcsv[i,2]
    inputdat[iPWparam] <- paste0("PROBABILITYWEIGHTING ",ruserpw)
  }
  if (swhichparam == "PW")
  {
    inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
    specdat$spf <- ruserspf
    inputdat[iPWparam] <- paste0("PROBABILITYWEIGHTING ",VALUEcsv[i,2])
  }
  if (swhichparam == "Cost")
  {
    inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
    specdat$spf <- ruserspf
    inputdat[iPWparam] <- paste0("PROBABILITYWEIGHTING ",ruserpw)
  }
  specdat$spf <- specdat$spf * targetfilter$spf
  # save spec.dat
  write.csv(specdat,paste0(sMarxanDir,"/input/spec",swhichparam,i,".dat"),quote=FALSE,row.names=FALSE)
  # read pu.dat
  pudat <- read.csv(paste0(sMarxanDir,"/input/pu",iinputdat,".dat"))
  if (swhichparam == "Cost")
  {
    rCostWeighting <- VALUEcsv[i,2]
  } else {
    rCostWeighting <- rusercost
  }
  # compute weighted cost
  pudat$cost <- pudat$cost * rCostWeighting
  # save pu.dat
  write.csv(pudat,paste0(sMarxanDir,"/input/pu",swhichparam,i,".dat"),quote=FALSE,row.names=FALSE)                    
  # edit parameters and save input.dat
  inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
  inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
  inputdat[iSPECNAMEparam] <- paste0("SPECNAME spec",swhichparam,i,".dat")
  inputdat[iPUNAMEparam] <- paste0("PUNAME pu",swhichparam,i,".dat")
  inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",swhichparam,i)
  inputdat[iNUMREPSparam] <- paste0("NUMREPS ",iRepsPerTest)
  writeLines(inputdat,paste0(sMarxanBaseDir,"/core",i,"/input",swhichparam,i,".dat"))
}
foreach(i=1:iNumberOfCores) %dopar%   
{
  # run Marxan
  setwd(paste0(sMarxanBaseDir,"/core",i))
  if ((.Platform$pkgType == "mac.binary") || (.Platform$pkgType == "mac.binary.mavericks"))
  {
    cat("mac\n")
    system(paste0("./MarOpt_v243_Mac64 -s input",swhichparam,i,".dat"))
  } else {
    cat("linux\n")
    system(paste0("./MarOpt_v243_Linux64 -s input",swhichparam,i,".dat"))
  }
}
foreach(i=1:iNumberOfCores) %dopar%   
{
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
    write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,13]),sep=","),
          file=paste0(sMarxanDir,"/output/output_SPFsummary",i,".csv"))
  }
  if (swhichparam == "PW")
  {
    # write the cost and probability shortfall to the value summary file
    write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,12]),sep=","),
          file=paste0(sMarxanDir,"/output/output_PWsummary",i,".csv"))
  }
  if (swhichparam == "Cost")
  {
    # write the cost and target shortfall to the value summary file
    write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,12]),sep=","),
          file=paste0(sMarxanDir,"/output/output_Costsummary",i,".csv"))
  }
}                

# compose summary table
for (i in 1:iNumberOfCores)
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
  if (swhichparam == "PW")
  {
    write(readLines(con=paste0(sMarxanDir,"/output/output_PWsummary",i,".csv")),
          file=paste0(sMarxanDir,"/output/output_PWsummary.csv"),append=TRUE)
  }
  if (swhichparam == "Cost")
  {
    write(readLines(con=paste0(sMarxanDir,"/output/output_Costsummary",i,".csv")),
          file=paste0(sMarxanDir,"/output/output_Costsummary.csv"),append=TRUE)
  }
}
}
}
