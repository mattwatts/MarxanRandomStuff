# Author: Matt Watts, m.watts@uq.edu.au
# Date: October 2013
# Prepare Marxan input files for Nimrod cloud bursting runs.
# This file, Nimrod.R, contains the R function definitions.

PrepareSpecDatNimrod <- function(sInputDir,sOutputDir,iPropVars,iSpfVars)
# Prepare a number of spec.dat files with different 'prop' and 'spf' values.
# Create a log file listing parameters used for each scenario generated.
# Warning: existing spec files and log file in the output directory will be deleted.
{
  # switch off scientific notation for output files
  options(scipen=500)

  specdat <- read.csv(paste(sInputDir,"/spec.dat",sep=""))
  
  iPropColumn <- which(colnames(specdat)=="prop")
  iSpfColumn <- which(colnames(specdat)=="spf")
  
  # these are the parameter variations we will use for prop and spf
  props <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)  
  spfs <- c(0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000)
  
  # initialise the output file counter
  iSpecDat <- 0
  
  # initialise the log file
  write('index,prop,spf',file=paste(sOutputDir,"/log_spec.csv",sep=""))
  
  for (i in 1:iPropVars)
  {
    specdat[,iPropColumn] <- props[i]
    
    for (j in 1:iSpfVars)
    {
      # increment the output file counter
      iSpecDat <- iSpecDat + 1
      
      specdat[,iSpfColumn] <- spfs[j]
      
      # write the spec dat file
      write.csv(specdat,
                paste(sOutputDir,"/spec",iSpecDat,".dat",sep=""),
                quote=FALSE,row.names=FALSE)
      
      # append to the log file
      write(paste(iSpecDat,props[i],spfs[j],sep=","),
            file=paste(sOutputDir,"/log_spec.csv",sep=""),
            append=TRUE)
    }
  }
}

PreparePuDatNimrod <- function(sInputDir,sOutputDir,iCostVars)
# Prepare a number of pu.dat files with different 'cost' values.
# Create a log file listing parameters used for each scenario generated.
# Warning: existing pu files and log file in the output directory will be deleted.
{
  # switch off scientific notation for output files
  options(scipen=500)
  
  pudat <- read.csv(paste(sInputDir,"/pu.dat",sep=""))
  
  iCostColumn <- which(colnames(pudat)=="cost")
  
  # these are the parameter weighting variations we will use for cost
  weightings <- c(0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000)
  
  # initialise the log file
  write('index,costweighting',file=paste(sOutputDir,"/log_pu.csv",sep=""))
  
  for (i in 1:iCostVars)
  {
    # reload file to apply the next weighting to the original cost values
    if (i > 1)
    {
      pudat <- read.csv(paste(sInputDir,"/pu.dat",sep=""))
    }
    
    # apply the cost weighting
    pudat[,iCostColumn] <- pudat[,iCostColumn] * weightings[i]
    
    # write the pu dat file
    write.csv(pudat,
              paste(sOutputDir,"/pu",i,".dat",sep=""),
              quote=FALSE,row.names=FALSE)
    
    # append to the log file
    write(paste(i,weightings[i],sep=","),
          file=paste(sOutputDir,"/log_pu.csv",sep=""),
          append=TRUE)
  }
}

PrepareInputDatNimrod <- function(sInputDir,sOutputDir,iBlmVars,iPuVars,iPropVars,iSpfVars)
# Prepare a number of input.dat files with different 'BLM' values
# and referring to a number of pu.dat and spec.dat files.
# Create a log file listing parameters used for each scenario generated.
# Warning: existing input dat files and log file in the output directory will be deleted.
{
  # these are the parameter weighting variations we will use for cost and logging the other parameters
  BLM <- c(0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000)
  weightings <- c(0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000)
  props <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)  
  spfs <- c(0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000)
  
  # initialise the log file
  write('index,BLM,BLMindex,puindex,propindex,spfindex,specindex,costweighting,prop,spf',file=paste(sOutputDir,"/log_inputdat.csv",sep=""))
  
  # load the original input.dat file
  inputdat <- readLines(paste(sInputDir,"/input.dat",sep=""))
  # find the parameters we are modifying in the input parameter file
  iBLMparam <- which(regexpr("BLM",inputdat)==1)
  iPUNAMEparam <- which(regexpr("PUNAME",inputdat)==1)
  iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
  
  # initialise the output file counter
  iInputDat <- 0

  for (i in 1:iBlmVars)
  {
    for (j in 1:iPuVars)
    {
      iSpecDat <- 0
      
      for (k in 1:iPropVars)
      {
        for (l in 1:iSpfVars)
        {
          # increment the output file counter
          iInputDat <- iInputDat + 1
          
          iSpecDat <- iSpecDat + 1

          # update the parameters
          inputdat[iBLMparam] <- paste("BLM",BLM[i],sep=" ")
          inputdat[iPUNAMEparam] <- paste("PUNAME pu",j,".dat",sep="")
          inputdat[iSPECNAMEparam] <- paste("SPECNAME spec",iSpecDat,".dat",sep="")
        
          # write the parameters
          writeLines(inputdat,paste(sOutputDir,"/input",iInputDat,".dat",sep=""))
        
          # append to the log file
          write(paste(iInputDat,BLM[i],i,j,k,l,iSpecDat,weightings[j],props[k],spfs[k],sep=","),
                file=paste(sOutputDir,"/log_inputdat.csv",sep=""),
                append=TRUE)
        }
      }
    }
  }
}

PrepareCompressFilesNimrod <- function(sInputDir,sOutputDir,iBlmVars,iPuVars,iPropVars,iSpfVars)
# Using a set of marxan input files, create a set of compressed files ready for Nimrod.
# Create a tar.gz file that is common for each scenario, i.e. bound.dat, puvsp.dat
# Create a tar file for each scenario that includes only the files necessary for that scenario.
# i.e. input1.dat, pu1.dat, spec1.dat
# Only include the files necessary to run each scenario to avoid duplication of files
# and to avoid wasted storage and network transmissions.
# Create a Nimrod parameter file ready to execute on a Nimrod server.
{
  # create the tarball with appropriate input files for every scenario
  # puvspr.dat, bound.dat
  setwd(sInputDir)
  system("tar -cf scenario_all.tar puvsp.dat bound.dat")
  system("gzip scenario_all.tar")
  system(paste("mv scenario_all.tar.gz ",sOutputDir,sep=""))
  
  setwd(sOutputDir)
  # initialise the log file
  write('index,inputdat,pudat,specdat',
        file=paste(sOutputDir,"/log_compress.csv",sep=""))

  # initialise the output file counter
  iInputDat <- 0
  
  for (i in 1:iBlmVars)
  {
    for (j in 1:iPuVars)
    {
      iSpecDat <- 0
      
      for (k in 1:iPropVars)
      {
        for (l in 1:iSpfVars)
        {
          # increment the output file counter
          iInputDat <- iInputDat + 1
          
          iSpecDat <- iSpecDat + 1
          
          # create the tarball with appropriate input files for this scenario
          # input.dat pu.dat spec.dat
          system(paste("tar -cf scenario",iInputDat,".tar",
                       " input",iInputDat,".dat pu",j,".dat spec",iSpecDat,".dat",sep=""))
          system(paste("gzip scenario",iInputDat,".tar",sep=""))
                    
          # append to the log file
          write(paste(iInputDat,iInputDat,j,iSpecDat,sep=","),
                file=paste(sOutputDir,"/log_compress.csv",sep=""),
                append=TRUE)
        }
      }
    }
  }  
}


