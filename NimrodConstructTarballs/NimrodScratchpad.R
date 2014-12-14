# Author: Matt Watts, m.watts@uq.edu.au
# Date: October 2013
# Prepare Marxan input files for Nimrod cloud bursting runs.
# This file, NimrodScratchpad.R, is the scratchpad file that contains code used to develop R functions.

# prepare a number of spec.dat files with different 'prop' and 'spf' values

specdat <- read.csv("/Users/matthewwatts/Documents/R/MPA_Activity/input/spec.dat")

iPropColumn <- which(colnames(specdat)=="prop")
iSpfColumn <- which(colnames(specdat)=="spf")

props <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

spfs <- c(0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000)

iSpecDat <- 0

for (i in 1:4)
{
  specdat[,iPropColumn] <- props[i]
  
  for (j in 1:4)
  {
    iSpecDat <- iSpecDat + 1
    
    specdat[,iSpfColumn] <- spfs[j]
    
    write.csv(specdat,
              paste("/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod/spec",iSpecDat,".dat",sep=""),
              quote=FALSE,row.names=FALSE)
  }
}

# call the function
PrepareSpecDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                     "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                     2,2)

# prepare a number of pu.dat files with different 'cost' values

pudat <- read.csv("/Users/matthewwatts/Documents/R/MPA_Activity/input/pu.dat")
head(pudat)

iCostColumn <- which(colnames(pudat)=="cost")
iCostColumn

weightings <- c(0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000)
weightings

pudat[,iCostColumn] <- pudat[,iCostColumn] * weightings[1]

# call the function
PreparePuDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                   "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                   2)

# Prepare a number of input.dat files with different 'BLM' values

inputdat <- readLines("/Users/matthewwatts/Documents/R/MPA_Activity/input.dat")
inputdat

inputdat[1]
length(inputdat)

grep

x <- regexpr("BLM",inputdat[9])
regexpr("BLM",inputdat[1])
regexpr("BLM",inputdat[55])

typeof(x)
class(x)

x = regexpr("BLM",inputdat)
y = attr(x,"match.length")
y

PUNAME
SPECNAME



BLMmatch <- regexpr("BLM",inputdat)
BLMlength <- attr(BLMmatch,"match.length")

iBLMparam <- which(BLMmatch==1)
iBLMparam

PUNAMEmatch <- regexpr("PUNAME",inputdat)
PUNAMElength <- attr(PUNAMEmatch,"match.length")

iPUNAMEparam <- which(PUNAMEmatch==1)
iPUNAMEparam

SPECNAMEmatch <- regexpr("SPECNAME",inputdat)
SPECNAMElength <- attr(SPECNAMEmatch,"match.length")

iSPECNAMEparam <- which(SPECNAMEmatch==1)
iSPECNAMEparam

# call the function
PrepareSpecDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                     "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                     2,2)
PreparePuDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                   "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                   2)
PrepareInputDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity",
                      "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                      2,2,2,2)

PrepareSpecDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                     "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                     5,5)
PreparePuDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                   "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                   5)
PrepareInputDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity",
                      "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                      5,5,5,5)

PrepareSpecDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                     "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                     10,10)
PreparePuDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                   "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                   10)
PrepareInputDatNimrod("/Users/matthewwatts/Documents/R/MPA_Activity",
                      "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                      10,10,10,10)
PrepareCompressFilesNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                           "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                           10,10,10,10)
# remove the *.dat and *.gz files
system('for f in /Users/matthewwatts/Documents/R/MPA_Activity/Nimrod/*.dat; do rm "$f"; done')
system('for f in /Users/matthewwatts/Documents/R/MPA_Activity/Nimrod/*.gz; do rm "$f"; done')

# creating a tarball

tar("/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod/all_scenarios.tar.gz",
    c("/Users/matthewwatts/Documents/R/MPA_Activity/input/bound.dat",
      "/Users/matthewwatts/Documents/R/MPA_Activity/input/puvsp.dat"),
    compression="gzip")

untar("/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod/all_scenarios.tar.gz",list=TRUE)
    

untar("/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod/test1",list=TRUE)

system("tar -cf /Users/matthewwatts/Documents/R/MPA_Activity/Nimrod/test3.tar /Users/matthewwatts/Documents/R/MPA_Activity/input/bound.dat /Users/matthewwatts/Documents/R/MPA_Activity/input/puvsp.dat")

untar("/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod/test3.tar",list=TRUE)

setwd("/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod")
system("tar -cf test4.tar input1.dat spec1.dat pu1.dat")
system("gzip test4.tar")
untar("test4.tar",list=TRUE)

# call the function
PrepareCompressFilesNimrod("/Users/matthewwatts/Documents/R/MPA_Activity/input",
                           "/Users/matthewwatts/Documents/R/MPA_Activity/Nimrod",
                           2,2,2,2)
  
setwd("/Users/matthewwatts/Documents/R/MPA_Activity/input")
