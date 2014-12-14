library(foreach)
library(doMC)

registerDoMC(10)  # the number of CPU cores 
# Initialise output files for the integrated parameter calibration and sensitivity analysis app
sMarxanDir <- "/Users/matthewwatts/Documents/zzz/zzz"

setwd(sMarxanDir)
rRampSPFmin <- 0.0001
rRampSPFmax <- 1e+13
rRampBLMmin <- 0
rRampBLMmax <- 1e+13
rtargetmin <- 0
rtargetmax <- 1
rcostmin <- 0.0001
rcostmax <- 1e+13
ruserspf <- 0.0001
ruserblm <- 0
rusertarg <- 0
rusercost <- 0.0001
#swhichparam <- "BLM"
#swhichparam <- "SPF"
#swhichparam <- "Targ"
swhichparam <- "Cost"
safe_log <- function(rValue)
{
  if (rValue > 0)
  {
    return(log(rValue))
  } else {
    return(-20)
  }
}

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
                #i <- 2
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
                    #targetfilter <- read.csv(paste(sMarxanDir,"/input/conservation_features_scenario4.csv",sep=""),stringsAsFactors=FALSE)
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
                    specdat$spf <- specdat$spf #* targetfilter$target
                    specdat$prop <- specdat$prop #* targetfilter$target
                    # save spec.dat
                    write.csv(specdat,paste0(sMarxanDir,"/input/spec",swhichparam,i,".dat"),quote=FALSE,row.names=FALSE)
                    # read pu.dat
                    pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"))
                    #if (swhichparam == "Cost")
                    #{
                    #    rCostWeighting <- VALUEcsv[i,2]
                    #} else {
                    #    rCostWeighting <- rusercost
                    #}
                    # compute weighted cost
                    #costweightings <- read.csv(paste0(sMarxanDir,"/input/cost_weightings.csv"))
                    #load(file=paste0(sMarxanDir,"/input/pu_cost_norm.Rdata"))
                    for (j in 1:dim(pudat)[1])
                    {
                        #pudat$cost[j] <- sum(unlist(pucosts[j,2:22]) * costweightings$Weighting) * rCostWeighting
                        pudat$cost[j] <- pudat$cost[j] * rCostWeighting
                    }
                    #pudat$cost <- sum(unlist(pucosts[,2:22]) * costweightings$Weighting) * rCostWeighting
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
                

##############

        CamdenSoundMP <- c(7,8)
        CamdenSoundSZ <- c(1,2,3,4,5,6,9)
        TerrestrialReserves <- c(10001,10002,10003,10004,10005,10006)

iPUID <- 1

iIsCSMP <- length(which(CamdenSoundMP==iPUID))
iIsCSSZ <- length(which(CamdenSoundSZ==iPUID))
iIsTR <- length(which(TerrestrialReserves==iPUID))

##############

ramp1 <- colorRampPalette(c("green","yellow"))(6)
ramp2 <- colorRampPalette(c("yellow","blue"))(6)

aramp <- c(ramp1[1:5],ramp2)
aramp <- c(colorRampPalette(c("green","yellow"))(6)[1:5],colorRampPalette(c("yellow","blue"))(6))

blueramp <- colorRampPalette(c("green","yellow","blue"))(11)

##############
# generate the park name report



pu_parkname_table <- read.dbf("/Users/matthewwatts/Documents/zzz/_20140529_gda94AustAlbers/_20140529_terres_gda94AustAlbers_attrib.dbf")
colnames(pu_parkname_table)
pu_parkname_table <- sqldf("SELECT PU_ID, Park_Name from pu_parkname_table")
colnames(pu_parkname_table)
dim(pu_parkname_table)[1]

parkname_vector <- rep(0,each=dim(pu_parkname_table)[1])
length(parkname_vector)

for (i in 1:dim(pu_parkname_table)[1])
{
  if (pu_parkname_table$Park_Name[i] == "CSMP")
  {
    parkname_vector[i] <- 1
  }
  if (pu_parkname_table$Park_Name[i] == "HFMP")
  {
    parkname_vector[i] <- 2
  }
  if (pu_parkname_table$Park_Name[i] == "NKMP")
  {
    parkname_vector[i] <- 3
  }
}

parkname_vector[1]
parkname_vector[100]
parkname_vector[8000]

save(parkname_vector,file="/Users/matthewwatts/Documents/zzz/parkname_vector.Rdata")

# create empty vector for sp results to live in
iSpecCount <- 111
sMarxanDir <- "/Users/matthewwatts/Documents/zzz/RunMarxan_rev8_scenario4/p/files"

s_feature_table <- paste0(sMarxanDir,"/input/conservation_features_scenario4a.csv")
sMarxanSolution_table <- paste0(sMarxanDir,"/output/output_r00038.csv")
sMatrix_table <- paste0(sMarxanDir,"/input/puvsp.dat")

feature_table <- read.csv(s_feature_table,stringsAsFactors=FALSE)
marxan_soln_table <- read.csv(sMarxanSolution_table,stringsAsFactors=FALSE)
marxan_matrix <- read.csv(sMatrix_table,stringsAsFactors=FALSE)

# create blank matrix to store our results
spvalues <- matrix(0,nrow=nrow(feature_table),ncol=6)
dim(spvalues)

for (i in 1:nrow(marxan_matrix))
{
  ispindex <- which(feature_table$spid == marxan_matrix$species[i])
  ipuindex <- which(marxan_soln_table$PUID == marxan_matrix$pu[i])
  
  ramount <- marxan_matrix$amount[i]
  
  if (parkname_vector[ipuindex] == 1)
  {
    # CSMP
    spvalues[ispindex,1] <- spvalues[ispindex,1] + ramount
    
    if (marxan_soln_table$SOLUTION[ipuindex]  == 1)
    {
      spvalues[ispindex,2] <- spvalues[ispindex,2] + ramount
    }
  }
  if (parkname_vector[ipuindex] == 2)
  {
    # HFMP
    spvalues[ispindex,3] <- spvalues[ispindex,3] + ramount
    
    if (marxan_soln_table$SOLUTION[ipuindex]  == 1)
    {
      spvalues[ispindex,4] <- spvalues[ispindex,4] + ramount
    }
  }
  if (parkname_vector[ipuindex] == 3)
  {
    # NKMP
    spvalues[ispindex,5] <- spvalues[ispindex,5] + ramount
    
    if (marxan_soln_table$SOLUTION[ipuindex]  == 1)
    {
      spvalues[ispindex,6] <- spvalues[ispindex,6] + ramount
    }
  }
}
colnames(spvalues) <- c("CSMP Amount","CSMP Selected","HFMP Amount","HFMP Selected","NKMP Amount","NKMP Selected")

#feature_table$spid
#feature_table$spname

outputtable <- cbind(feature_table,spvalues)
costoutput <- sqldf("SELECT * from outputtable WHERE spf = 0")
thetable <- sqldf("SELECT spid,spname,CSMP_Amount,CSMP_Selected,HFMP_Amount,HFMP_Selected,NKMP_Amount,NKMP_Selected from costoutput")
speciesoutput <- sqldf("SELECT * from outputtable WHERE spf = 1")
speciesoutput <- sqldf("SELECT spid,spname,CSMP_Amount,CSMP_Selected,HFMP_Amount,HFMP_Selected,NKMP_Amount,NKMP_Selected from speciesoutput")

for (i in 1:dim(thetable)[1])
{
  if (thetable[i,3] > 0)
  {
    thetable[i,4] <- thetable[i,4] / thetable[i,3]
  }
  if (thetable[i,5] > 0)
  {
    thetable[i,6] <- thetable[i,6] / thetable[i,5]
  }
  if (thetable[i,7] > 0)
  {
    thetable[i,8] <- thetable[i,8] / thetable[i,7]
  }
}

# parse the Marxan matrix file line by line
# lookup feature id
# lookup pu id
# add amount to relevant vector

dim(marxan_matrix)
colnames(marxan_matrix)
head(marxan_matrix)
head(marxan_soln_table)
tail(marxan_soln_table)

head(feature_table)
dim(feature_table)
colnames(feature_table)
feature_table$spf
#write.csv(costweight,paste0(sMarxanDir,"/input/costweight.dat"),quote=FALSE,row.names=FALSE)

# load a Marxan solution to report against



infile <- file(sMatrix_table,"r")

repeat{
sLine <- readLines(con=infile,n=1)
  #if(condition){
  #  break
  #}
cat(paste0(sLine,"\n"))
}

close(infile)

