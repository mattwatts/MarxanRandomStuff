
library(shiny)
library(PBSmapping)
library(maptools)
library(sp)

library(RgoogleMaps)
library(dismo)
library(rgdal)

sMarxanDir <- "/Users/matthewwatts/Documents/zzz/scenario2/p/files"

pu_outline <- readShapeLines(paste0(sMarxanDir,"/pulayer/puoutline.shp"))
pubbox <- bbox(pu_outline)

dim(puoutline)
puoutline[1,2]

puoutline <<- SpatialLines2PolySet(pu_outline)

dim(puoutline)
head(puoutline)

min(puoutline$X)
max(puoutline$X)
min(puoutline$Y)
max(puoutline$Y)

#e <<- extent(123.43202,129.01997,-16.60744,-13.50782)
e <<- extent(min(puoutline$X),max(puoutline$X),min(puoutline$Y),max(puoutline$Y))
#g2 <- gmap(e, type='satellite', z=11, lonlat=TRUE)
g2 <- gmap(e, type='satellite', z=7, lonlat=TRUE)

plot(g2)
polygon(puoutline)
#addPolys(puoutline)

#plotMap(g2)

plotPolys(puoutline)

# SPF Calibration

# init the SPF Calibration app
sMarxanDir <- "/Users/matthewwatts/Documents/zzz/CalibrationSensitivity_scenario3/p/files"

setwd(sMarxanDir)
rRampSPFmin <- 0.0001
rRampSPFmax <- 1e+13
safe_log <- function(rValue)
{
  if (rValue > 0)
  {
    return(log(rValue))
  } else {
    return(-20)
  }
}

                # initialise the SPF file
                write('i,SPF',file=paste0(sMarxanDir,"/SPF.csv"))
                rMinimum <- safe_log(rRampSPFmin)
                rMaximum <- safe_log(rRampSPFmax)
                rInterval <- (rMaximum - rMinimum) / 9
        
                rValue <- rRampSPFmin
                write(paste0(1,",",rValue),file=paste0(sMarxanDir,"/SPF.csv"),append=TRUE)
                cat("\n")
                cat(rValue)
                cat("\n")        
       
                for (i in 2:10)
                {
                  rValue <- exp(rMinimum+((i-1)*rInterval))
                  write(paste0(i,",",rValue),file=paste0(sMarxanDir,"/SPF.csv"),append=TRUE)
                  cat(rValue)
                  cat("\n")        
                }
        
                # initialise a SPF summary file
                write('i,SPF,cost,shortfall',
                      file=paste0(sMarxanDir,"/output/output_SPFsummary.csv"))
                
                # load the SPF's
                SPFcsv <- read.csv(paste0(sMarxanDir,"/SPF.csv"))
                SPFcsv
                
                for (i in 1:10)       
                {
                  # read input.dat and edit parameters
                  inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                  iBLMparam <- which(regexpr("BLM",inputdat)==1)
                  iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                  iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                  iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
                  inputdat[iBLMparam] <- paste0("BLM ",0)
                  inputdat[iSCENNAMEparam] <- paste0("SCENNAME outputSPF",i)
                  inputdat[iNUMREPSparam] <- paste0("NUMREPS ",10)
                  inputdat[iSPECNAMEparam] <- paste0("SPECNAME specSPF",i,".dat")
                  # save input.dat
                  writeLines(inputdat,paste0(sMarxanDir,"/inputSPF",i,".dat"))

                  # read spec.dat and edit parameters
                  specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
                  specdat$spf <- SPFcsv[i,2]
                  # save spec.dat
                  write.csv(specdat,paste0(sMarxanDir,"/input/specSPF",i,".dat"),quote=FALSE,row.names=FALSE)
                
                  # run Marxan
                  #system(paste0("./MarOpt_v243_Linux64 -s input",i,".dat"))
                  system(paste0("./MarOpt_v243_Mac64 -s inputSPF",i,".dat"))
                
                  # read the boundary length and cost from the summary file
                  sumfile <- read.csv(paste0(sMarxanDir,"/output/outputSPF",i,"_sum.csv"))
  
                  # write to the log file
                  write(paste(i,SPFcsv[i,2],mean(sumfile[,3]),mean(sumfile[,12]),sep=","),
                        file=paste0(sMarxanDir,"/output/output_SPFsummary.csv"),
                        append=TRUE)
                }        

# init the BLM calibration app
sMarxanDir <- "/Users/matthewwatts/Documents/zzz/CalibrationSensitivity_scenario3/p/files"
setwd(sMarxanDir)
rRampBLMmin <- 0
rRampBLMmax <- 1e+13
safe_log <- function(rValue)
{
  if (rValue > 0)
  {
    return(log(rValue))
  } else {
    return(-20)
  }
}
# test BLM ramp between 0 and 1: works ok
# test where min is > 0 and < 1
#rRampBLMmin <- 0.1 # 0.5
#rRampBLMmax <- 1000 # 1.3

                # initialise the BLM file
                write('i,BLM',file=paste0(sMarxanDir,"/BLM.csv"))
                rMinimum <- safe_log(rRampBLMmin)
                rMaximum <- safe_log(rRampBLMmax)
                rInterval <- (rMaximum - rMinimum) / 9
        
                rValue <- rRampBLMmin
                write(paste0(1,",",rValue),file=paste0(sMarxanDir,"/BLM.csv"),append=TRUE)
                cat("\n")
                cat(rValue)
                cat("\n")        
       
                for (i in 2:10)
                {
                  rValue <- exp(rMinimum+((i-1)*rInterval))
                  write(paste0(i,",",rValue),file=paste0(sMarxanDir,"/BLM.csv"),append=TRUE)
                  cat(rValue)
                  cat("\n")        
                }
        
                # initialise a BLM summary file
                write('i,BLM,cost,boundary length',
                      file=paste0(sMarxanDir,"/output/output_BLMsummary.csv"))
                
                # load the BLM's
                BLMcsv <- read.csv(paste0(sMarxanDir,"/BLM.csv"))
                BLMcsv

                for (i in 1:10)       
                {
                  # read input.dat and edit parameters
                  inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                  iBLMparam <- which(regexpr("BLM",inputdat)==1)
                  iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                  iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                  inputdat[iBLMparam] <- paste0("BLM ",BLMcsv[i,2])
                  inputdat[iSCENNAMEparam] <- paste0("SCENNAME outputBLM",i)
                  inputdat[iNUMREPSparam] <- "NUMREPS 10"
                  # save input.dat
                  writeLines(inputdat,paste0(sMarxanDir,"/inputBLM",i,".dat"))
                
                  # run Marxan
                  #system(paste0("./MarOpt_v243_Linux64 -s input",i,".dat"))
                  system(paste0("./MarOpt_v243_Mac64 -s inputBLM",i,".dat"))
                
                  # read the boundary length and cost from the summary file
                  sumfile <- read.csv(paste0(sMarxanDir,"/output/outputBLM",i,"_sum.csv"))
  
                  # write to the log file
                  write(paste(i,BLMcsv[i,2],mean(sumfile[,3]),mean(sumfile[,5]),sep=","),
                        file=paste0(sMarxanDir,"/output/output_BLMsummary.csv"),
                        append=TRUE)
                }                

# init the Target calibration app
sMarxanDir <- "/Users/matthewwatts/Documents/zzz/CalibrationSensitivity_scenario3/p/files"
setwd(sMarxanDir)
getwd()
rtargetmin <- 0
rtargetmax <- 1

                # initialise the target file
                write('i,target',file=paste0(sMarxanDir,"/Targ.csv"))
                rInterval <- (rtargetmax - rtargetmin) / 9
        
                write(paste0(1,",",rtargetmin),file=paste0(sMarxanDir,"/Targ.csv"),append=TRUE)
                cat(paste0("\n",rValue,"\n"))
       
                for (i in 2:10)
                {
                  rValue <- rtargetmin+((i-1)*rInterval)
                  write(paste0(i,",",rValue),file=paste0(sMarxanDir,"/Targ.csv"),append=TRUE)
                  cat(paste0(rValue,"\n"))
                }
        
                # initialise a target summary file
                write('i,Targ,cost',
                      file=paste0(sMarxanDir,"/output/output_Targsummary.csv"))
                
                # load the targets
                targetcsv <- read.csv(paste0(sMarxanDir,"/Targ.csv"))
                targetcsv

                for (i in 1:10)       
                {
                  # read input.dat and edit parameters
                  inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                  iBLMparam <- which(regexpr("BLM",inputdat)==1)
                  iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                  iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                  iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
                  inputdat[iBLMparam] <- "BLM 0"
                  inputdat[iSCENNAMEparam] <- paste0("SCENNAME outputTarg",i)
                  inputdat[iNUMREPSparam] <- "NUMREPS 10"
                  inputdat[iSPECNAMEparam] <- paste0("SPECNAME specTarg",i,".dat")
                  # save input.dat
                  writeLines(inputdat,paste0(sMarxanDir,"/inputTarg",i,".dat"))
                  
                  # read spec.dat and edit parameters
                  specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
                  specdat$prop <- targetcsv[i,2]
                  # save spec.dat
                  write.csv(specdat,paste0(sMarxanDir,"/input/specTarg",i,".dat"),quote=FALSE,row.names=FALSE)
                
                  # run Marxan
                  #system(paste0("./MarOpt_v243_Linux64 -s input",i,".dat"))
                  system(paste0("./MarOpt_v243_Mac64 -s inputTarg",i,".dat"))
                
                  # read the cost from the summary file
                  sumfile <- read.csv(paste0(sMarxanDir,"/output/outputTarg",i,"_sum.csv"))
  
                  # write to the log file
                  write(paste(i,targetcsv[i,2],mean(sumfile[,3]),sep=","),
                        file=paste0(sMarxanDir,"/output/output_Targsummary.csv"),
                        append=TRUE)
                }                




# integrating status
require(sp)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
sMarxanDir <- "/Users/matthewwatts/Documents/zzz/RunMarxan_rev3_scenario2/p/files"
iNUMREPS <- 10

    sPulayer <- paste(sMarxanDir,"/pulayer/pulayer.shp",sep="")
    pulayer <- readShapePoly(sPulayer)
    pulayer <<- SpatialPolygons2PolySet(pulayer)
    pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))

    pustatus <- read.csv(paste0(sMarxanDir,"/input/pu.dat"))
    pustatus <<- unlist(sqldf("SELECT status from pustatus"))
  
      values <- sqldf(paste("SELECT SSOLN2 from pu_table",sep=""))
			blueramp <- colorRampPalette(c("white","blue"))(16)
			colours <- rep(blueramp[1],nrow(values))
			for (j in 1:nrow(values))
			{
			    if (pustatus[j] == 2)
			    {
			        colours[j] <- "#40E0D0" # Turquoise
			    } else {
			        if (pustatus[j] == 3)
			        {
			            colours[j] <- "grey"
			        } else {
			            colours[j] <- blueramp[round(15 / iNUMREPS * values[j,])+1]
			        }
			    }
			}
			plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)

# debugging PrepareDisplay for ILP with status added
sMarxanDir <- "/Users/matthewwatts/Documents/zzz/RunMarxan_rev3_scenario2/p/files"

    # prepare the map: pulayer object
    sPulayer <- paste(sMarxanDir,"/pulayer/pulayer.shp",sep="")
    pulayer <- readShapePoly(sPulayer)
    pulayer <<- SpatialPolygons2PolySet(pulayer)
    pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))
    
    # prepare the planning unit status object
    pustatus <- read.csv(paste0(sMarxanDir,"/input/pu.dat"))
    pustatus <<- unlist(sqldf("SELECT status from pustatus"))

    # prepare the cluster analysis objects and map objects for ILP & LP
    solutions_raw<-read.table(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),header=TRUE, row.name=1, sep=",")
    ilp_soln <- read.table(paste0(sMarxanDir,"/output/output_ilp_puidsel.csv"),header=TRUE, row.name=1, sep=",")
    # prepare summary table data
    ILPpucount <<- nrow(ilp_soln)
    ILPcost <<- read.csv(paste0(sMarxanDir,"/output/output_ilp_objective.csv"))[1,2]
    # join ILP results to the solutions matrix
    ilp_row <- c()
    for (i in 1:ncol(solutions_raw))
    {
      iILPValue <- 0
  
      iPUID <- as.integer(substr(colnames(solutions_raw)[i],2,nchar(colnames(solutions_raw)[i])))
      iWhich <- which(ilp_soln[,1]==iPUID)
      if (length(iWhich > 0))
      { 
        iILPValue <- 1
      } 

      ilp_row <- c(ilp_row,iILPValue)
    }
    # move best solution to start of table
    thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
    thetable <- round(sqldf("SELECT Score, Cost, Planning_Units, Penalty, Shortfall, Missing_Values, MPM from thetable"))
    iBest <- which.min(thetable[,1])
    Best <- solutions_raw[iBest,]
    solutions_raw <- solutions_raw[-iBest,]
    solutions_join <- rbind(ilp_row,Best,solutions_raw)
    # remove the columns that relate to planning units that are locked in and locked out
    # i.e. remove the columns from the solutions matrix where pu status is 2 or 3
    for (i in ncol(solutions_raw):1)
    {
        if (pustatus[i] > 0)
        {
            cat(paste0("i ",i," "))
        }
    }
    for (i in ncol(solutions_raw):1)
    {
        if (pustatus[i] > 0)
        {
            solutions_join <- solutions_join[,-i]
        }
    }
    rownames(solutions_join) <- c("ILP",paste0("S",iBest," (Best)"),row.names(solutions_raw))
    plotlabels <<- c("ILP",paste0("S",iBest," (Best)"),row.names(solutions_raw))
    
    # prepare map fields
    ILPvalues <<- ilp_row+1

    solutions <- unique(solutions_join)
    iUniqueSolutions <- dim(solutions)[1]
    nmdscolours <- rep("black",each = iUniqueSolutions)  
    nmdscolours[1] <- "blue"
    nmdscolours[2] <- "green"
    nmdscolours <<- nmdscolours
    soldist<-vegdist(solutions,distance="bray")
    sol.mds<<-nmds(soldist,2)
    h<<-hclust(soldist, method="complete")

##########

taor <- c()
for (i in 60:1)
{
  taor <- c(taor,i)
}
taor

#######
# parallel loop

sMarxanDir <- "/Users/matthewwatts/Documents/zzz/parallel_experiment"

iNumCores <- 10
library(foreach)
library(doMC)
registerDoMC(iNumCores)  # the number of CPU cores 

dir.create(paste0(sMarxanDir,"/output"))

foreach(i=1:iNumCores) %dopar% 
{
    # copy files to execution directory
    dir.create(paste0(sMarxanDir,"/core",i))
    file.copy(paste0(sMarxanDir,"/input.dat"),paste0(sMarxanDir,"/core",i,"/input.dat"))
    file.copy(paste0(sMarxanDir,"/MarOpt_v243_Mac64"),paste0(sMarxanDir,"/core",i,"/MarOpt_v243_Mac64"))
    
    # set parameters
    inputdat <- readLines(paste0(sMarxanDir,"/core",i,"/input.dat"))
    iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
    iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
    iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
    inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
    inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
    inputdat[iSCENNAMEparam] <- paste0("SCENNAME core",i,"output")
    writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input.dat"))
    
    # run Marxan
    setwd(paste0(sMarxanDir,"/core",i))
    system(paste0("./MarOpt_v243_Mac64 -s input.dat"))
}

# join files for parallel run
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

sMarxanDir <- "/Users/matthewwatts/Documents/zzz/RunMarxan_rev4_scenario3/p/files"

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
iBest

# rename _mv files and _r solution files
# output1[1..10], output2[1..10], ... , output10[1..10] -> output[1..100]
# output1_mv00001.csv
# output1_r00001.csv
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

# copy files to create output_mvbest.csv and output_best.csv
file.copy(paste0(sMarxanDir,"/output/output_mv",PadInt(iBest),".csv"),
          paste0(sMarxanDir,"/output/output_mvbest.csv"),
          overwrite=TRUE)
file.copy(paste0(sMarxanDir,"/output/output_r",PadInt(iBest),".csv"),
          paste0(sMarxanDir,"/output/output_best.csv"),
          overwrite=TRUE)

# join ssoln files
# output1_ssoln.csv
# output_ssoln.csv
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

# join cluster files
solnmtx <- read.csv(paste0(sMarxanDir,"/output/output",1,"_solutionsmatrix.csv"))
for (i in 2:10)
{
    solnmtx_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_solutionsmatrix.csv"))
    solnmtx <- rbind(solnmtx,solnmtx_)
}
solnmtx[,1] <- as.character(solnmtx[,1])
for (i in 1:100)
{
    solnmtx[i,1] <- paste0("S",i)
}
write.csv(solnmtx,
              paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),
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

length(iBest)

solnmtx[,1]
warnings()
head(solnmtx)

ncol(ssolntable)
head(ssolntable)
head(pui)
file.rename(from,to)
rownames(solnmtx)
colnames(solnmtx)
min(sumtable[,2]) @ lowest score
sumtable[,1]
sumtable[1,1]

round(runif(10)*100000)


sumtable[2,1]

library(sqldf)

# optimise app startup with .Rdata objects
sMarxanDir <- "/Users/matthewwatts/Documents/zzz/zzz/p/files"

# prepare the map: pulayer object
sPulayer <<- paste(sMarxanDir,"/pulayer/pulayer.shp",sep="")
pulayer <- readShapePoly(sPulayer)
pulayer_ <<- SpatialPolygons2PolySet(pulayer)
# prepare the map: puoutline object
puoutline <- readShapeLines(paste0(sMarxanDir,"/pulayer/puoutline.shp"))
puoutline <<- SpatialLines2PolySet(puoutline)
# prepare the planning unit status object
pustatus <- read.csv(paste0(sMarxanDir,"/input/pu.dat"))
pustatus_ <<- unlist(sqldf("SELECT status from pustatus"))
# save the R binary object
save(pulayer_,puoutline,pustatus_,file=paste0(sMarxanDir,"/pulayer/pulayer.Rdata"))
save(pulayer_,pustatus_,file=paste0(sMarxanDir,"/pulayer/pulayer.Rdata"))


plotPolys(pulayer_,col="blue",border=NA)
addLines(puoutline,col="black")
# Marxan_net_apps/RunMarxan_Tas_rev2
sMarxanDir <- "/Users/matthewwatts/Documents/Marxan_net_apps/RunMarxan_Tas_rev2/p/files"

# prepare the map: pulayer object
sPulayer <<- paste(sMarxanDir,"/pulayer/pulayer.shp",sep="")
pulayer <- readShapePoly(sPulayer)
pulayer_ <<- SpatialPolygons2PolySet(pulayer)
# prepare the map: puoutline object
puoutline <- readShapeLines(paste0(sMarxanDir,"/pulayer/tas.shp"))
puoutline <<- SpatialLines2PolySet(puoutline)
# prepare the planning unit status object
pustatus <- read.csv(paste0(sMarxanDir,"/input/pu.dat"))
pustatus_ <<- unlist(sqldf("SELECT status from pustatus"))
# save the R binary object
save(pulayer_,puoutline,pustatus_,file=paste0(sMarxanDir,"/pulayer/pulayer.Rdata"))

###########
# compute the scenario 4 cost weightings

sMarxanDir <- "/Users/matthewwatts/Documents/zzz/RunMarxan_scenario4/p/files"

costweightings <- read.csv(paste0(sMarxanDir,"/input/cost_weightings.csv"))
pucosts <- read.csv(paste0(sMarxanDir,"/input/pu_cost.csv"))
pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"))

dim(costweightings)
dim(pucosts)
#colnames(pucosts)
dim(pudat)

sum(unlist(pucosts[2,2:22]) * costweightings$Weighting)
costweightings
# normalise costs
head(pucosts)
for (i in 2:dim(pucosts)[2])
{
  cat(paste0(max(pucosts[,i])),"  ")
  #pucosts[,i] <- pucosts[,i]/max(pucosts[,i])
}
save(pucosts,file=paste0(sMarxanDir,"/input/pu_cost_norm.Rdata"))
write.csv(pucosts,file=paste0(sMarxanDir,"/input/pu_cost_norm.csv"))

for (i in 1:dim(pudat)[1])
{
    pudat$cost[i] <- sum(unlist(pucosts[i,2:22]) * costweightings$Weighting)
}
head(pudat)


specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
as.character(specdat$name)