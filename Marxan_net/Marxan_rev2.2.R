# Author: Matt Watts, m.watts@uq.edu.au, March 2014
# Copyright the University of Queensland 2013-2014
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE Version 3
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU AFFERO GENERAL PUBLIC LICENSE Version 3 for more details.
#
# See the license here http://www.gnu.org/licenses/agpl.txt
#
# Software purpose: Run Marxan, perform cluster analysis, display output
# graphs, maps and tables.
# This file, Marxan_rev2.2.R, contains the R function definitions.

#
# maps
#

library(sp)
library(maptools)
library(PBSmapping)

DisplaySsolnMap <- function(planningunits,displayzone,fTransparent)
{
  # display a summed solution map
  # displayzone is the zone we are displaying summed solution for
  blueramp <- colorRampPalette(c("white","blue"))(16) 
  if (isTRUE(fTransparent))
  {
    spplot(planningunits[paste0("SSOLN",displayzone)],col.regions=blueramp,col="transparent")
  } else {
    spplot(planningunits[paste0("SSOLN",displayzone)],col.regions=blueramp)
  }
}

DisplayMap <- function(planningunits,displayfield,fTransparent)
{
  # display map of a single solution
  # 0, best solution
  # 1..100, solution X
  greenramp <- colorRampPalette(c("white","green"))(2)
  if (displayfield<1)
  {
    # 0, best solution
    if (identical(TRUE,fTransparent))
    {
      spplot(planningunits["BESTSOLN"],col.regions= greenramp,col="transparent")
    } else {
      spplot(planningunits["BESTSOLN"],col.regions= greenramp)
    }
  } else {
    # 1..100, solution X
    if (identical(TRUE,fTransparent))
    {
      spplot(planningunits[paste0("SOLN",displayfield)],col.regions= greenramp,col="transparent")
    } else {
      spplot(planningunits[paste0("SOLN",displayfield)],col.regions= greenramp)
    }
  }
}

DisplaySsolnMapPBSm <- function(pupolygons,putable,displayzone,iNumReps,colour,fTransparent)
{
  # display a summed solution map
  # displayzone is the zone we are displaying summed solution for
  values <- sqldf(paste0("SELECT SSOLN",displayzone," from putable"))
  blueramp <- colorRampPalette(c("white",colour))(16)
  colours <- rep(blueramp[1],nrow(values))
  for (j in 1:nrow(values))
  {
    colours[j] <- blueramp[round(15 / iNumReps * values[j,])+1]
  }
  if (isTRUE(fTransparent))
  {
    plotPolys(pupolygons,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
  } else {
    plotPolys(pupolygons,col=colours,axes=FALSE,cex.lab=0.1,cex.axis=0.1)    
  }
}

DisplayMapPBSm <- function(pupolygons,putable,displayfield,colourpalette,fTransparent)
{
  # display map of a single solution
  # 0, best solution
  # 1..100, solution X
  if (displayfield<1)
  {
    values <- sqldf("SELECT BESTSOLN from putable")
  } else {
    values <- sqldf(paste0("SELECT SOLN",displayfield," from putable"))
  }
  colours <- rep(colourpalette[1],nrow(values))
  for (j in 1:nrow(values))
  {
    colours[j] <- colourpalette[values[j,]]
  }
  if (isTRUE(fTransparent))
  {
    plotPolys(pupolygons,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
  } else {
    plotPolys(pupolygons,col=colours,axes=FALSE,cex.lab=0.1,cex.axis=0.1)    
  }
}

#
# tables
#

GetOutputFileext <- function(sMarxanDir,sParam)
# For the specified Marxan output file, return the file extension (.csv or .txt)
# Scan input.dat for the parameter,
# if value = 1, .dat, tab delimited, no header
# if value = 2, .txt, comma delimited (Qmarxan sets this value)
# if value = 3, .csv, comma delimited
{
  inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
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

DisplaySumTable <- function(sMarxanDir)
{
  sumtable <- read.csv(paste0(sMarxanDir,"/output/output_sum",GetOutputFileext(sMarxanDir,"SAVESUMMARY")))
  View(sumtable)
}

GenerateMVFilename <- function(iRunNumber,sMarxanDir)
{
  sFilename <- paste0(sMarxanDir,"/output/output_mv")  
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste0(sFilename,"0")
    }
  }
  sFilename <- paste0(sFilename,iRunNumber,GetOutputFileext(sMarxanDir,"SAVETARGMET"))  
}

DisplayMVTable <- function(sMarxanDir,displayrun)
{
  # where displayrun = 0 means best
  #                  = 1..100 means run 1..100
  if (displayrun < 1)
  {
    sFilename <- paste0(sMarxanDir,"/output/output_mvbest",GetOutputFileext(sMarxanDir,"SAVETARGMET"))
  } else {
    
    sFilename <- GenerateMVFilename(displayrun,sMarxanDir)
  }
  mvtable <- read.csv(sFilename)
  View(mvtable)
}

#
# cluster analysis in Shiny
#

library(shiny)
library(vegan)
library(labdsv)

EucDist <- function(xloc,yloc,adataframe)
# Computes the euclidean distance from a x,y location to each point in a dataframe of x,y locations.
# This is used to find the closest solution to a point clicked.
# Called when user clicks on a 2d cluster graph in a Shiny Server interface.
# adataframe is the 2d points object computed by nmds()
{  
  mindistance <- 1000
  
  for (i in 1:dim(adataframe)[1]){
    
    x1 <- adataframe[i,][1]
    y1 <- adataframe[i,][2]
    distance <- sqrt(((x1 - xloc) ^ 2) + ((y1 - yloc) ^ 2))
    
    if (i==1){
      distances <- c(distance)
    } else {
      distances <- c(distances,distance)
    }
    
    if (distance < mindistance){
      mindistance <- distance
      closestpoint <- i
    }
  }
  
  return(closestpoint)
}

#
# Import map data from the output ascii files to the planning unit shape file dbf.
#

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

GenerateSolnFilename <- function(iRunNumber,sMarxanDir)
{
  sFilename <- paste0(sMarxanDir,"/output/output_r")  
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste0(sFilename,"0")
    }
  }
  sFilename <- paste0(sFilename,iRunNumber,GetOutputFileext(sMarxanDir,"SAVERUN"))  
}

library(foreign)
library(sqldf)

ImportOutputsCsvToShpDbf <- function(sPuShapeFileDbf, sMarxanDir, iNumberOfRuns, iNumberOfZones, sPUID)
# Imports the relevant contents of output files to the planning unit shape file dbf.
{
  # load and prepare pu_table
  pu_table <- read.dbf(sPuShapeFileDbf)
  
  if (length(which(regexpr(sPUID,colnames(pu_table))==1))==0)
  {
    print(paste0("Error: ", sPUID, "not found in ", sPuShapeFileDbf))
  }else{
    pu_table <- sqldf(paste0("SELECT ", sPUID, " from pu_table"))
    colnames(pu_table)[1] <- "PUID"
                    
    pu_table$PUID <- as.integer(pu_table$PUID)
  
    # load and prepare ssoln_table
    ssoln_table <- read.csv(paste0(sMarxanDir,"/output/output_ssoln",GetOutputFileext(sMarxanDir,"SAVESUMSOLN")))
    colnames(ssoln_table)[1] <- "PUID"
  
    if (iNumberOfZones > 2)
    {
      # read in the the SSOLN fields for multiple zones
      # "SELECT PUID, SSOLN1, SSOLN2, SSOLN3 from ssoln_table"
      sSelectSQL <- "SELECT PUID"
      for (i in 1:iNumberOfZones)
      {
        colnames(ssoln_table)[i+2] <- paste0("SSOLN",i)
        sSelectSQL <- paste0(sSelectSQL,", SSOLN",i)
      }
      sSelectSQL <- paste0(sSelectSQL," from ssoln_table")
    
      ssoln_table <- sqldf(sSelectSQL)
    } else {
      colnames(ssoln_table)[2] <- "SSOLN2"
      ssoln_table$SSOLN1 <- as.integer(iNumberOfRuns - ssoln_table$SSOLN2)
      ssoln_table$SSOLN2 <- as.integer(ssoln_table$SSOLN2)    
    }
  
    # join pu_table and ssoln_table
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table USING(PUID)")
  
    # load and prepare best_table
    best_table <- read.csv(paste0(sMarxanDir,"/output/output_best",GetOutputFileext(sMarxanDir,"SAVEBEST")))
    if (iNumberOfZones > 2)
    {
      # the field has a different field name in MarZone: "zone" instead of "SOLUTION"
      colnames(best_table)[1] <- "PUID"
      colnames(best_table)[2] <- "SOLUTION"
      best_table$BESTSOLN <- as.integer(best_table$SOLUTION)
    } else {
      best_table$BESTSOLN <- as.integer(best_table$SOLUTION + 1)
    }
    best_table <- sqldf("SELECT PUID, BESTSOLN from best_table")
  
    # join pu_table and best_table
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table USING(PUID)")
  
    for (i in 1:iNumberOfRuns)
    {
      sFieldName <- paste0("SOLN",i)
    
      # load and prepare solnX_table
      solnX_table <- read.csv(GenerateSolnFilename(i,sMarxanDir))
      if (iNumberOfZones > 2)
      {
        # the field has a different field name in MarZone: "zone" instead of "SOLUTION"
        colnames(solnX_table)[1] <- "PUID"
        colnames(solnX_table)[2] <- "SOLUTION"
        solnX_table[sFieldName] <- as.integer(solnX_table$SOLUTION)
      } else {
        solnX_table[sFieldName] <- as.integer(solnX_table$SOLUTION + 1)
      }
      solnX_table <- sqldf(paste0("SELECT PUID, ",sFieldName," from solnX_table"))
  
      # join pu_table and solnX_table
      pu_table <- sqldf("SELECT * from pu_table LEFT JOIN solnX_table USING(PUID)")
    
      rm(solnX_table)
    }
  
    # save the new pu_table
    colnames(pu_table)[1] <- sPUID
    write.dbf(pu_table,sPuShapeFileDbf)
  }
}

#
# Cluster analysis routines
#

ClusterUniqueSolutions <- function(sSolutionsMatrix)
# Returns the set of unique solutions from a solutions matrix.
# Returns NULL if there is less than 2 unique solutions.
{
  solutions_raw<-read.table(sSolutionsMatrix,header=TRUE, row.name=1, sep=",")
  solutions <- unique(solutions_raw)
  iUniqueSolutions <- dim(solutions)[1]
  if (iUniqueSolutions < 2)
  {
    return(NULL)
  } else {
    return(solutions)
  }
}

ClusterPlotNMDS <- function(solutions)
{
  soldist <- vegdist(solutions,distance="bray")
  
  sol.mds<-nmds(soldist,2)
  
  plot(sol.mds$points, type='n', xlab='', ylab='', main='NMDS of solutions')
  text(sol.mds$points, labels=row.names(solutions))
}


ClusterPlotDendogram <- function(solutions)
{
  soldist <- vegdist(solutions,distance="bray")
  
  h<-hclust(soldist, method="complete")
  
  plot(h, xlab="Solutions", ylab="Disimilarity", main="Bray-Curtis dissimilarity of solutions")
}


