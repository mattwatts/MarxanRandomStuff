#################
# Gurobi: scenario 2

sMarxanDir <- "/Users/matthewwatts/Documents/zzz/marxan_scenario2/"

library(gurobi)
library(sqldf)

###############
# Transform Marxan data to prepare it for Gurobi.
# Save the Gurobi data as R binary objects.
###############


# generate targets in gurobi format
# get totalarea from total area file
tafile <- read.csv(paste0(sMarxanDir,"/MarOptTotalAreas.csv"))
tafile <- sqldf("SELECT spname,totalarea from tafile")
colnames(tafile)[1] <- "spid"
head(tafile)
# get prop from spec.dat file
specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
colnames(specdat)[1] <- "spid"
specdat <- sqldf("SELECT spid, prop from specdat")
head(specdat)
# join total area file to spec.dat file
specdat <- sqldf("SELECT * from specdat LEFT JOIN tafile USING(spid)")
head(specdat)
# compute target
specdat$target <- specdat$prop * specdat$totalarea
head(specdat)

gtarg_ <- sqldf("SELECT target from specdat")
gtarg_ <- unlist(gtarg_$target)

save(gtarg_,file=paste0(sMarxanDir,"/input/gtarg_.Rda"))

# generate costs in gurobi format
pudat <- read.csv(paste(sMarxanDir,"/input/pu.dat",sep=""))
colnames(pudat)
gcost_ <- sqldf("SELECT cost from pudat")
gcost_ <- unlist(gcost_$cost)

save(gcost_,file=paste0(sMarxanDir,"/input/gcost_.Rda"))

# generate PUID array
pudat <- read.csv(paste(sMarxanDir,"/input/pu.dat",sep=""))
puid_ <- sqldf("SELECT id from pudat")
puid_ <- unlist(puid_$id)

save(puid_,file=paste0(sMarxanDir,"/input/puid_.Rda"))

# generate matrix in gurobi format
# takes around 10 minutes to run
specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
fnames_ <- sqldf("SELECT id from specdat")

pudat <- read.csv(paste(sMarxanDir,"/input/pu.dat",sep=""))
punames <- sqldf("SELECT id from pudat")

puvspdat <- read.csv(paste(sMarxanDir,"/input/puvsp.dat",sep=""))
row1 <- rep(0,dim(specdat)[1])
puvspq <- sqldf(paste0("SELECT species, amount from puvspdat where pu = ",punames[1,1]))
if (nrow(puvspq) > 0)
{
  for (j in 1:nrow(puvspq))
  {
    amount <- puvspq[j,2]
    if (is.na(amount) == TRUE)
    {
      amount <- 0
    }
    row1[which(fnames_$id==puvspq[j,1])] <- amount
  }
}
rnames <- c(1)

for (i in 2:dim(pudat)[1])
{
  rowN <- rep(0,dim(specdat)[1])
  puvspq <- sqldf(paste0("SELECT species, amount from puvspdat where pu = ",punames[i,1]))
  if (nrow(puvspq) > 0)
  {
    for (j in 1:nrow(puvspq))
    {
      amount <- puvspq[j,2]
      if (is.na(amount) == TRUE)
      {
        amount <- 0
      }
      rowN[which(fnames_$id==puvspq[j,1])] <- amount
    }
  }
  row1 <- rbind(row1,rowN)
  rnames <- c(rnames,i)
}

rownames(row1) <- rnames

gmtx_ <- row1
save(gmtx_,file=paste0(sMarxanDir,"/input/gmtx_.Rda"))

gmtx_t_ <- t(row1)
save(gmtx_t_,file=paste0(sMarxanDir,"/input/gmtx_t_.Rda"))

dim(gmtx_t_)

# parse the costs and the matrix, only including rows that have cost > 0 and feature amounts > 0
load(paste0(sMarxanDir,"/input/gcost_.Rda"))
load(paste0(sMarxanDir,"/input/gmtx_.Rda"))
load(paste0(sMarxanDir,"/input/puid_.Rda"))

puid_filter_ <- c()
gcost_filter_ <- c()

iMtxRowCount <- 0
rnames <- c()

# remember the planning units filtered because of no features, and their cost
rFilteredCost <- 0
PuFiltered <- c()

for (i in 1:length(puid_))
{
  if (gcost_[i] > 0)
  {
    if (sum(gmtx_[i,]) > 0)
    {
      puid_filter_ <- c(puid_filter_,puid_[i])
      gcost_filter_ <- c(gcost_filter_,gcost_[i])
      if (iMtxRowCount == 0)
      {
        gmtx_filter_ <- gmtx_[i,]        
      } else {
        gmtx_filter_ <- rbind(gmtx_filter_,gmtx_[i,])
      }
      iMtxRowCount <- iMtxRowCount + 1
      rnames <- c(rnames,iMtxRowCount)
    } else {
      # remember the planning units filtered because of no features, and their cost
      rFilteredCost <- rFilteredCost + gcost_[i]
      PuFiltered <- c(PuFiltered,puid_[i])
    }
  }
}

# remember the planning units filtered because of no features, and their cost
write(paste0('filtered cost ',rFilteredCost),file=paste0(sWorkDir,"/input/filtered_cost.txt"))
save(PuFiltered,file=paste0(sMarxanDir,"/input/PuFiltered.Rda"))

rownames(gmtx_filter_) <- rnames
gmtx_filter_t_ <- t(gmtx_filter_)

save(gmtx_filter_,file=paste0(sMarxanDir,"/input/gmtx_filter_.Rda"))
save(gmtx_filter_t_,file=paste0(sMarxanDir,"/input/gmtx_filter_t_.Rda"))
save(puid_filter_,file=paste0(sMarxanDir,"/input/puid_filter_.Rda"))
save(gcost_filter_,file=paste0(sMarxanDir,"/input/gcost_filter_.Rda"))

##################
# construct the matrix in a format for lpSolve
load(paste0(sMarxanDir,"/input/gcost_filter_.Rda"))
load(paste0(sMarxanDir,"/input/gmtx_filter_.Rda"))

lpsmtx_ <- cbind(gcost_filter_,gmtx_filter_)
dim(lpsmtx_)

specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
fnames_ <- sqldf("SELECT id from specdat")

cnames <- c("COST")
for (i in 1:dim(fnames_)[1])
{
  cnames <- c(cnames,paste0("SPEC.",fnames_[i,1]))
}
colnames(lpsmtx_) <- cnames

save(lpsmtx_,file=paste0(sMarxanDir,"/input/lpsmtx_.Rda"))

##################
# construct the model: ILP == "B"
load(paste0(sMarxanDir,"/input/gtarg_.Rda"))
load(paste0(sMarxanDir,"/input/gcost_filter_.Rda"))
load(paste0(sMarxanDir,"/input/gmtx_filter_t_.Rda"))
load(paste0(sMarxanDir,"/input/puid_filter_.Rda"))

length(gtarg_)
length(gcost_filter_)
dim(gmtx_filter_t_)
length(puid_filter_)

model <- list()

model$A <- gmtx_filter_t_
model$obj <- gcost_filter_
model$sense <-  rep(">=",each = length(gtarg_)) 
model$rhs <- gtarg_
model$vtype <- "B"

dim(model$A)
length(model$obj)
length(model$sense)
length(model$rhs)

params <- list(Presolve=0,TimeLimit=1000.0)

# solve the problem
result <- gurobi(model,params)

# browse the results
print(result$objval) # planning unit cost
print(result$x) # selected planning units

# count selected planning units
sum(result$x)

# get list of selected planning units puid's
selectedpus <- result$x * puid_filter_
pus <- c()
for (i in 1:length(selectedpus))
{
  if (selectedpus[i] > 0)
  {
    pus <- c(pus,selectedpus[i])
  }
}

# save the results
write.csv(result$objval,file=paste0(sMarxanDir,"/output/gurobi_objval.csv"))
write.csv(pus,file=paste0(sMarxanDir,"/output/gurobi_pus.csv"))
save(pus,file=paste0(sMarxanDir,"/output/gurobi_pus.Rda"))
save(result,file=paste0(sMarxanDir,"/output/gurobi_result.Rda"))

##################
# run the model with lpSolve and the filtered data
# run ILP for Tas Activity

load(paste0(sMarxanDir,"/input/lpsmtx_.Rda"))
load(paste0(sMarxanDir,"/input/gtarg_.Rda"))

library(lpSolveAPI)

n <- length(lpsmtx_[,1])#no. of variables
m <- length(gtarg_)#no. of constraints
msp1coln <- as.vector(colnames(lpsmtx_)) #vector containing column names of msp1 (for oview-matrix)
puid <- as.vector(rownames(lpsmtx_))

f <- make.lp(0,n)
set.objfn(f, c(lpsmtx_[,1]))

for(i in 1:m){
  add.constraint(f,c(lpsmtx_[,i+1]), ">=", gtarg_[i])
}

set.bounds(f, upper = rep(1,n), columns = seq(1,n))
set.type(f, columns = seq(1,n), type = "binary")

lp.control(f, timeout = 1000, verbose = "detailed")
#lp.control(f, timeout = 10, verbose = "normal")

# run lp_solve
solve(f)

#get results
obj <- get.objective(f)
var <- get.variables(f)

#selected sites (indices of var which are /= 0)
sel <- which(var>0)

puidsel <- c() #pu ids of the selected sites
for(i in 1:length(sel)){
  puidsel[i] <- puid[sel[i]]
}
puidsel
length(puidsel)
#mean cost
mcost <- mean(c(lpsmtx_[,1]))
mcost

#standard deviation cost
#(mean distance from mcost)
sdcost <- sd(c(lpsmtx_[,1]))
sdcost

write.csv(get.objective(f), paste0(sMarxanDir,"/output/output_ilp_objective.csv"))
write.csv(get.variables(f), paste0(sMarxanDir,"/output/output_ilp_variables.csv"))
write.csv(puidsel, paste0(sMarxanDir,"/output/output_ilp_puidsel.csv"))
write.csv(c(mcost, sdcost), paste0(sMarxanDir,"/output/output_ilp_costana.csv"))
