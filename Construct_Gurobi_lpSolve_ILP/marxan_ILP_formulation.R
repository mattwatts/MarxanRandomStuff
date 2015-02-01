#### Initialization
set.seed(500)

# set user params
n_pu=100
n_spp=2
blm=1
spp_targets=c(0.2, 0.2)
spp_spfs=c(100000,100000)

# load deps
library(marxan)
library(RandomFields)
library(plyr)
library(lpSolveAPI)

# define functions
test=function(){source('C:/Users/jeff/Documents/GitHub/MarxanRandomStuff/Construct_Gurobi_lpSolve_ILP/marxan_ILP_formulation.R')}

#### Preliminary processing
# simulate pu data
n_pu=ceiling(sqrt(n_pu))^2
puRST=raster(matrix(n_pu, ncol=sqrt(n_pu), nrow=sqrt(n_pu)), xmn=0, xmx=100, ymn=0, ymx=100)
puRST=setValues(puRST, seq_len(n_pu))
puPOLYS=rasterToPolygons(puRST)

# simulate spp data
brks=seq(0.5, 99.5, length.out=n_pu*5)
model=RPbernoulli(RMexp(scale=20), threshold=1)
sppSTK=RFsimulate(model, brks, brks, n=n_spp)
sppSTK=stack(llply(seq_len(n_spp), function(x) {return(raster(sppSTK, layer=x))}))

# generate marxan dfs
puDF=data.frame(id=seq_len(n_pu), cost=1, status=0)
speciesDF=data.frame(id=seq_len(n_spp), target=spp_targets*cellStats(sppSTK, 'sum'), spf=spp_spfs)
boundDF=calcBoundaryData(puPOLYS)
puvspeciesDF=calcPuVsSpeciesData(puPOLYS, sppSTK)

#### Main processing
## sink model to cplex file format

### variable nomenclature
# sfX = binary variable to indicate if target not met for species X
sf.vars=paste0('sf',speciesDF$id)
# puX = binary variable to indicate if pu varible X is selected
pu.vars=paste0('pu',puDF$id)
# puX_puY = binary variable to indicate if pu X xor pu Y is selected
notedge=which(boundDF$id1!=boundDF$id2)
edge=which(boundDF$id1==boundDF$id2)
puxorpu.vars=paste0('pu',boundDF$id1[notedge],'_pu',boundDF$id2[notedge])

# ini
# cplex format http://lpsolve.sourceforge.net/5.5/CPLEX-format.htm
# common logical operations http://rutcor.rutgers.edu/~dpapp/om-07fall/logical_constraints.pdf
sink('C:/Users/jeff/Documents/GitHub/MarxanRandomStuff/Construct_Gurobi_lpSolve_ILP/marxan_model.lp', type='output')
cat('/* marxan model */\n\n')
cat('/* objective function */\n')
## objective function
cat('min: ')
obj.pucosts=paste0(puDF$cost, ' ', pu.vars)
obj.sfpenalties=paste0(speciesDF$spf, ' ', sf.vars)
obj.boundarypenalties=c(
	paste0(blm*boundDF$boundary[edge], ' pu', puDF$id[which(puDF$id %in% boundDF$id1[edge])]),
	paste0(blm*boundDF$boundary[notedge], ' ', puxorpu.vars)
)
cat(paste(c(obj.pucosts, obj.sfpenalties, obj.boundarypenalties), collapse=" + "),';\n',sep="")
cat('\n',sep="")
## constraints
# shortfall penalties
# http://www.aimms.com/aimms/download/manuals/aimms3om_integerprogrammingtricks.pdf

# leq=function(fx, alpha, y, U=1000, L=0) {
	# return(
		# (fx+((alpha+1-L)*y) >= alpha+1) &
		# (fx+((U-L)*y) <= alpha+U-L)
	# )
# }
# leq(1, 100, 1) & !leq(1, 100, 0)
# !leq(10, 5, 1) & leq(10, 5, 0)
cat('/* constraints */\n')

tempLST=list()
for (i in seq_len(n_spp)) {
	rows=which(puvspeciesDF$species==i)
	currU=sum(puvspeciesDF$amount[rows])+1
	curr_pus=paste0('pu',puvspeciesDF$pu[rows])
	curr_target=speciesDF$target[i]-1
	curr_repr=paste(paste0(puvspeciesDF$amount[rows], ' ', curr_pus), collapse=" + ")
	tempLST[[i]]=c(
		paste0(curr_repr, ' + ', curr_target+1, ' ',sf.vars[i],' >= ', curr_target+1),
		paste0(curr_repr, ' + ',currU,' ',sf.vars[i],' <= ', curr_target+currU)
	)
}
cons.sf=unlist(tempLST, recursive=FALSE, use.names=FALSE)
cat(paste0('sfcon',seq_along(cons.sf), ": ",cons.sf,';\n'), sep="")

# boundary constraints from stack overflow user Turnip
# http://stackoverflow.com/questions/2385389/xor-using-mathematical-operators
# http://brblog.typepad.com/files/mipformref-1.pdf

cons.boundary=c(
	paste0(puxorpu.vars,' <= pu',boundDF$id1[notedge],' + pu',boundDF$id2[notedge]),
	paste0(puxorpu.vars,' >= pu',boundDF$id1[notedge],' - pu',boundDF$id2[notedge]),
	paste0(puxorpu.vars,' >= -pu',boundDF$id1[notedge],' + pu',boundDF$id2[notedge]),
	paste0(puxorpu.vars,' <= 2 - pu',boundDF$id1[notedge],' + pu',boundDF$id2[notedge])
)
cat(paste0(cons.boundary,';\n'), sep="")

## variable types
cat('\n/* variable types */')
cat('\nbin ',
	paste(
		c(
			sf.vars,
			pu.vars,
			puxorpu.vars
		),
		collapse=", "
	),
	";\n",
	sep=""
)
sink(file=NULL)

# solve problem
lpmod=read.lp('C:/Users/jeff/Documents/GitHub/MarxanRandomStuff/Construct_Gurobi_lpSolve_ILP/marxan_model.lp', verbose='full')
solve(lpmod)
pus=get.variables(lpmod)[seq_len(n_pu)]
ta=get.variables(lpmod)[n_pu+seq_len(n_spp)]
con=get.variables(lpmod)[n_pu+n_spp+seq_len(sum(boundDF$id1!=boundDF$id2))]

# view solution
puPOLYS=SpatialPolygonsDataFrame(puPOLYS, data=data.frame(status=pus))
for (i in seq_len(n_spp)) {
	puamounts=tapply(puvspeciesDF$amount[which(puvspeciesDF$species==i)], puvspeciesDF$pu[which(puvspeciesDF$species==i)])
	puPOLYS@data[[paste0('spp',i)]]=replace(rep(0, n_pu), puvspeciesDF$pu[which(puvspeciesDF$species==i)], puamounts)
}
try(dev.off(), silent=TRUE)
try(dev.off(), silent=TRUE)
spplot(puPOLYS, c('status'))
x11()
spplot(puPOLYS, c('spp1','spp2'))






