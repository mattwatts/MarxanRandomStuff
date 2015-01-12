#### Initialization
# set user params
set.seed(500)
n_rep=100

# load deps
library(rgdal)
library(raster)
library(maptools)
library(Rcpp)
library(inline)
library(microbenchmark)
library(gdalUtils)
library(plyr)

# define functions
test=function(){source('C:/Users/jeff/Documents/GitHub/marxanutils/marxan_fast_extract.R')}

makePUs=function(n) {
  rast=raster(matrix(NA, ceiling(sqrt(n)), ceiling(sqrt(n))), xmn=-1, xmx=1, ymn=-1, ymx=1)
  rast=setValues(rast, seq_len(ncell(rast)))
  return(rasterToPolygons(rast, n=4, dissolve=FALSE))
}

makeRaster=function(n) {
	rast=raster(matrix(NA, ceiling(sqrt(n)), ceiling(sqrt(n))), xmn=-1, xmx=1, ymn=-1, ymx=1)
	return(setValues(rast, rnorm(ncell(rast))))
}


cppFunction(code='
	NumericVector rcpp_groupsum(IntegerVector cat_vec, NumericVector val_vec) {
		// init
		IntegerVector ids_vec=na_omit(sort_unique(cat_vec));
		IntegerVector levels_vec=match(cat_vec,ids_vec)-1;
		NumericVector ret_vec(ids_vec.size());

		// main
		for (int i=0; i<cat_vec.size(); ++i) {
			if (!IntegerVector::is_na(cat_vec[i]) && !NumericVector::is_na(val_vec[i])) {
				ret_vec[levels_vec[i]]+=val_vec[i];
			}
		}
		
		// exports
		ret_vec.attr("ids")=ids_vec;
		return(ret_vec);
	}
', include='
#include<vector>
#include<algorithm>
'
)

cppFunction(code='
	NumericVector rcpp_groupcombine(std::vector<NumericVector> group_sums) {
		// init
		std::vector<int> all_ids_vec;
		all_ids_vec.reserve(group_sums[0].size() * group_sums.size() * 10);
		std::vector<double> all_vals_vec;
		all_vals_vec.reserve(group_sums[0].size() * group_sums.size() * 10);
		
		// preliminary processing
		IntegerVector int_tmp;
		for (std::size_t i=0; i!=group_sums.size(); ++i) {
			int_tmp=group_sums[i].attr("ids");
			for (auto j=int_tmp.begin(); j!=int_tmp.end(); ++j)
				all_ids_vec.push_back(*j);
			for (auto j=group_sums[i].begin(); j!=group_sums[i].end(); ++j)
				all_vals_vec.push_back(*j);
		}
		int_tmp=wrap(all_ids_vec);
		
		// main processing
		IntegerVector ids_vec=wrap(na_omit(sort_unique(int_tmp)));
		IntegerVector levels_vec=match(int_tmp,ids_vec)-1;
		NumericVector ret_vec(ids_vec.size());
		
		for (std::size_t i=0; i!=all_vals_vec.size(); ++i) {
			ret_vec[levels_vec[i]]+=all_vals_vec[i];
		}

		// exports
		ret_vec.attr("ids") = ids_vec;
		return(ret_vec);
	}
', include='
#include<vector>
#include<algorithm>
', plugins=c("cpp11"),verbose=FALSE
)

is.gdalInstalled=function() {
	gdal_setInstallation()
	return(!is.null(getOption("gdalUtils_gdalPath")))
}

rasterize.gdal=function(polys, rast, field=NULL) {
	writeOGR(polys, tempdir(), 'polys', driver='ESRI Shapefile', overwrite=TRUE)
	writeRaster(setValues(rast, NA), file.path(tempdir(), 'rast.tif'), NAflag=-9999, overwrite=TRUE)
	return(gdal_rasterize(file.path(tempdir(), 'polys.shp'), file.path(tempdir(), 'rast.tif'), l="polys", a="id", output_Raster=TRUE)[[1]])
}

.zonalSum.RasterLayerInMemory=function(polys, rast, featureName) {
	tmp=rcpp_groupsum(getValues(polys),getValues(rast))
	return(data.frame(species=featureName, pu=attr(tmp, "ids"), amount=c(tmp)))
}

.zonalSum.RasterLayerNotInMemory=function(bs, polys, rast, featureName, ncores, registered) {
	if (registered & .Platform$OS.type=="windows")
		clusterExport(clust, c("bs","polys", "rast", "rcpp_groupsum"))
	tmp=rcpp_groupcombine(llply(seq_len(bs$n), .parallel=registered, function(i) {
		return(rcpp_groupsum(getValues(polys, bs$row[i], bs$nrows[i]), getValues(rast, bs$row[i], bs$nrows[i])))
	}))
	return(data.frame(species=featureName, pu=attr(tmp, "ids"), amount=c(tmp)))
}

zonalSum.RasterLayer=function(polys, rast, featureName="Layer.1", ncores=1) {
	if (canProcessInMemory(polys,2)) {
		return(.zonalSum.RasterLayerInMemory(polys, rast, featureName))
	} else {
		bs=blockSize(polys)
		if(ncores>1) {
			if (.Platform$OS.type=="windows") {
				library(doSNOW)
				clust=makeCluster(ncores, type="SOCK")
				clusterEvalQ(clust, {library(raster);library(Rcpp)})
				registerDoSNOW(clust)
			} else {
				library(doMC)
				registerDoMC(cores=ncores)
			}
		}
		return(.zonalSum.RasterLayerNotInMemory(bs, rast, featureName, ncores, ncores>1))
	}
}

zonalSum.RasterStack=function(polys, raststk, featureNames=names(raststk), ncores=1) {
	if (canProcessInMemory(polys,2)) {
		return(rbind.fill(llply(seq_len(nlayers(raststk)), function(l) {
				return(.zonalSum.RasterLayerInMemory(polys, raststk[[l]], featureNames[l]))
		})))
	} else {
		bs=blockSize(polys)	
		if (ncores>1) {
			if (.Platform$OS.type=="windows") {
				library(doSNOW)
				clust=makeCluster(ncores, type="SOCK")
				clusterEvalQ(clust, {library(raster);library(Rcpp)})
				clusterExport(clust, c("bs", "polys", "rcpp_groupsum"))
				registerDoSNOW(clust)
			} else {
				library(doMC)
				registerDoMC(cores=ncores)
			}
		}
		# main processing
		return(rbind.fill(llply(seq_len(nlayers(raststk)), function(l) {
			return(.zonalSum.RasterLayerNotInMemory(bs, polys, raststk[[l]], featureNames[l], registered=ncores>1))
		})))
	}
}

createFeatureDataFrame=function(polys, rast, featureNames=names(rast), field=NULL, ncores=1) {
	# check for invalid inputs
	stopifnot(inherits(polys, "SpatialPolygons"))
	stopifnot(inherits(rast, c("RasterLayer", "RasterStack", "RasterBrick")))
	if(inherits(rast, "RasterLayer") & length(featureNames)>1)
		warning("rast is a RasterLayer and multiple featureNames provided, so only first name will be used.")
	if (inherits(rast, c("RasterLayer", "RasterStack"))) {
		if (length(featureNames)!=nlayers(rast)) {
			warning("The number of layers in rast and the length of featureNames is different, so featureNames will be set as names(rast).")
			featureNames=names(rast)
		}
	}

	# prepare attribute table
	if (is.null(field)) {
		polys@data=data.frame(id=seq_len(nrow(polys@data)), row.names=row.names(polys@data))
	} else {
		polys@data=data.frame(id=polys@data[[field]], row.names=row.names(polys@data))
	}
	
	# generate raster layer with polygons
	temprast=rast
	if (inherits(rast, c("RasterBrick", "RasterStack")))
		temprast=rast[[1]]
	if (!canProcessInMemory(rast,2) & is.gdalInstalled()) {
		polys=rasterize.gdal(polys, temprast, field)
	} else {
		polys=rasterize(polys, temprast, method="ngb")
	}
	
	# main processing
	if (inherits(rast, "RasterLayer")) {
		return(zonalSum.RasterLayer(polys, rast, featureNames, ncores))
	} else {
		return(zonalSum.RasterStack(polys, rast, featureNames, ncores))
	}
}

#### Preliminary processing
# generate raster
cat("generating pu data..\n")
pus1=makePUs(16)
rast1=makeRaster(1024)
pus2=makePUs(1000)
rast2=makeRaster(50000)

#### Main processing
# run benchmark
cat("running benchmark data..\n")
bench=microbenchmark(
	"zonal; n=16/1024"=zonal(rast1, rasterize(pus1, rast1), fun='sum'),
	"custom_code; n=16/1024"=createFeatureDataFrame(pus1, rast1),
	"zonal; n=1k/50k"=zonal(rast2, rasterize(pus2, rast2), fun='sum'),
	"custom_code; n=1k/50k"=createFeatureDataFrame(pus2, rast2),
	times=100, unuit="s"
)
bench2=microbenchmark:::convert_to_unit(bench, "s")
bench2$expr=bench$expr
class(bench2)="data.frame"

#### Exports
boxplot(time~expr, data=bench2, ylab="time (s)", las=1)
