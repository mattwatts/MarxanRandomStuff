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

puRasterize=function(polys, rast, field=NULL) {
	# prepare attribute table
	if (is.null(field)) {
		polys@data=data.frame(id=seq_len(nrow(polys@data)), row.names=row.names(polys@data))
	} else {
		polys@data=data.frame(id=polys@data[[field]], row.names=row.names(polys@data))
	}
	# main processing
	if (canProcessInMemory(rast,2)) {	
		# do processing in R via the raster package
		return(rasterize(polys, rast))
	} else {
		# do processing in gdal
		writeOGR(polys, tempdir(), 'polys', driver='ESRI Shapefile', overwrite=TRUE)
		writeRaster(setValues(rast, NA), file.path(tempdir(), 'rast.tif'), NAflag=-9999, overwrite=TRUE)
		return(gdal_rasterize(file.path(tempdir(), 'polys.shp'), file.path(tempdir(), 'rast.tif'), l="polys", a="id", output_Raster=TRUE)[[1]])
	}
}

puExtract=function(polys, rast, field=NULL, ncores=1, progressbar="none") {
	if (canProcessInMemory(polys,2)) {	
		return(rcpp_groupsum(getValues(polys), getValues(rast)))
	} else {
		# preprocessing
		out=rasterTmpFile()
		bs=blockSize(polys)
		# set up parallel processing
		if(ncores>1) {
			if (.Platform$OS.type=="windows") {
				library(doSNOW)
				registerDoSNOW(makeCluster(ncores, type="SOCK"))
			} else {
				library(doMC)
				registerDoMC(cores=ncores)
			}
		}
		# main processing 
		return(rcpp_groupcombine(llply(seq_len(bs$n), .parallel=ncores>1, .progress=progressbar, .parallel=ncores>1, function(i) {
			return(rcpp_groupsum(getValues(polys, bs$row[i], bs$nrows[i]), getValues(rast, bs$row[i], bs$nrows[i])))
		})))
	}
}

createFeatureDataFrame=function(polys, rast, field=NULL, ncores=1, progressbar="none") {
	# check data types
	stopifnot(inherits(polys, "SpatialPolygons"))
	# generate raster layer with polygons
	if (inherits(rast, "RasterLayer")) {
		polys=puRasterize(polys, rast, field)
	} else if (inherits(rast, c("RasterBrick", "RasterStack"))) {
		polys=puRasterize(polys, rast[[1]], field)
	} else {
		stop("rast should be of class: RasterLayer, RasterStack, RasterBrick")
	}
	# main processing	
	if (inherits(rast, "RasterLayer")) {
		return(data.frame(layer.1=puExtract(polys, rast, field, ncores, progressbar)))
	} else if (inherits(rast,c("RasterStack", "RasterBrick"))) {
		retLST=list()
		for (i in seq_len(nlayers(rast))) {
			retLST[[names(rast)[i]]]=puExtract(polys, rast, field, ncores, progressbar)
		}
		return(do.call(cbind.data.frame, retLST))
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
