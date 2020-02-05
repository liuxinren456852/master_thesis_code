# args = commandArgs(TRUE)
# 
# if(length(args)==0){
#   print("No arguments supplied.")
#   # Default values
#   map_dir <- "~"
# } else {
#   for(i in 1:length(args)){
#     eval(parse(text=args[[i]]))
#   }
# }

source("png_map_reader.R")
source("map_grid_maker.R")
source("las_filter_string.R")
source("dt_segment_lookup.R")
library(lidR)
library(parallel)

# Read Omap-png and create grid for lookup from that
map_dir    <- "~/kartor/kvarn_liten"
map        <- png_map_reader(paste0(map_dir, "/omap_ren"))
map_grid   <- map_grid_maker(map)

# Read relevant LiDAR files
las_tol     <- 0
laspath    <- paste0(map_dir, "/laserdata/")
lasfiles   <- paste0(laspath, dir(laspath, ".la[sz]+$"))
las_select <- las_filter_string(map_grid, tol = las_tol) 
las        <- readLAS(lasfiles, select = "xyzi", filter = las_select)
setkey(las@data, X, Y)

# Read relevant surfance model files
sfm_tol    <- 1
sfmpath    <- paste0(map_dir, "/ytmodell/")
sfmfiles   <- paste0(sfmpath, dir(sfmpath, ".la[sz]+$"))
sfm_select <- las_filter_string(map_grid, tol = sfm_tol) 
sfm        <- readLAS(sfmfiles, select = "xyzRGB", filter = sfm_select)
setkey(sfm@data, X, Y)

# Process laslookup in sfm chunkwise

las_sfm_lookup <- dt_lookup_factory(map_grid, las@data, sfm@data, sfm_tol, dt_closest)

#lapply(X = seq.int(1,nrow(map_grid)), FUN = function(seg_no){
las_lookup <- lapply(X = seq.int(1,4), FUN = las_sfm_lookup)

