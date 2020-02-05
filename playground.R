args = commandArgs(TRUE)

if(length(args)==0){
  print("No arguments supplied.")
  # Default values
  map_dir <- "~"
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

source("png_map_reader.R")
source("map_grid_maker.R")
source("las_filter_string.R")
library(lidR)

# Read Omap-png and create grid for lookup from that
map        <- png_map_reader(paste0(map_dir, "/omap_ren"))
map_grid   <- map_grid_maker(map)

# Read relevant LiDAR files
las_tol     <- 0
laspath    <- paste0(map_dir, "/laserdata/")
lasfiles   <- paste0(laspath, dir(laspath, ".la[sz]+$"))
las_select <- las_filter_string(map_grid, tol = las_tol) 
las        <- readLAS(lasfiles, select = "xyzi", filter = las_select)

# Read relevant surfance model files
sfm_tol    <- 1
sfmpath    <- paste0(map_dir, "/ytmodell/")
sfmfiles   <- paste0(sfmpath, dir(sfmpath, ".la[sz]+$"))
sfm_select <- las_filter_string(map_grid, tol = sfm_tol) 
sfm        <- readLAS(sfmfiles, select = "xyzRGB", filter = sfm_select)

# 

  