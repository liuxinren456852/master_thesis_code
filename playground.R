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
source("las_reader.R")
source("dt_segment_lookup.R")

# Read Omap-png and create grid for lookup from that
map_dir    <- "~/kartor/kvarn_liten"
map        <- png_map_reader(paste0(map_dir, "/omap_ren"))
map_grid   <- map_grid_maker(map)

# Read relevant LiDAR files
las_tol    <- 0
las        <- las_reader(map_dir, map_grid, "las", las_tol)

# Read relevant surfance model files
sfm_tol    <- 1
sfm        <- las_reader(map_dir, map_grid, "sfm", sfm_tol)

# Process laslookup in sfm chunkwise
las_sfm_lookup <- dt_lookup_factory(map_grid, las@data, sfm@data, sfm_tol, dt_closest)

#lapply(X = seq.int(1,nrow(map_grid)), FUN = function(seg_no){
las_lookup <- lapply(X = seq.int(1,4), FUN = las_sfm_lookup)

