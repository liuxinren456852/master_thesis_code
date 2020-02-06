args <- commandArgs(TRUE)

if(length(args)==0){
  print("No arguments supplied.")
  # Default values
  map_dir <- "~"
  seg_size <- 125
} 

map_dir    <- args[[1]]
seg_size   <- as.numeric(args[[2]])


init_time <- Sys.time()
write(paste("Started at", init_time),stdout())
source("png_map_reader.R")
source("map_grid_maker.R")
source("las_reader.R")
source("dt_segment_lookup.R")

#map_dir    <- "~/kartor/kvarn_liten"
#seg_size   <- 125

# Read Omap-png and create grid for lookup from that
map        <- png_map_reader(paste0(map_dir, "/omap_ren"))
map_grid   <- map_grid_maker(map, seg_size = seg_size)

# Read relevant LiDAR files
las_tol    <- 0
las        <- las_reader(map_dir, map_grid, "las", las_tol)

# Read relevant surfance model files
sfm_tol    <- 1
sfm        <- las_reader(map_dir, map_grid, "sfm", sfm_tol)

write(paste(nrow(las@data),"points to be looked up using", nrow(map_grid), "segments" ), stdout())
# Process laslookup in sfm chunkwise
cl <- parallel::makeForkCluster(floor(parallel::detectCores()/2))
las_sfm_lookup <- dt_lookup_factory(map_grid, las@data, sfm@data, sfm_tol, dt_closest, cl)

#las_lookup  <- lapply(X = seq.int(1,nrow(map_grid)), FUN = las_sfm_lookup)
las_lookup <- rbindlist(lapply(X = seq.int(1,5), FUN = las_sfm_lookup))

save(las_lookup, file = paste0(map_dir, "/las_lookup.Rdata"))

end_time <- Sys.time()
total_time <- difftime(end_time, init_time, "mins")
record_time <- difftime(end_time, init_time, "secs")/nrow(las@data)*1000
write(paste("Result saved to disk.\nTotal time:", round(total_time,1), 
             "minutes.\nTime per 1000 points:", round(record_time,1), "seconds" ), 
      stdout())
stopCluster(cl)
#load( paste0(map_dir, "/las_lookup.Rdata"))
