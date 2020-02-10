args <- commandArgs(TRUE)

if(length(args)==0){
  print("No arguments supplied. Using defaults!")
  # Default values
  map_dir    <- "~"
  seg_size   <- 125
  runmode    <- 4
  output_dir <- paste0(getwd(), "/area_output/")
} else {
  map_dir    <- args[[1]]
  seg_size   <- as.numeric(args[[2]])
  runmode    <- args[[3]]
  output_dir <- args[[4]]
}

init_time_0 <- Sys.time()
write(paste("Started at", init_time_0),stdout())
source("png_map_reader.R")
source("map_grid_maker.R")
source("las_reader.R")
source("dt_segment_lookup.R")
source("omap_colour_codes.R")
source("little_helpers.R")
#
map_dir    <- "~/kartor/kvarn_liten"
seg_size   <- 125
runmode    <- 4
output_dir <- paste0(getwd(), "/dataprep/kvarn_liten")

# Ensure output directory exists
if(!dir.exists(output_dir)) {dir.create(output_dir)}

# Create set of true labels
true_labels<- create_true_labels()

# Read Omap-png and create grid for lookup from that
omap       <- png_map_reader(paste0(map_dir, "/omap_ren"), true_labels)
omap_grid  <- map_grid_maker(omap, seg_size = seg_size)

# Read relevant LiDAR files
las_tol    <- 0
las        <- las_reader(map_dir, omap_grid, "las", las_tol)

# Read relevant surfance model files
sfm_tol    <- 1
sfm        <- las_reader(map_dir, omap_grid, "sfm", sfm_tol)

end_time_0 <- Sys.time()
write(paste0("Prework done in ", 
             round(difftime(end_time_0, init_time_0, units = "secs"),1), " s."),
      stdout())

# To allow for running test mode with fewer segments, try and cast runmode as integer
# If possible only run that many segments, else run all.
end_seg    <- suppressWarnings(ifelse(is.na(as.integer(runmode)), 
                                      nrow(omap_grid),
                                      min(as.integer(runmode), nrow(omap_grid))))

write(paste(nrow(las@data),"points to be looked up using", end_seg, "segments" ), stdout())

# Process laslookup in sfm chunkwise
init_time_1 <- Sys.time()
cl         <- parallel::makeForkCluster(floor(parallel::detectCores()/2))
las_sfm_lookup <- dt_lookup_factory(map_grid = omap_grid, 
                                    by = c("X", "Y"),
                                    source_data = las@data,
                                    source_var = c("Z","Intensity"), 
                                    target_data = sfm@data, 
                                    target_var = c("R", "G", "B"), 
                                    target_tol = sfm_tol, fun = dt_closest, cl = cl)

las_sfm_join <- rbindlist(lapply(X = seq.int(1, end_seg), FUN = las_sfm_lookup))
stopCluster(cl)
timing_writer(init_time_1, Sys.time(), nrow(las_sfm_join))

# Process las-lookup in omap
init_time_2 <- Sys.time()
cl         <- parallel::makeForkCluster(floor(parallel::detectCores()/2))
las_omap_lookup <- dt_lookup_factory(map_grid = omap_grid, 
                                     by = c("X", "Y"),
                                     source_data = las_sfm_join,
                                     source_var = c("Z","Intensity", "R", "G", "B"), 
                                     target_data = omap, 
                                     target_var = c("cat_id"), 
                                     target_tol = 1, fun = dt_closest, cl = cl)

#las_omap_join <- rbindlist(lapply(X = seq.int(1,end_seg), FUN = las_omap_lookup))
las_omap_join <- lapply(X = seq.int(1,end_seg), FUN = las_omap_lookup)
stopCluster(cl)
save(las_omap_join, file = paste0(map_dir, "/las_lookup_2.Rdata"))
timing_writer(init_time_2, Sys.time(), nrow(las_sfm_join))

seg_writer <- seg_list_writer_factory(output_dir, las_omap_join)
lapply(1:length(X = seq.int(1,4)), seg_writer)
