args <- commandArgs(TRUE)

if(length(args)==0){
  print("No arguments supplied.")
  # Default values
  map_dir    <- "~"
  seg_size   <- 125
  runmode    <- "test"
} 

map_dir    <- args[[1]]
seg_size   <- as.numeric(args[[2]])
runmode    <- args[[3]]


init_time_0 <- Sys.time()
write(paste("Started at", init_time_0),stdout())
source("png_map_reader.R")
source("map_grid_maker.R")
source("las_reader.R")
source("dt_segment_lookup.R")
source("omap_colour_codes.R")
# # 
# map_dir    <- "~/LIU/kartor/kvarn_liten"
# seg_size   <- 125
# runmode    <- 2

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

# To allow for running test mode with fewer segments
end_seg    <- suppressWarnings(ifelse(is.na(as.integer(runmode)), 
                                      nrow(omap_grid),
                                      min(as.integer(runmode), nrow(omap_grid))))

write(paste(nrow(las@data),"points to be looked up using", end_seg, "segments" ), stdout())
init_time_1 <- Sys.time()

# Process laslookup in sfm chunkwise
cl         <- parallel::makeForkCluster(floor(parallel::detectCores()/2))
las_sfm_lookup <- dt_lookup_factory(map_grid = omap_grid, 
                                    source_data = las@data,
                                    source_var = c("X","Y","Z","Intensity"), 
                                    target_data = sfm@data, 
                                    target_var = c("X", "Y", "R", "G", "B"), 
                                    target_tol = sfm_tol, fun = dt_closest, cl = cl)

las_sfm_join <- rbindlist(lapply(X = seq.int(1, end_seg), FUN = las_sfm_lookup))
setnames(las_sfm_join, names(las_sfm_join), c("X", "Y", "Z", "Intensity", "R", "G", "B"))
save(las_sfm_join, file = paste0(map_dir, "/las_lookup.Rdata"))
stopCluster(cl)

# Tell the user we're done
end_time_1 <- Sys.time()
total_time <- difftime(end_time_1, init_time_1, units = "mins")
record_time<- as.numeric(total_time) * 60 / (nrow(las_sfm_join)/1000)
time_record<- nrow(las_sfm_join) / as.numeric(total_time)
write(paste("Result saved to disk.\nTotal time:", round(total_time, 2), 
            "minutes.\nTime per 1K points:", round(record_time, 4), 
            "seconds\nRecords per minute:", round(time_record, 1)), 
      stdout())

init_time_2 <- Sys.time()

# Process las-lookup in omap
cl         <- parallel::makeForkCluster(floor(parallel::detectCores()/2))
las_omap_lookup <- dt_lookup_factory(map_grid = omap_grid, 
                                    source_data = las_sfm_join,
                                    source_var = c("X","Y","Z","Intensity", "R", "G", "B"), 
                                    target_data = omap, 
                                    target_var = c("X", "Y", "category"), 
                                    target_tol = 1, fun = dt_closest, cl = cl)

las_omap_join <- rbindlist(lapply(X = seq.int(1,end_seg), FUN = las_omap_lookup))
setnames(las_omap_join, c(names(las_sfm_join), "category"))

stopCluster(cl)
save(las_omap_join, file = paste0(map_dir, "/las_lookup_2.Rdata"))

# Tell the user we're done
end_time_2 <- Sys.time()
total_time <- difftime(end_time_2, init_time_2, units = "mins")
record_time<- as.numeric(total_time) * 60 / (nrow(las_sfm_join)/1000)
time_record<- nrow(las_sfm_join) / as.numeric(total_time)
write(paste("Result saved to disk.\nTotal time:", round(total_time, 2), 
            "minutes.\nTime per 1K points:", round(record_time, 4), 
            "seconds\nRecords per minute:", round(time_record, 1)), 
      stdout())



