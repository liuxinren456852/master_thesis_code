suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  # make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
  #             help="Print extra output [default]"),
  # make_option(c("-q", "--quietly"), action="store_false", 
  #             dest="verbose", help="Print little output"),
  make_option(c("-m", "--source_dir"), type="character", default="~/kartor/", 
              help="Directory where source maps are located as subfolders [default %default]",
              dest = "source_dir"),
  make_option(c("-o", "--output_dir"), type = "character", default = paste0(getwd(), "/area_output/"),
              help = "Directory where to output treated files. [default %default]",
              dest = "output_dir"),
  make_option(c("-s", "--seg_size"), type = "integer", default = 100,
              help = "Size of chunks to be processed and represent sub area [default %default]",
              dest = "seg_size"),
  make_option(c("-r", "--runmode"), type = "character", default = "4",
              help = "Maximum number of segments to run. Non-integer to run all [default %default]",
              dest = "runmode")
  )

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

source("png_map_reader.R")
source("map_grid_maker.R")
source("las_reader.R")
source("dt_segment_lookup.R")
source("omap_colour_codes.R")
source("little_helpers.R")
require(lidR)

init_time_0<- Sys.time()
source_dir <- opts$source_dir
output_dir <- opts$output_dir
seg_size   <- opts$seg_size
runmode    <- opts$runmode

# Create set of true labels and catalogs of las and sfm-files
true_labels<- create_true_labels()
las_cat    <- catalog(paste0(source_dir, "_laserdata"))
sfm_cat    <- catalog(paste0(source_dir, "_ytmodell"))

# Traverse input dir and ensure output directories exists
areas      <- dir(source_dir)
areas      <- areas[!areas %in% c("_laserdata", "_ytmodell", "_other")]
write(paste("Started at", init_time_0,"\nFound ", length(areas), "areas to process."),
      stdout())
if(!dir.exists(output_dir)) {dir.create(output_dir)}
for(area in areas){
  area_init   <- Sys.time()
  write(paste0("Starting area ", area, " at ", area_init), stdout())
  curr_output <- paste0(output_dir, area, "/")
  curr_source <- paste0(source_dir, area, "/")
  if(!dir.exists(curr_output)) {dir.create(curr_output)}
  
  # Read Omap-png and create grid for lookup from that
  mapname    <- dir(curr_source,".png$") 
  omap       <- png_map_reader(mapfile = paste0(curr_source, mapname), true_categories = true_labels)
  omap_grid  <- map_grid_maker(omap, seg_size = seg_size)
  omap_ext   <- raster::extent(c(range(omap[,X]), range(omap[, Y])))
  curr_las_c <- catalog_intersect(las_cat, map_extent)
  curr_sfm_c <- catalog_intersect(sfm_cat, map_extent)
  
  # Read relevant LiDAR files
  las_tol    <- 0
  las        <- las_reader(curr_las_c, map_grid = omap_grid, type = "las", tol = las_tol)
  
  # Read relevant surfance model files
  sfm_tol    <- 1
  sfm        <- las_reader(sfm_cat, map_grid = omap_grid, type = "sfm", tol = sfm_tol)
  
  write(paste0("Prework done in ", 
               round(difftime(Sys.time(), area_init, units = "secs"),1), " s."),
        stdout())
  
  # To allow for running test mode with fewer segments, try and cast runmode as integer
  # If possible only run that many segments, else run all.
  end_seg    <- suppressWarnings(ifelse(is.na(as.integer(runmode)), 
                                        nrow(omap_grid),
                                        min(as.integer(runmode), nrow(omap_grid))))
  
  #write(paste(nrow(las@data),"points to be looked up using", end_seg, "segments" ), stdout())
  
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
                                       target_var = c("category"), 
                                       target_tol = 1, fun = dt_closest, cl = cl)
  
  #las_omap_join <- rbindlist(lapply(X = seq.int(1,end_seg), FUN = las_omap_lookup))
  las_omap_join <- lapply(X = seq.int(1,end_seg), FUN = las_omap_lookup)
  stopCluster(cl)
  # save(las_omap_join, file = paste0(source_dir, "/las_lookup_2.Rdata"))
  #timing_writer(init_time_2, Sys.time(), nrow(las_sfm_join))
  
  seg_writer <- seg_list_writer_factory(output_dir = curr_output, 
                                        data_list = las_omap_join)
  invisible(lapply(1:length(x = seq.int(1,end_seg)), seg_writer))
  timing_writer(init_time_2, Sys.time(), nrow(las_sfm_join))
}
write(paste0(length(areas), " areas finished in ", 
             round(difftime(Sys.time(), init_time_0, units = "mins"),1), " min."),
      stdout())
