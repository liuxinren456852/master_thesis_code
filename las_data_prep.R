suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  # make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
  #             help="Print extra output [default]"),
  # make_option(c("-q", "--quietly"), action="store_false", 
  #             dest="verbose", help="Print little output"),
  make_option(c("-m", "--map_source"), type = "character", default="~/kartor/", 
              help="Directory where source maps are located as subfolders [default %default]",
              dest = "map_source"),
  make_option(c("-o", "--output_dir"), type = "character", default = paste0(getwd(), "/area_output/"),
              help = "Directory where to output treated files. [default %default]",
              dest = "output_dir"),
  make_option(c("-l", "--las_source"), type = "character", default = "~/laslager/",
              help = "Directory where lasfiles are located [default %default]",
              dest = "las_source"),
  make_option(c("-s", "--seg_size"), type = "integer", default = 100,
              help = "Size of chunks to be processed and represent sub area [default %default]",
              dest = "seg_size"),
  make_option(c("-r", "--runmode"), type = "character", default = "21",
              help = "Maximum number of segments to run. Non-integer to run all [default %default]",
              dest = "runmode"),
  make_option(c("-c", "--cores"), type = "integer", default = 6,
              help = "Number of cores to use [default %default]",
              dest = "num_cores")
  )

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

source("png_map_reader.R")
source("map_grid_maker.R")
source("las_reader.R")
source("dt_segment_lookup.R")
source("true_colour_codes.R")
source("little_helpers.R")
source("seg_list_writer.R")
suppressPackageStartupMessages(require(lidR))
suppressPackageStartupMessages(require(parallel))
init_time_0<- Sys.time()
map_source <- opts$map_source
output_dir <- opts$output_dir
las_source <- opts$las_source
seg_size   <- opts$seg_size
runmode    <- opts$runmode
num_cores  <- opts$num_cores

# To normalize intensity and RGB-values to [0,1]
rgb_max    <- 2^16
intens_max <- 255

# Create set of true labels and catalogs of las and sfm-files
true_labels<- create_true_labels()

# Traverse input dir and ensure output directories exists
areas      <- dir(map_source)
areas      <- areas[!areas %in% c("_laserdata", "_ytmodell", "_other")]
main_areas <- substr(areas,1,5)
area_idx   <- 0
las_zlim   <- read.delim(paste0(las_source,"zlim.txt"))
write(paste("Started at", init_time_0,"\nFound ", length(areas), "areas to process."),
      stdout())
if(!dir.exists(output_dir)) {dir.create(output_dir)}

for(area in areas){
  # Setup area specific variabels and check area is done already
  area_idx    <- area_idx + 1
  main_area   <- main_areas[area_idx]
  area_init   <- Sys.time()
  write(paste0("Starting area ", area, " at ", area_init), stdout())
  curr_output <- paste0(output_dir, "Area_", area_idx , "/")
  curr_map_sc <- paste0(map_source, area, "/")
  curr_las_sc <- paste0(las_source, main_area, "/")
  if (file.exists(paste0(curr_output, ".area"))) {
    write("Area done, skipping!", stdout())
    next
  }
  las_cat     <- catalog(paste0(curr_las_sc, "laserdata"))
  sfm_cat     <- catalog(paste0(curr_las_sc, "ytmodell"))
  curr_las_zlim <- unlist(las_zlim[las_zlim$main_area==main_area,c(2,3)])
  if(length(curr_las_zlim)==0) { curr_las_zlim <- setNames(c(0,250),c("min", "max")) }
  if(!dir.exists(curr_output)) { dir.create(curr_output) }
  
  # Read Omap-png and create grid for lookup from that
  mapname     <- dir(curr_map_sc,".png$") 
  omap        <- png_map_reader(mapfile = paste0(curr_map_sc, mapname), 
                                true_categories = true_labels)
  omap_grid   <- map_grid_maker(omap, seg_size = seg_size)
  
  # To allow for running test mode with fewer segments, try and cast runmode as integer
  # If possible only run that many segments, else run all.
  end_seg     <- suppressWarnings(ifelse(is.na(as.integer(runmode)), 
                                        nrow(omap_grid),
                                        min(as.integer(runmode), nrow(omap_grid))))
  
  omap_grid  <- omap_grid[rownum <= end_seg, ]
  end_seg_grp<- max(omap_grid[, rowgrp])

  
  done_seg   <- dir(paste0(output_dir, "Area_", area_idx))
  done_seg_id<- as.numeric(sub(x = done_seg, pattern="segment_", replacement = "")) -10
  omap_grid  <- omap_grid[!(rownum %in% done_seg_id),]
  seg_grp    <- min(omap_grid[, rowgrp])
  
  # Read relevant LiDAR files and normalize intensity
  las_tol    <- 0
  las        <- las_reader(las_cat, map_grid = omap_grid, select_string = "i", 
                           tol = las_tol, zlim = curr_las_zlim)
  las@data[, Intensity := Intensity / intens_max]
  
  # Read relevant surfance model files and normalize colours
  sfm_tol    <- 1
  sfm        <- las_reader(sfm_cat, map_grid = omap_grid, select_string = "*", 
                           tol = sfm_tol, zlim = curr_las_zlim)
  sfm@data[, R := R / rgb_max][, G := G / rgb_max][, B := B / rgb_max]
  alarm()
  write(paste("Prework done in", 
               round(difftime(Sys.time(), area_init, units = "secs"),1), "s.\n", 
               nrow(las@data), "points to look up using", nrow(omap_grid), "segments.\n"),
        stdout())
  
  while (seg_grp <= end_seg_grp){
    curr_grp    <- unlist(omap_grid[rowgrp == seg_grp, rownum])
    # Process laslookup in sfm
    init_time_1 <- Sys.time()
    cl         <- parallel::makeForkCluster(min(floor(parallel::detectCores())-1, num_cores))
# 
#     las_sfm_lookup <- dt_lookup_factory(map_grid = omap_grid, 
#                                         by = c("X", "Y"),
#                                         source_data = las@data,
#                                         source_var = c("Z","Intensity"),
#                                         target_data = sfm@data, 
#                                         target_var = c("R", "G", "B"), 
#                                         target_tol = sfm_tol, fun = .dt_closest, cl = cl)
# 
#     las_omap_lookup <- dt_lookup_factory(map_grid = omap_grid,
#                                          by = c("X", "Y"),
#                                          source_data = las_sfm_join,
#                                          source_var = c("Z", "Intensity", "R", "G", "B"),
#                                          target_data = omap,
#                                          target_var = c("category"),
#                                          target_tol = 5, fun = .dt_closest, cl = cl)
# 
   
# 
#     las_sfm_join <- parLapply(X = curr_grp, fun = las_sfm_lookup, cl = cl)
#     las_omap_join <- parLapply(X = curr_grp, fun = las_omap_lookup, cl = cl)
    
    las_fuzzy_join <- dt_fuzzy_join_factory(omap_grid, 
                                            by = c("X", "Y"),
                                            source_data = las@data,
                                            source_var = c("Z","Intensity"),
                                            target_data = list(sfm@data, omap),
                                            target_var = list(c("R", "G", "B"), c("category")), 
                                            target_tol = list(sfm_tol, 5), 
                                            fun = .dt_closest,
                                            cl = cl)
    
    las_join <- lapply(X = curr_grp, FUN = las_fuzzy_join)

    stopCluster(cl)
    
    seg_writer <- seg_list_writer_factory(output_dir = curr_output,
                                          data_list = las_join,
                                          seg_grp = seg_grp)
    
    invisible(lapply(seq.int(1,length(las_join)), seg_writer))

    timing_writer(init_time_1, Sys.time(), seg_grp, end_seg_grp, sum(sapply(las_join, nrow)))
    alarm()
    seg_grp    <- seg_grp + 1 
  }
  file.create(paste0(curr_output, ".area"))
  write(paste0("Area finished in ", 
               round(difftime(Sys.time(), area_init, units = "mins"),1), " min.\n\n"),
        stdout())
  gc()
}
write(paste0(length(areas), " areas finished in ", 
             round(difftime(Sys.time(), init_time_0, units = "mins"),1), " min."),
      stdout())
