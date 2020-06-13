suppressPackageStartupMessages(library("optparse"))

option_list <- list( 

  make_option(c("-m", "--map_source"), type = "character", default="~/master_thesis_code/omap_cleaned/", 
              help="Directory where source maps are located as subfolders [default %default]",
              dest = "map_source"),
  make_option(c("-o", "--output_dir"), type = "character", default = paste0(getwd(), "/area_output2/"),
              help = "Directory where to output treated files. [default %default]",
              dest = "output_dir"),
  make_option(c("-l", "--las_source"), type = "character", default = "/media/gustav/storage/laslager/",
              help = "Directory where lasfiles are located [default %default]",
              dest = "las_source"),
  make_option(c("-s", "--seg_size"), type = "integer", default = 120,
              help = "Size of chunks to be processed and represent sub area [default %default]",
              dest = "seg_size"),
  make_option(c("-r", "--runmode"), type = "character", default = "all",
              help = "Maximum number of segments to run. Non-integer to run all [default %default]",
              dest = "runmode"),
  make_option(c("-c", "--cores"), type = "integer", default = parallel::detectCores()-1,
              help = "Number of cores to use [default %default]",
              dest = "num_cores"),
  make_option(c("-p", "--pred_dim"), type = "character", default = NULL,
              help = "Pixel equivalent to do prediction data (i.e. no labels). Text string like XdimxYdim, f.ex. 1280x1024  [default %default]",
              dest = "pred_dim")
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

# Test wether a prediction material is to be made, i.e. no ground truth omap exists
if(regexpr("^[0-9]*x[0-9]*$", opts$pred_dim)==1){
  predictions <- TRUE
  pred_width  <- as.integer(strsplit(opts$pred_dim, "x")[[1]][1])
  pred_height <- as.integer(strsplit(opts$pred_dim, "x")[[1]][2])
}

# To normalize intensity and RGB-values to [0,1]
rgb_max    <- 2^16
intens_max <- 255

# Create set of true labels and catalogs of las and sfm-files
true_labels<- create_true_labels()

# Traverse input dir and ensure output directories exists
areas      <- dir(map_source)
areas      <- areas[!areas %in% c("laserdata", "ytmodell", "other","zlim.txt")]
main_areas <- substr(areas,1,5)
area_idx   <- 0
las_zlim   <- read.delim(paste0(las_source,"zlim.txt"))
write(paste("Started at", init_time_0,"\nFound ", length(areas), "areas to process."),
      stdout())
if(!dir.exists(output_dir)) {dir.create(output_dir)}
print(areas)

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
  

  # Create blank map if predictions are to be made
  if(predictions){
    omap <- data.table(expand.grid(list(pX = seq.int(1, pred_width), 
                                        pY = seq.int(1, pred_height))),
                       category = 99)
    # Could be done nicer, mainly borrowed from the png_map_reader file
    pred_mapname  <- sub(".pgw","",dir(curr_map_sc,pattern = ".pgw",full.names = TRUE))
    map_trns <- .png_worldfile_to_transform_matrix(pred_mapname)
    map_xy   <- data.table(as.matrix(cbind(omap[, .(pX,pY)], 1)) %*% map_trns)
    
    # Adds X and Y to map data.
    omap[, c("X", "Y") := map_xy]
    setcolorder(omap, c("X", "Y", "category", "pX", "pY"))
    setkey(omap, X, Y)
    
    # Save data needed for later predictions gridding
    pred_omap_data <- list(map = omap[c(1, nrow(omap)),], resolution = map_trns[1,1])
    save(pred_omap_data, file = paste0(curr_map_sc, "pred_omap_data.Rdata"))
  } else { 
    # Read Omap-png and create grid for lookup from that
    premade_omap<- dir(curr_map_sc, ".Rdata", full.names = TRUE)
    
    if(length(premade_omap) == 0){
      write("Creating map data", "")
      mapname     <- dir(curr_map_sc,".png$") 
      omap        <- png_map_reader(mapfile = paste0(curr_map_sc, mapname), 
                                    true_categories = true_labels)
      save(omap, file = paste0(area, "_omap.Rdata"))
      map_dt_plot(omap, colour = "cat_colour")
    } else {
      write(paste0("Loading map data from", premade_omap),"")
      load(premade_omap[1])
    }
  }
  # The omap_grid decides the segmentation but could be made from something else than
  # a png file if necessary.
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
