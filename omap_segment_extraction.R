suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-m", "--source_dir"), type="character", default="omap_cleaned/", 
              help="Directory where source maps are located as subfolders [default %default]",
              dest = "source_dir"),
  make_option(c("-o", "--output_dir"), type = "character", default = paste0(getwd(), "/omap_cleaned/"),
              help = "Directory where to output treated files. [default %default]",
              dest = "output_dir"),
  make_option(c("-s", "--seg_size"), type = "integer", default = 120,
              help = "Size of chunks to be processed and represent sub area [default %default]",
              dest = "seg_size")
  )

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

source("png_map_reader.R")
source("map_grid_maker.R")
source("true_colour_codes.R")
source("little_helpers.R")

init_time_0<- Sys.time()
source_dir <- opts$source_dir
output_dir <- opts$output_dir
seg_size   <- opts$seg_size

tsegs <- readLines("test_segs.txt")
vsegs <- readLines("valid_segs.txt")

suppressPackageStartupMessages(library(stringr))
split_tseg  <- str_match(string = tsegs, pattern = "Area\\_([0-9]{1,2})\\_segment\\_([0-9]{2,3})")
split_vseg  <- str_match(string = vsegs, pattern = "Area\\_([0-9]{1,2})\\_segment\\_([0-9]{2,3})")
area_ids <- as.integer(unique(split_tseg[,2]))
area_tsegs <- area_vsegs <-vector("list", length = length(area_ids))

for(area in area_ids){
  area_tsegs[[area]] <- sort(as.integer(split_tseg[split_tseg[,2] == area,3])-10)
  area_vsegs[[area]] <- sort(as.integer(split_vseg[split_vseg[,2] == area,3])-10)
}

# Create set of true labels and catalogs of las and sfm-files
true_labels<- create_true_labels()

# Traverse input dir and ensure output directories exists
areas      <- dir(source_dir)
areas      <- areas[!areas %in% c("_laserdata", "_ytmodell", "_other")]
area_idx   <- 0
area_segs  <- vector("list", length(areas))
names(area_segs) <- areas

if(!dir.exists(output_dir)) {dir.create(output_dir)}

for(area in areas){
  area_idx    <- area_idx + 1
  area_init   <- Sys.time()
  curr_source <- paste0(source_dir, area, "/")
  curr_output <- paste0(output_dir, area, "/")
  if(!dir.exists(curr_output)) {dir.create(curr_output)}
  
  # Read Omap-png and create grid for lookup from that
  premade_omap<- dir(curr_source, ".Rdata", full.names = TRUE)
  
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
  
  omap_grid  <- map_grid_maker(omap, seg_size = seg_size)
  test_grid  <- omap_grid[rownum %in% area_tsegs[[area_idx]]]
  valid_grid <- omap_grid[rownum %in% area_vsegs[[area_idx]]]
  
  omap[, set_id := "Training"]
  
  for(trow in 1:nrow(test_grid)){
    omap[X %between% c(test_grid[trow,xmin],test_grid[trow,xmax])  &
           Y %between% c(test_grid[trow,ymin],test_grid[trow,ymax]), set_id := "Test"]  
  }
  for(vrow in 1:nrow(valid_grid)){
    omap[X %between% c(valid_grid[vrow,xmin],valid_grid[vrow,xmax])  &
           Y %between% c(valid_grid[vrow,ymin],valid_grid[vrow,ymax]), set_id := "Validation"]  
  }
  #table(omap$set_id)
  #test_segs  <- .dt_subset(test_grid, omap)
  #valid_segs <- .dt_subset(valid_grid, omap)
  
  fills <- setNames(viridisLite::viridis(3), c("Training", "Validation", "Test"))
  omap[, set_colour := fills[set_id]]
  save(omap, file = paste0(area, "_omap.Rdata"))
  # save map the map to file and copy world file
  map_dt_plot(omap, colour = "set_colour", dirname = paste0(curr_output,"split"))
  #file.copy(from = dir(path = curr_source, pattern = ".pgw$", full.names = TRUE), 
  #          to = paste0(curr_output, area, "_cat_colour_map.pgw"))
  
  #area_segs[[area]] <- list("tg" = test_grid, "ts" = test_segs, 
  #                          "vg" = valid_grid, "vs" = valid_segs)
  
}

area_stats <- rbind(area_stats, t(colSums(area_stats)))
area_stats[, area := c(areas, "total")]

write(paste0(length(areas), " areas finished in ", 
             round(difftime(Sys.time(), init_time_0, units = "mins"),1), " min."),
      stdout())

write.csv(area_stats, file="omap_stats.csv")
