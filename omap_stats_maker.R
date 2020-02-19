suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-m", "--source_dir"), type="character", default="~/kartor/", 
              help="Directory where source maps are located as subfolders [default %default]",
              dest = "source_dir"),
  make_option(c("-o", "--output_dir"), type = "character", default = paste0(getwd(), "/omap_stats/"),
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

# Create set of true labels and catalogs of las and sfm-files
true_labels<- create_true_labels()

# Traverse input dir and ensure output directories exists
areas      <- dir(source_dir)
areas      <- areas[!areas %in% c("_laserdata", "_ytmodell", "_other")]
area_idx   <- 0
area_stats <- data.table::data.table(matrix(nrow = 0, ncol = nrow(true_labels)+4))
setnames(area_stats, c(true_labels$category, "xsize", "ysize", "size", "seg_count"))

if(!dir.exists(output_dir)) {dir.create(output_dir)}

for(area in areas){
  area_idx    <- area_idx + 1
  area_init   <- Sys.time()
  curr_source <- paste0(source_dir, area, "/")

  # Read Omap-png and create grid for lookup from that
  mapname    <- dir(curr_source,".png$") 
  omap       <- png_map_reader(mapfile = paste0(curr_source, mapname), 
                               true_categories = true_labels)
  omap_grid  <- map_grid_maker(omap, seg_size = seg_size)
  
  # save map the map to file
  map_dt_plot(omap, colour = "cat_colour", dirname = paste0(output_dir, area))
  
  # calc stats
  map_count  <- factor(omap$category, levels = true_labels$ID)
  levels(map_count) <- true_labels$category
  xsize      <- max(omap$X) - min(omap$X)
  ysize      <- max(omap$Y) - min(omap$Y)
  
  area_stats <- rbind(area_stats, 
                      t(c(table(map_count), 
                          xsize = xsize, 
                          ysize = ysize, 
                          size = xsize*ysize,
                          seg_count = nrow(omap_grid))))
}

area_stats <- rbind(area_stats, t(colSums(area_stats)))
area_stats[, area := c(areas, "total")]

write(paste0(length(areas), " areas finished in ", 
             round(difftime(Sys.time(), init_time_0, units = "mins"),1), " min."),
      stdout())
