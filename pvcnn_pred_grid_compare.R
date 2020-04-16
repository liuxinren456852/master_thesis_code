# Routine for comparing gridded point predictions to omaps
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-p", "--pred_dir"), type="character", default="~/master_thesis_code/pvcnn/data/terrain/predict/", 
              help="Directory where predictions are located in subfolders [default %default]",
              dest = "pred_dir"),
  make_option(c("-m", "--omap_dir"), type="character", default="~/master_thesis_code/omap_corrected/", 
              help="Directory where omaps are located in subfolders [default %default]",
              dest = "omap_dir"),
  make_option(c("-s", "--seg_size"), type = "integer", default = 120,
              help = "Size of chunks to be processed and represent sub area [default %default]",
              dest = "seg_size")
)

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

suppressPackageStartupMessages(library("RcppCNPy"))
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("patchwork"))

source("png_map_reader.R")
source("map_grid_maker.R")
source("little_helpers.R")
source("true_colour_codes.R")

true_labels <- create_true_labels()

# Get all omap data into list for late comparison
omaps <- dir(opts$omap_dir, full.names = TRUE)
area_omaps <- vector("list", length = length(omaps))

for(oid in seq_along(omaps)){
  mapname     = dir(omaps[oid],".png$", full.names = TRUE)
  omap        = png_map_reader(mapfile = mapname, 
                                 true_categories = true_labels)
  omap_grid   = map_grid_maker(omap, seg_size = opts$seg_size)
  resolution  = as.numeric(readLines(dir(omaps[oid], "*.pgw", full.names = TRUE)))[1]
  area_omaps[[oid]] <- list(map = omap, grid = omap_grid, resolution = resolution)
}

pred_areas <- dir(opts$pred_dir, full.names = TRUE)
pred_areas <- areas[!(grepl(x = pred_areas, pattern =  "\\."))]
nareas     <- length(pred_areas)
ent_thres <- 1.0
for(area in pred_areas){
  area_name  <- str_match(area, "Area_[0-9]{1,2}")
  map_name   <- 
  write(paste("Processing", area_name),"")
  segs       <- dir(area, full.names = TRUE)
  if(length(segs) == 0) { write("Area empty, skipping!\n",""); next } # skip empty folders

  for(seg in segs[sample(1:length(segs),5)]){
    # Get area and segment that predicted segment comes from
    # Will ofc only work for test/validation sets or eqv naming
    split_seg    <- str_match(string = seg, pattern = "Area\\_([0-9]{1,2})\\_segment\\_([0-9]{2,3})")
    orig_area_id <- as.integer(unique(split_seg[,2]))
    orig_seg_id  <- as.integer(unique(split_seg[,3])) - 10 # Old crap comes back to bite you...
    write(paste("Processing area ", orig_area_id, "segment", orig_seg_id),"")
    
    seg_bounds   <- area_omaps[[orig_area_id]]$grid[rownum == orig_seg_id,]
    seg_xmin     <- seg_bounds$xmin
    seg_ymin     <- seg_bounds$ymin
    seg_res      <- area_omaps[[orig_area_id]]$resolution
    
    # Lad segment data
    seg_data     <- as.data.table(npyLoad(paste0(seg,"/xyzrgb.npy"))[,1:2])
    setnames(seg_data, c("X", "Y"))
    seg_data[, c("prediction", "entropy") := .(npyLoad(paste0(seg,"/preds.npy")), 
                                               npyLoad(paste0(seg,"/entropy.npy")))]
    # Create grid columns 
    seg_data[, Xpix := (((X-seg_xmin) %/% seg_res) * seg_res + seg_xmin)]
    seg_data[, Ypix := (((Y-seg_ymin) %/% seg_res) * seg_res + seg_ymin)]
    seg_grouped  <- seg_data[, .(filtered_mode(prediction, entropy, 1.2), filtered_mean(entropy, 1.2), log(.N/(2.54^2))), by =c("Xpix", "Ypix") ]
    
    seg_omap     <- subset_omap(area_omaps, orig_area_id, seg_bounds)
    
    print(
    ggplot(seg_omap, aes(x=X, y=Y, fill = cat_colour)) + geom_tile(col="grey50")+scale_fill_manual(values=setNames(unique(seg_omap$cat_colour),unique(seg_omap$cat_colour)))  + labs(title=paste("Ground truth A",orig_area_id,"S",orig_seg_id)) +
    ggplot(seg_grouped, aes(x=Xpix, y=Ypix, fill = as.character(V1))) + geom_tile(col = "grey50")  + scale_fill_manual(values = setNames(as.character(true_labels$colour),true_labels$ID)) + labs(title="Entropy weighted category predictions") +
    ggplot(seg_grouped, aes(x=Xpix, y=Ypix, fill = V2)) + geom_tile(col = "grey50")  + scale_fill_viridis_c(direction = 1, option = "D") + labs(title="Average entropy per grid unit") +
    ggplot(seg_grouped, aes(x=Xpix, y=Ypix, fill = V3)) + geom_tile(col = "grey50")  + scale_fill_viridis_c(direction = 1, option = "D") + labs(title="Density, pts/m²")
    )
  }
}

