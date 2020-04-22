# Routine for comparing gridded point predictions to omaps
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-p", "--pred_dir"), type="character", default="~/master_thesis_code/pvcnn/data/terrain/predict/", 
              help="Directory where predictions are located in subfolders [default %default]",
              dest = "pred_dir"),
  make_option(c("-m", "--omap_dir"), type="character", default="~/master_thesis_code/omap_cleaned/", 
              help="Directory where omaps are located in subfolders [default %default]",
              dest = "omap_dir"),
  make_option(c("-s", "--seg_size"), type = "integer", default = 120,
              help = "Size of chunks to be processed and represent sub area [default %default]",
              dest = "seg_size"),
    make_option(c("-e", "--entropy_limit"), type = "double", default = 1.2,
              help = "Maximum entropy for points to be considered in grid classification [default %default]",
              dest = "entropy_limit")
)

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

suppressPackageStartupMessages(library("RcppCNPy"))
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("patchwork"))
na_colour <- "grey70"
theme_set(theme_void(base_family = "serif") + 
            theme(panel.background = element_rect(fill = na_colour),
                  legend.position = "bottom", legend.title = element_blank()))
no_xexpand <- scale_x_continuous(expand = c(0,0)) 
no_yexpand <- scale_y_continuous(expand = c(0,0)) 
source("png_map_reader.R")
source("map_grid_maker.R")
source("little_helpers.R")
source("true_colour_codes.R")

true_labels <- create_true_labels()
round_mult  <- 100 
entropy_limit <- opts$entropy_limit

# Get all omap data into list for late comparison
omaps <- dir(opts$omap_dir, full.names = TRUE)
area_omaps <- vector("list", length = length(omaps))

for(oid in seq_along(omaps)){
  premade_omap<- dir(omaps[oid], ".Rdata", full.names = TRUE)
  
  if(length(premade_omap) == 0){
    mapname     = dir(omaps[oid],".png$", full.names = TRUE)
    omap        = png_map_reader(mapfile = mapname, 
                                 true_categories = true_labels)
  } else {
    load(premade_omap[1])
    # multiplying and casting as int to get rid of rounding errors in join
    omap[, X := as.integer(X*round_mult)][, Y := as.integer(Y*round_mult)][, category := factor(category, levels = true_labels$ID)]
    setkey(omap, X, Y)
  }

  omap_grid   = map_grid_maker(omap, seg_size = opts$seg_size*round_mult)
  resolution  = as.numeric(readLines(dir(omaps[oid], "*.pgw", full.names = TRUE)))[1]*round_mult
  area_omaps[[oid]] <- list(map = omap, grid = omap_grid, resolution = resolution)
}

pred_areas <- dir(opts$pred_dir, full.names = TRUE)
pred_areas <- pred_areas[!(grepl(x = pred_areas, pattern =  "\\."))]
seg_count  <- sum(sapply(pred_areas, function(x) length(dir(x))))
seg_no     <- 0
conf_mats  <- unpredicted <- vector("list", seg_count)
grid_counts<- vector("integer", seg_count)

for(area in pred_areas){
  curr_area_id  <- str_match(area, "Area_[0-9]{1,2}")
  map_name   <- area_names[area_id == curr_area_id, area]
  write(paste("Processing", map_name),"")
  segs       <- dir(area, full.names = TRUE)
  if(length(segs) == 0) { write("Area empty, skipping!\n",""); next } # skip empty folders

  for(seg in segs){
    seg_no       <- seg_no + 1
    # Get area and segment that predicted segment comes from
    # Will ofc only work for test/validation sets or eqv naming
    split_seg    <- str_match(string = seg, pattern = "Area\\_([0-9]{1,2})\\_segment\\_([0-9]{2,3})")
    orig_area_id <- as.integer(unique(split_seg[,2]))
    orig_seg_id  <- as.integer(unique(split_seg[,3])) - 10 # Old crap comes back to bite you...
    orig_area_name<- area_names[area_id == paste0("Area_", orig_area_id), area]
    write(paste("Processing", orig_area_name, "segment", orig_seg_id),"")
    
    seg_bounds   <- area_omaps[[orig_area_id]]$grid[rownum == orig_seg_id,]
    seg_omap     <- subset_omap(area_omaps, orig_area_id, seg_bounds)
    seg_res      <- area_omaps[[orig_area_id]]$resolution
    seg_xmin     <- min(seg_omap$X)
    seg_ymin     <- min(seg_omap$Y)
    seg_xmax     <- max(seg_omap$X)
    seg_ymax     <- max(seg_omap$Y)
    
    # Load segment data, shift data by half seg_res to 
    seg_data     <- as.data.table(npyLoad(paste0(seg,"/xyzrgb.npy"))[,1:2])*round_mult 
    setnames(seg_data, c("X", "Y"))
    seg_data[, c("prediction", "entropy") := .(factor(npyLoad(paste0(seg,"/preds.npy")), levels = true_labels$ID), 
                                               npyLoad(paste0(seg,"/entropy.npy")))]
    
    seg_data <- seg_data[X %between% c(seg_xmin,seg_xmax)  & Y %between% c(seg_ymin,seg_ymax), ]
    # Create grid columns 
    seg_data[, Xpix := as.integer( (((X-seg_xmin) %/% seg_res) * seg_res + seg_xmin))]
    seg_data[, Ypix := as.integer( (((Y-seg_ymin) %/% seg_res) * seg_res + seg_ymin))]
    
    seg_grouped  <- seg_data[, .(pred_cat = filtered_mode(prediction, entropy, entropy_limit)), 
                                 #mean_entropy = filtered_mean(entropy, entropy_limit), 
                                 #log_dens = log(.N/(2.54^2))), 
                             by =c("Xpix", "Ypix") ]
    setkey(seg_grouped, Xpix, Ypix)

    eval_data    <- seg_grouped[seg_omap]
    
    grid_counts[seg_no]   <- nrow(eval_data)
    unpredicted[[seg_no]] <- eval_data[is.na(pred_cat), .(unpredicted = .N), by = category ]
    conf_mats[[seg_no]]   <- caret::confusionMatrix(data = eval_data$pred_cat, 
                                                  reference = eval_data$category)$table

    if(runif(1)>1.05){
      print(
        ggplot(seg_omap, aes(x=X, y=Y, fill = cat_colour)) + 
          geom_tile(col="grey50")+
          scale_fill_manual(values=setNames(unique(seg_omap$cat_colour),
                                            unique(seg_omap$cat_colour)))  + 
          labs(title=paste("Ground truth",orig_area_name,"S",orig_seg_id)) + 
          no_xexpand + no_yexpand +
        ggplot(seg_grouped, aes(x=Xpix, y=Ypix, fill = as.character(pred_cat))) + 
          geom_tile(col = "grey50")  + 
          scale_fill_manual(values = setNames(as.character(true_labels$colour),
                                              true_labels$ID)) + 
          labs(title="Entropy weighted category predictions") +
          no_xexpand + no_yexpand +
        ggplot(seg_grouped, aes(x=Xpix, y=Ypix, fill = mean_entropy)) + 
          geom_tile(col = "grey50")  + 
          scale_fill_viridis_c(direction = -1, option = "D",na.value = "transparent" , 
                               breaks = seq(0,2,0.5), limits = c(0,2)) + 
          labs(title="Average entropy per grid unit") +
          no_xexpand + no_yexpand +
        ggplot(seg_grouped, aes(x=Xpix, y=Ypix, fill = log_dens)) + 
          geom_tile(col = "grey50")  + 
          scale_fill_viridis_c(direction = 1, option = "D", breaks = seq(-2,6,1), 
                               labels = round(exp(seq(-2,6,1)),1)) + 
          labs(title="Density, pts/m²") + 
          no_xexpand + no_yexpand 
      )
    }
  }
}

save(grid_counts, conf_mats, unpredicted, opts, file = paste0(opts$pred_dir, "grid_eval_stats.RData"))
load(paste0(opts$pred_dir, "grid_eval_stats.RData"))
conf_mat_total <- Reduce(`+`, conf_mats)
classacc <- diag(conf_mat_total)/colSums(conf_mat_total)
classiou <- diag(conf_mat_total)/(colSums(conf_mat_total) + rowSums(conf_mat_total) - diag(conf_mat_total))

pred_rate <- sapply(1:seg_count, function(seg){sum(conf_mats[[seg]])/grid_counts[seg]})


ggplot(data.table(pred_rate=pred_rate, seg = 1:length(pred_rate)), aes(x=pred_rate)) +
  geom_density(stat = stat_count(),col = "grey50", fill = "mediumorchid2") + theme_minimal()


accuracy  <- data.table(t(sapply(1:seg_count, function(seg){diag(conf_mats[[seg]])/colSums(conf_mats[[seg]])})))
iou       <- data.table(t(sapply(1:seg_count, function(seg){diag(conf_mats[[seg]])/(colSums(conf_mats[[seg]])+rowSums(conf_mats[[seg]])-diag(conf_mats[[seg]]))})))
setnames(accuracy, true_labels$category)
setnames(iou, true_labels$category)


library(ggridges)
library(patchwork)

aaa<-ggplot(melt(accuracy), aes(x=value, y = variable, fill = variable)) + 
  ggridges::geom_density_ridges2(scale = .85, col = "grey50") +  theme_minimal() +
  scale_x_continuous("Accuracy",limits=c(0,1)) + 
  scale_fill_manual(values = setNames(true_labels$colour, true_labels$category)) +
  theme(legend.position = "none") + ylab("") 
bbb<-ggplot(melt(iou), aes(x=value, y = variable, fill = variable)) + 
  ggridges::geom_density_ridges2(scale = .85, col = "grey50") +  theme_minimal() +
  scale_x_continuous("IoU",limits=c(0,1)) + 
  scale_fill_manual(values = setNames(true_labels$colour, true_labels$category)) +
  theme(legend.position = "none") + ylab("")
  
  aaa/bbb
