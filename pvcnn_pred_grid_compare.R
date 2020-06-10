# Routine for comparing gridded point predictions to omaps
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-p", "--pred_dir"), type="character", default="~/master_thesis_code/pvcnn/data/terrain/h5_output", 
              help="Directory where point predictions are located in subfolders [default %default]",
              dest = "pred_dir"),
  make_option(c("-m", "--omap_dir"), type="character", default="~/master_thesis_code/omap_cleaned/", 
              help="Directory where omaps are located in subfolders [default %default]",
              dest = "omap_dir"),
  make_option(c("-s", "--seg_size"), type = "integer", default = 120,
              help = "Size of chunks to be processed and represent sub area [default %default]",
              dest = "seg_size"),
  make_option(c("-w", "--width"), type = "double", default = 0.5,
              help = "Width multiplier whose predictions should be evaluated [default %default]",
              dest = "width"),
  make_option(c("-e", "--entropy_limit"), type = "double", default = 1.2,
              help = "Maximum entropy for points to be considered in grid classification [default %default]",
              dest = "entropy_limit"),
  make_option(c("-a", "--area"), type = "character", default = NULL,
              help = "Area for which to do predictions [default %default]",
              dest = "area")
)

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

suppressPackageStartupMessages(library("RcppCNPy"))
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("patchwork"))
na_colour <- "grey70"
theme_set(theme_grey()+  
            theme(panel.background = element_rect(fill = na_colour),
                  panel.border = element_rect(fill = "transparent", colour = "black", size = 1.5),
                  legend.position = "none", legend.title = element_blank(),
                  axis.title = element_blank(), axis.text = element_blank(),
                  axis.ticks = element_blank(), panel.grid = element_blank()))
no_xexpand <- scale_x_continuous(expand = c(0,0)) 
no_yexpand <- scale_y_continuous(expand = c(0,0)) 
source("png_map_reader.R")
source("map_grid_maker.R")
source("little_helpers.R")
source("true_colour_codes.R")
load("hdpintervals.Rdata")
true_labels <- create_true_labels()
round_mult  <- 100 


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

if(is.null(opts$area)){
  pred_areas <- dir(opts$pred_dir, full.names = TRUE)
  pred_areas <- pred_areas[(grepl("Area\\_[0-9]{1,2}", pred_areas))]
} else {
  pred_areas <- paste0(opts$pred_dir, opts$area)
}
seg_count  <- sum(sapply(pred_areas, function(x) length(dir(x))))
for(width in opts$width){
  prefix <- paste0("c",gsub("\\.","p",width))
  for(ent_lim in opts$entropy_limit){
    entropy_limit <- ent_lim
    write(paste0("Processing width ", prefix, " entropy limit ", ent_lim),"")
    
    seg_no     <- 0
    conf_mats  <- unpredicted <- eval_list <- vector("list", seg_count)
    grid_counts<- vector("integer", seg_count)
    # if(!dir.exists(paste0(opts$pred_dir, "/plots_", prefix,"_el",gsub("\\.","p",entropy_limit)))){
    #   dir.create(paste0(opts$pred_dir, "/plots_", prefix,"_el",gsub("\\.","p",entropy_limit)))
    # }
    
    for(area in pred_areas){
      curr_area_id  <- str_match(area, "Area_[0-9]{1,2}")
      save_dir <- paste0(opts$pred_dir,curr_area_id, "/eval/")
      if(!dir.exists(save_dir)){ dir.create(save_dir) }
      map_name   <- area_names[area_id == curr_area_id, area]
      write(paste("Processing", map_name),"")
      segs       <- dir(area, full.names = TRUE)
      segs       <- segs[grepl("segment\\_[0-9]{1,3}", segs)]
      if(length(segs) == 0) { write("Area empty, skipping!",""); next } # skip empty folders
      pb <- txtProgressBar(min=0,max=length(segs), width = 40, style = 3)
      for(seg in segs){
        seg_no       <- seg_no + 1
      
        split_seg    <- str_match(string = seg, pattern = "(Area\\_)?([0-9]{1,2})?(\\_)?(segment\\_)([0-9]{2,3})")
        orig_area_id <- as.integer(unique(split_seg[,3]))
        orig_seg_id  <- as.integer(unique(split_seg[,6])) - 10 # Old crap comes back to bite you...
        if(is.na(orig_area_id)){ orig_area_id <- as.integer(str_extract(string = curr_area_id, pattern = "([0-9]{1,2})"))}
        orig_area_name<- area_names[area_id == paste0("Area_", orig_area_id), area]
        #write(paste("Processing", orig_area_name, "segment", orig_seg_id),"")
        
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
        seg_data[, c("prediction", "entropy") := .(factor(npyLoad(paste0(seg,"/",prefix,"_w2preds.npy")), levels = true_labels$ID), 
                                                   npyLoad(paste0(seg,"/",prefix,"_w2entropy.npy")))]
        
        seg_data <- seg_data[X %between% c(seg_xmin,seg_xmax)  & Y %between% c(seg_ymin,seg_ymax), ]
        # Create grid columns 
        seg_data[, Xpix := as.integer( (((X-seg_xmin) %/% seg_res) * seg_res + seg_xmin))]
        seg_data[, Ypix := as.integer( (((Y-seg_ymin) %/% seg_res) * seg_res + seg_ymin))]
        
        seg_grouped  <- seg_data[, .(pred_cat = filtered_mode(prediction, entropy, entropy_limit), 
                                     f_mean_entropy = filtered_mean(entropy, entropy_limit), 
                                     mean_entropy = mean(entropy) 
                                     #min_entropy = min(entropy),
                                     #sd_entropy = sd(entropy),
                                     #log_dens = log(.N/(2.54^2))
                                     ), 
                                 by =c("Xpix", "Ypix") ]
        setkey(seg_grouped, Xpix, Ypix)
        
        eval_data    <- seg_grouped[seg_omap]
        
        
        eval_list[[seg_no]]  <- eval_data
        
        grid_counts[seg_no]   <- nrow(eval_data)
        unpredicted[[seg_no]] <- eval_data[is.na(pred_cat), .(unpredicted = .N), by = category ]
        conf_mats[[seg_no]]   <- caret::confusionMatrix(data = eval_data$pred_cat, 
                                                        reference = eval_data$category)$table
        iou <- mean(diag(conf_mats[[seg_no]])/(colSums(conf_mats[[seg_no]]) + rowSums(conf_mats[[seg_no]]) - diag(conf_mats[[seg_no]])), na.rm =TRUE)
        
        setTxtProgressBar(pb,seg_no)
        
        # area_plot<- ggplot(seg_omap, aes(x = X, y = Y, fill = cat_colour, colour = cat_colour)) +
        #   geom_tile() +
        #   scale_fill_manual(
        #     values = setNames(unique(seg_omap$cat_colour),
        #                       unique(seg_omap$cat_colour)),
        #     label = setNames(as.character(true_labels$category),
        #                      true_labels$colour), aesthetics =c("colour", "fill")
        #   )  +
        #   labs(title = paste("Ground truth")) +
        #   no_xexpand + no_yexpand +
        #   ggplot(seg_grouped, aes(
        #     x = Xpix,
        #     y = Ypix,
        #     fill = as.character(pred_cat)
        #   )) +
        #   geom_tile()  +
        #   scale_fill_manual(
        #     values = setNames(as.character(true_labels$colour),
        #                       true_labels$ID),
        #     label = setNames(as.character(true_labels$category),
        #                      true_labels$ID),aesthetics =c("colour", "fill")
        #   ) +
        #   labs(title = paste("Predictions, IoU:", round(iou, 3))) +
        #   no_xexpand + no_yexpand +
        #   ggplot(seg_grouped, aes(x = Xpix, y = Ypix, fill = mean_entropy)) +
        #   geom_tile()  +
        #   scale_fill_viridis_c(
        #     direction = -1,
        #     option = "D",
        #     na.value = "transparent" ,
        #     breaks = seq(0, 2, 0.5),
        #     limits = c(0, 2), aesthetics =c("colour", "fill")
        #   )  +
        #   labs(title = "Average entropy per grid unit") +
        #   no_xexpand + no_yexpand + theme(legend.position = "right")
        # 
        # ggsave(filename = paste0(opts$pred_dir, "/plots_", prefix,"_el",gsub("\\.","p",entropy_limit),"/", orig_area_name,"_S",orig_seg_id,".pdf"),
        #        plot = area_plot, width = 22, height = 7.5, units = "cm")
      }
    }
    eval_data <- rbindlist(eval_list)
    #save(eval_data, opts, file = paste0(save_dir ,prefix,"_el",gsub("\\.","p",entropy_limit), "_grid_eval_stats.RData"))
    save(grid_counts, conf_mats, unpredicted, eval_list, opts, file = paste0(save_dir,prefix,"_el",gsub("\\.","p",entropy_limit), "_grid_eval_stats.RData"))
    conf_mat_total <- Reduce(`+`, conf_mats)
    print(confusion_matrix_xtable(paste0("PVCNN++, ", prefix), "validation data grid units", conf_mat = conf_mat_total))
  }
}
