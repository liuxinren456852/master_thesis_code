# Routine for comparing gridded point predictions to omaps
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-p", "--pred_dir"), type="character", default="~/master_thesis_code/pvcnn/data/terrain/h5_nosplit/", 
              help="Directory where predictions are located in subfolders [default %default]",
              dest = "pred_dir"),
  make_option(c("-m", "--omap_dir"), type="character", default="~/master_thesis_code/omap_cleaned/", 
              help="Directory where omaps are located in subfolders [default %default]",
              dest = "omap_dir"),
  make_option(c("-w", "--width"), type = "double", default = c(0.25,0.5,1),
              help = "Width multiplier whose predictions should be evaluated [default 0.25, 0.5, 1]",
              dest = "width"),
  make_option(c("-e", "--entropy_limit"), type = "double", default = c(0.8,1.0,1.2,1.4,1.6),
              help = "Maximum entropy for points to be considered in grid classification [default 0.8 - 1.6 in steps of 0.2]",
              dest = "entropy_limit"),
  make_option(c("-a", "--area"), type = "character", default = NULL,
              help = "Area for which to do predictions [default %default]",
              dest = "area"),
  make_option(c("-n", "--new_area"), type = "logical", default = FALSE,
              help = "Is this a completely new area for which no ground truth exists [default %default]",
              dest = "new_area")
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
setDTthreads(threads = 3)

# Get all omap data into list for late comparison
omaps <- dir(opts$omap_dir, full.names = TRUE)

area_omaps <- vector("list", length = length(omaps))
write("Loading omaps","")
omap_pb <- txtProgressBar(0,length(area_omaps),width = 10,style = 3)
if(!opts$new_area){
  # Handle predictions for areas used in training

  for(oid in seq_along(omaps)){
    premade_omap <- dir(omaps[oid], ".Rdata", full.names = TRUE)
    
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
    
    resolution  = as.numeric(readLines(dir(omaps[oid], "*.pgw", full.names = TRUE)))[1]*round_mult
    area_omaps[[oid]] <- list(map = omap[, .(X,Y,category)], resolution = resolution)
    setTxtProgressBar(omap_pb,oid)
  }
  close(omap_pb)
} else {
  for(oid in seq_along(omaps)){
    premade_omap <- dir(omaps[oid], ".Rdata", full.names = TRUE)
    load(premade_omap[1])
    area_omaps[[oid]] <- pred_omap_data
    area_omaps[[oid]]$map[, X := as.integer(X*round_mult)][, Y := as.integer(Y*round_mult)]
    area_omaps[[oid]]$resolution <- area_omaps[[oid]]$resolution * round_mult
    setTxtProgressBar(omap_pb,oid)
  }
  
}
if(is.null(opts$area)){
  pred_areas <- dir(opts$pred_dir, full.names = TRUE)
  pred_areas <- pred_areas[(grepl("Area\\_[0-9]{1,2}", pred_areas))]
} else {
  pred_areas <- paste0(opts$pred_dir, opts$area)
}

seg_count  <- sum(sapply(pred_areas, function(x) length(dir(x))))

for(area in pred_areas){
  curr_area_id<- str_match(area, "Area_[0-9]{1,2}")
  area_id    <- as.integer(str_match(curr_area_id, "[0-9]{1,2}"))
  area_res   <- area_omaps[[area_id]]$resolution
  area_omap  <- area_omaps[[area_id]]$map
  area_xmin  <- min(area_omap$X)
  area_ymin  <- min(area_omap$Y)
  area_xmax  <- max(area_omap$X)
  area_ymax  <- max(area_omap$Y)
  
  save_dir   <- paste0(opts$pred_dir,curr_area_id, "/eval/")
  if(!dir.exists(save_dir)){ dir.create(save_dir) }
  map_name   <- area_names[area_id == curr_area_id, area]
  segs       <- dir(area, full.names = TRUE)
  segs       <- segs[grepl("segment\\_[0-9]{1,3}", segs)]
  if(length(segs) == 0) { write("\nArea empty, skipping!",""); next } # skip empty folders
  raw_points <- vector("list", length(segs))
  for(width in opts$width){
    prefix    <- paste0("c",gsub("\\.","p",width))
    
    # Load all points predicted and put them in common dt
    write(paste("Loading predictions for",map_name,", width:", width ),"")
    segm_pb   <- txtProgressBar(min=0,max=length(segs), width = 40, style = 3)
    seg_no    <- 0
    for(seg in segs){
      seg_data  <- as.data.table(npyLoad(paste0(seg,"/xyzrgb.npy"))[,1:2])*round_mult 
      setnames(seg_data, c("X", "Y"))
      seg_data[, c("prediction", "entropy") := .(factor(npyLoad(paste0(seg,"/",prefix,"_w1preds.npy")), 
                                                        levels = true_labels$ID), 
                                                 npyLoad(paste0(seg,"/",prefix,"_w1entropy.npy")))]
      raw_points[[seg]] <- seg_data
      seg_no    <- seg_no +1
      setTxtProgressBar(segm_pb, seg_no)
    }
    close(segm_pb)
    points        <- rbindlist(raw_points)
  
    # Filter points outside omap and create grid columns 
    points        <- points[X %between% c(area_xmin,area_xmax)  & Y %between% c(area_ymin,area_ymax), ]
    points[, Xpix := as.integer( (((X-area_xmin) %/% area_res) * area_res + area_xmin))]
    points[, Ypix := as.integer( (((Y-area_ymin) %/% area_res) * area_res + area_ymin))]
    setkey(points, Xpix, Ypix)
    
    # Grid predictions based on max entropy
    write(paste("Calculating predictions by entropy limit:"),"")
    ent_pb    <- txtProgressBar(0, length(opts$entropy_limit), style = 3, width = 10)
    group_pts <- vector("list", length(opts$entropy_limit))
    for(ent_id in seq_along(opts$entropy_limit)){
      entropy_limit <- opts$entropy_limit[ent_id]
      ent_pfx       <- gsub("\\.", "p", entropy_limit)
      ent_points    <- points[entropy <= entropy_limit, ]
      group_pts[[ent_id]]  <- ent_points[, .(pred_cat = weighed_mode(prediction, entropy)), 
                                         by =c("Xpix", "Ypix") ]
      setkey(group_pts[[ent_id]], Xpix, Ypix)
      
      mapplot <- ggplot(group_pts[[ent_id]], aes(x=Xpix, y=Ypix, fill = pred_cat)) +
        geom_tile()  +
        scale_fill_manual(
          values = setNames(as.character(true_labels$colour),
                            true_labels$ID),
          label = setNames(as.character(true_labels$category),
                           true_labels$ID),aesthetics =c("colour", "fill")
        ) +
        no_xexpand + no_yexpand 
      ggsave(filename = paste0(save_dir,prefix, "_e",ent_pfx,"_predicts.pdf"), plot = mapplot,
             units = "in")
      
      setnames(group_pts[[ent_id]], "pred_cat", paste0("pred_cat", ent_pfx))
      setTxtProgressBar(ent_pb, ent_id)
    }
    
    area_grouped  <- Reduce(function(x,y) x[y, all=TRUE], group_pts)
    group_ent     <- points[, .(mean_entropy = mean(entropy)), by =c("Xpix", "Ypix")]
    setkey(group_ent, Xpix, Ypix)
    area_grouped  <- area_grouped[group_ent, all=TRUE]
    area_grouped  <- area_grouped[area_omap, all=TRUE]
    save(area_grouped, file = paste0(save_dir,prefix,".Rdata"))
    
    entplot <- ggplot(group_ent, aes(x=Xpix, y=Ypix, fill = mean_entropy)) +
      geom_tile()  +
      scale_fill_viridis_c(
        direction = -1,
        option = "D",
        na.value = "transparent" ,
        breaks = seq(0, 2, 0.5),
        limits = c(0, 2), aesthetics =c("colour", "fill")
      )  +
      no_xexpand + no_yexpand 
    ggsave(filename = paste0(save_dir,prefix, "_e",ent_pfx,"_entropy.pdf"), plot = entplot,
            units = "in")
  }  
}

