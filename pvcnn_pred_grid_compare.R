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
  make_option(c("-w", "--width"), type = "double", default = c(1,0.5,0.25),
              help = "Width multiplier whose predictions should be evaluated [default %default]",
              dest = "width"),
    make_option(c("-e", "--entropy_limit"), type = "double", default = c(0.8,1.0,1.2,1.4,1.6),
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
theme_set(theme_grey()+  
            theme(panel.background = element_rect(fill = na_colour),
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


pred_areas <- dir(opts$pred_dir, full.names = TRUE)
pred_areas <- pred_areas[(grepl("Area\\_[0-9]{1,2}", pred_areas))]
seg_count  <- sum(sapply(pred_areas, function(x) length(dir(x))))
for(width in opts$width){
  prefix <- paste0("c",gsub("\\.","p",width))
  for(ent_lim in opts$entropy_limit){
    entropy_limit <- ent_lim
    write(paste0("Processing width ", prefix, " entropy limit ", ent_lim),"")

    seg_no     <- 0
conf_mats  <- unpredicted <- eval_list <- vector("list", seg_count)
grid_counts<- vector("integer", seg_count)
if(!dir.exists(paste0(opts$pred_dir, "/plots_", prefix,"_el",gsub("\\.","p",entropy_limit)))){
  dir.create(paste0(opts$pred_dir, "/plots_", prefix,"_el",gsub("\\.","p",entropy_limit)))
}
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
    seg_data[, c("prediction", "entropy") := .(factor(npyLoad(paste0(seg,"/",prefix,"preds.npy")), levels = true_labels$ID), 
                                               npyLoad(paste0(seg,"/",prefix,"entropy.npy")))]
    
    seg_data <- seg_data[X %between% c(seg_xmin,seg_xmax)  & Y %between% c(seg_ymin,seg_ymax), ]
    # Create grid columns 
    seg_data[, Xpix := as.integer( (((X-seg_xmin) %/% seg_res) * seg_res + seg_xmin))]
    seg_data[, Ypix := as.integer( (((Y-seg_ymin) %/% seg_res) * seg_res + seg_ymin))]
    
    seg_grouped  <- seg_data[, .(pred_cat = filtered_mode(prediction, entropy, entropy_limit), 
                                 f_mean_entropy = filtered_mean(entropy, entropy_limit), 
                                 mean_entropy = mean(entropy), 
                                 min_entropy = min(entropy),
                                 sd_entropy = sd(entropy),
                                 log_dens = log(.N/(2.54^2))), 
                             by =c("Xpix", "Ypix") ]
    setkey(seg_grouped, Xpix, Ypix)

    eval_data    <- seg_grouped[seg_omap]

    
    eval_list[[seg_no]]  <- eval_data
    
    grid_counts[seg_no]   <- nrow(eval_data)
    unpredicted[[seg_no]] <- eval_data[is.na(pred_cat), .(unpredicted = .N), by = category ]
    conf_mats[[seg_no]]   <- caret::confusionMatrix(data = eval_data$pred_cat, 
                                                  reference = eval_data$category)$table
    iou <- mean(diag(conf_mats[[seg_no]])/(colSums(conf_mats[[seg_no]]) + rowSums(conf_mats[[seg_no]]) - diag(conf_mats[[seg_no]])), na.rm =TRUE)
    
    
    
   
    
    area_plot<- ggplot(seg_omap, aes(x = X, y = Y, fill = cat_colour)) +
      geom_tile(col = "grey50") +
      scale_fill_manual(
        values = setNames(unique(seg_omap$cat_colour),
                          unique(seg_omap$cat_colour)),
        label = setNames(as.character(true_labels$category),
                         true_labels$colour)
      )  +
      labs(title = paste("Ground truth")) +
      no_xexpand + no_yexpand +
      ggplot(seg_grouped, aes(
        x = Xpix,
        y = Ypix,
        fill = as.character(pred_cat)
      )) +
      geom_tile(col = "grey50")  +
      scale_fill_manual(
        values = setNames(as.character(true_labels$colour),
                          true_labels$ID),
        label = setNames(as.character(true_labels$category),
                         true_labels$ID)
      ) +
      labs(title = paste("Predictions, IoU:", round(iou, 3))) +
      no_xexpand + no_yexpand +
      # ggplot(seg_grouped, aes(x=Xpix, y=Ypix, fill = as.character(pred_cat))) +
      #   geom_tile(col = "grey50", alpha = 1-seg_grouped$f_mean_entropy/entropy_limit)  +
      #   scale_fill_manual(values = setNames(as.character(true_labels$colour),
      #                                       true_labels$ID),
      #                     label= setNames(as.character(true_labels$category),
      #                                     true_labels$ID)) +
      #   labs(title=paste("Entropy weighted category predictions, IoU:", round(iou,3))) +
      #   no_xexpand + no_yexpand +
      #
      ggplot(seg_grouped, aes(x = Xpix, y = Ypix, fill = mean_entropy)) +
      geom_tile(col = "grey50")  +
      scale_fill_viridis_c(
        direction = -1,
        option = "D",
        na.value = "transparent" ,
        breaks = seq(0, 2, 0.5),
        limits = c(0, 2)
      )  +
      labs(title = "Average entropy per grid unit") +
      no_xexpand + no_yexpand + theme(legend.position = "right")
             
    ggsave(filename = paste0(opts$pred_dir, "/plots_", prefix,"_el",gsub("\\.","p",entropy_limit),"/", orig_area_name,"_S",orig_seg_id,".pdf"),
           plot = area_plot, width = 22, height = 7.5, units = "cm")
    
  }
}

save(grid_counts, conf_mats, unpredicted, eval_list, opts, file = paste0(opts$pred_dir,prefix,"_el",gsub("\\.","p",entropy_limit), "_grid_eval_stats.RData"))
}
}

###### Entropy Limits #####
all_stats <- data.table(matrix(nrow = 0, ncol = 7))
setnames(all_stats, c("width", "entropy", "pred_iou", "total_iou", "predict_rate", "pred_acc", "total_acc"))
for(width in opts$width){
  prefix <- paste0("c",gsub("\\.","p",width))
  for(ent_lim in  opts$entropy_limit){
    entropy_limit <- ent_lim
    ent_pfx<-paste0("el",gsub("\\.","p",entropy_limit))
    load(paste0(opts$pred_dir, prefix,"_",ent_pfx,"_grid_eval_stats.RData"))
    conf_mat_total <- Reduce(`+`, conf_mats)
    unpredicted_dt <- rbindlist(unpredicted)[order(category), .(unknowns = sum(unpredicted)), by = "category"]
    unpredicted_dt[, `:=`(predicted = colSums(conf_mat_total), category = true_labels$category[category], correct = diag(conf_mat_total),
                          pred_iou = diag(conf_mat_total)/(colSums(conf_mat_total)+rowSums(conf_mat_total)-diag(conf_mat_total)),
                          total_iou = diag(conf_mat_total)/(colSums(conf_mat_total)+rowSums(conf_mat_total)-diag(conf_mat_total)+unknowns))]

    totals <- data.table(category  = "Overall",                 unknowns  = sum(unpredicted_dt[, 2]), 
                         predicted = sum(unpredicted_dt[, 3]),  correct   = sum(unpredicted_dt[,4]),
                         pred_iou  = sum(unpredicted_dt[, 5])/nrow(unpredicted_dt), 
                         total_iou = sum(unpredicted_dt[, 6])/nrow(unpredicted_dt))
    unpredicted_dt <- rbind(unpredicted_dt, totals)
    unpredicted_dt[, `:=`(predict_rate = predicted/(unknowns+predicted), pred_acc = correct/predicted, total_acc = correct/(predicted+unknowns))]
    all_stats <- rbind(all_stats, data.table(width = prefix, entropy = ent_lim, unpredicted_dt[category == "Overall", 5:9]))
    print(paste0("PVCNN",gsub("p","",prefix),"prr <- c(",paste0(unpredicted_dt$predict_rate, collapse = ","),")"))
    confusion_matrix_xtable(paste0("PVCNN++, ", prefix), "validation data grid units", conf_mat = conf_mat_total)
  }
}
xtable(all_stats[, c(1,2,5,3,4,6,7)], digits = c(0,0,1,rep(3,5)))


###### Logistic regression ######
width <-0.5;  prefix <- paste0("c", gsub("\\.","p",width)); 
ent_lim <- 1.2; ent_pfx <- paste0("el",gsub("\\.","p",ent_lim));
load(paste0(opts$pred_dir, prefix,"_",ent_pfx,"_grid_eval_stats.RData"))




conf_mat_total <- Reduce(`+`, conf_mats)
# 
# unpredicted_dt <- rbindlist(unpredicted)[order(category), .(unknowns = sum(unpredicted)), by = "category"]
# unpredicted_dt[, `:=`(predicted = colSums(conf_mat_total), category = true_labels$category[category])]
# 
# totals <- data.table(category = "Total", unknowns = sum(unpredicted_dt[, 2]), predicted = sum(unpredicted_dt[, 3]))
# unpredicted_dt <- rbind(unpredicted_dt, totals)
# unpredicted_dt[, prediction_rate := (predicted/(predicted + unknowns))]
# print(paste0("PVCNN",gsub("p","",prefix),"prr <- c(",paste0(unpredicted_dt$prediction_rate, collapse = ","),")"))
# 
# confusion_matrix_xtable(paste0("PVCNN++, c=", opts$width), "validation data grid units", conf_mat = conf_mat_total)
# confusion_matrix_xtable(paste0("PVCNN++, c=", opts$width), "validation data points",
#                         cm_path = paste0("pvcnn/runs/terrain.pvcnn2.test_area.",prefix,"/best_area_16.conf_mat.npy"))
# 
# 

full_eval <- rbindlist(eval_list)[, dens := exp(log_dens)][, std_dens := (dens-mean(dens, na.rm =TRUE))/sd(dens, na.rm =TRUE), by = category][, hdp :="Low"]
  setkey(full_eval, Xpix, Ypix)
 #
 for(rowno in seq(1, nrow(intervalsbind))){
    bounds <- unlist(intervalsbind[rowno,])*round_mult
#    #print(bounds)
    full_eval[Xpix %between% bounds, hdp := "High"]
    full_eval[Ypix %between% bounds, hdp := "High"]
  }
# 
#  full_eval[, .(mean_ent= mean(mean_entropy, na.rm=TRUE), sd_ent = sd(mean_entropy, na.rm=TRUE), count = .N), by =pred_cat][order(pred_cat)]
# 
#  full_eval[,.(mean_dens =mean(dens, na.rm=TRUE), median_dens = median(dens, na.rm=TRUE), mean_ent= mean(mean_entropy, na.rm=TRUE)), by = category][order(-category)]
#  full_eval[,.(category = category, mean_dens =mean(dens, na.rm=TRUE), median_dens = median(dens, na.rm=TRUE), mean_ent= mean(mean_entropy, na.rm=TRUE), correct = mean(pred_cat==category, na.rm=TRUE)), by = .(hdp,category)][order(category)]
# full_eval[,.( median_dens = median(dens, na.rm=TRUE), mean_ent= mean(mean_entropy, na.rm=TRUE), correct = mean(pred_cat==category, na.rm=TRUE)), by = .(hdp,category)][order(category)]
aaa <- full_eval[, .(correct = mean(pred_cat == category, na.rm=TRUE), count =.N), by=.(hdp, category)]
full_eval[, correct:=factor(pred_cat==category, levels =c(FALSE, TRUE))]
# true_labels[, cat_fact:= factor(ID)]
full_eval[,hdp := factor(hdp,levels = c("Low", "High"))]
# full_eval <- full_eval[true_labels[,.(category, cat_fact)], on="category==cat_fact"]
eval_glm <- glm(correct ~category+hdp+hdp*category, data = full_eval, family=binomial(link = "logit"))
summary(eval_glm, signif.stars =FALSE) 

# exp(cbind(Odd_Ratio = coef(eval_glm), confint(eval_glm)))
# 
#eval_mat<- model.matrix(correct ~category+hdp+category*hdp, full_eval[!is.na(correct), .(hdp, category, correct)])
# fit <- glmnet(eval_mat[,-1], as.matrix(full_eval[!is.na(correct), correct]), family="binomial")
# 
# # effsize::cohen.d(full_eval$mean_entropy, full_eval$hdp, na.rm=TRUE)
# 
#   ggplot(full_eval, aes(x=dens, y=category, fill = hdp)) + ggridges::geom_density_ridges2() +theme_minimal() + scale_x_continuous(limits = c(0,5), breaks = seq(0,2,0.4)) + scale_y_discrete(labels=true_labels$category) + scale_fill_viridis_d()
# 
#   ggplot(full_eval, aes(x=mean_entropy, y=category, fill = hdp)) +
#     ggridges::geom_density_ridges2(alpha=0.4, col="grey30")+
#     theme_minimal() +
#     scale_x_continuous(limits = c(0,-log(1/8)), breaks = seq(0,2,0.4)) +
#     scale_y_discrete(labels=true_labels$category) +
#     scale_fill_viridis_d()
# 
  ggplot(aaa, aes(x=category, y=correct,group=hdp, fill = hdp, label = count, col=hdp)) +
    geom_col(position = position_dodge(.9),col="grey30", width = .8)+
    geom_text(aes(y = 0.07), position = position_dodge(.9), fontface ="bold",
               family ="serif", size = 2.85, show.legend=FALSE ) +
    theme_minimal(base_family="serif") +
    scale_x_discrete(labels=true_labels$category) +
    scale_y_continuous(breaks = seq(0,1,0.2), limits =c(0,1)) +
    scale_fill_viridis_d(begin = 1/6, "Point density") +
    scale_colour_viridis_d(begin = 1/6, "Point density",direction = -1)+
    labs(x="", y="",title = "Proportion of correctly predicted grid units by density and category",
         subtitle = paste0("c = ", width, ", entropy limit = ", ent_lim)) +
    theme(panel.grid.major.x = element_blank(), legend.position = "bottom")
  ggsave("correct_grid_prop.pdf", width = 22, height = 9.5, units = "cm")
  
  
# 
# 
# # # hist(full_eval[std_dens < 5, std_dens])
# # # 
# # # ggplot(full_eval[sample(1:nrow(full_eval),5000),], aes(x = std_dens, y = mean_entropy, col = category)) + geom_point() +theme_bw()
# # # 
# # # aa <- lm(mean_entropy ~ category + std_dens +0, data = full_eval)
# # # summary(aa)
# # # hist(resid(aa))
# # # 
# # pred_rate <- sapply(1:seg_count, function(seg){sum(conf_mats[[seg]])/grid_counts[seg]})
# # 
# # 
# # ggplot(data.table(pred_rate=pred_rate, seg = 1:length(pred_rate)), aes(x=pred_rate)) +
# #   geom_density(col = "grey50", fill = "mediumorchid2") + theme_minimal()
# # 
# # 
# # accuracy  <- data.table(t(sapply(1:seg_count, function(seg){diag(conf_mats[[seg]])/colSums(conf_mats[[seg]])})))
# # iou       <- data.table(t(sapply(1:seg_count, function(seg){diag(conf_mats[[seg]])/(colSums(conf_mats[[seg]])+rowSums(conf_mats[[seg]])-diag(conf_mats[[seg]]))})))
# # setnames(accuracy, true_labels$category)
# # setnames(iou, true_labels$category)
# # 
# # 
# # library(ggridges)
# # library(patchwork)
# # 
# # aaa<-ggplot(melt(accuracy), aes(x=value, y = variable, fill = variable)) + 
# #   ggridges::geom_density_ridges2(scale = .85, col = "grey50") +  theme_minimal() +
# #   scale_x_continuous("Accuracy",limits=c(0,1)) + 
# #   scale_fill_manual(values = setNames(true_labels$colour, true_labels$category)) +
# #   theme(legend.position = "none") + ylab("") 
# # bbb<-ggplot(melt(iou), aes(x=value, y = variable, fill = variable)) + 
# #   ggridges::geom_density_ridges2(scale = .85, col = "grey50") +  theme_minimal() +
# #   scale_x_continuous("IoU",limits=c(0,1)) + 
# #   scale_fill_manual(values = setNames(true_labels$colour, true_labels$category)) +
# #   theme(legend.position = "none") + ylab("")
# #   
# #   aaa/bbb
