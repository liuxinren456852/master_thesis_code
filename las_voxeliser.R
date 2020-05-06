# Routine for voxelising las-files and running multiple logistic regression on them
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-l", "--las_source"), type = "character", default="las_output_notest/", 
              help="Directory where source las files are located as subfolders [default %default]",
              dest = "las_source"),
  make_option(c("-m", "--omap_dir"), type="character", default="~/master_thesis_code/omap_cleaned/", 
              help="Directory where omaps are located in subfolders [default %default]",
              dest = "omap_dir"),
  make_option(c("-s", "--seg_size"), type = "integer", default = 120,
              help = "Size of chunks to be processed and represent sub area [default %default]",
              dest = "seg_size"),
  make_option(c("-t", "--train_prop"), type = "double", default = 0.45,
              help = "Proportion of grid units used to train regression [default %default]",
              dest = "train_prop"),
  make_option(c("-v", "--valid_prop"), type = "double", default = 0.15,
              help = "Proportion of grid units used to train regression [default %default]",
              dest = "valid_prop"),
  make_option(c("-e", "--entropy_limit"), type = "double", default = 1.2,
              help = "Maximum entropy for points to be considered in grid classification [default %default]",
              dest = "entropy_limit"),
  make_option(c("-r", "--rand_seed"), type = "integer", default = 831117,
              help = "Seed to be used [default %default]",
              dest = "rand_seed")
)

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

suppressPackageStartupMessages(library("RcppCNPy"))
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("lidR"))
suppressPackageStartupMessages(library(nnet))

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

areas  <- dir(opts$las_source, full.names = TRUE)
eval_data <- vector("list", length(areas))

for(area in areas){
  curr_area   <- str_match(area, "Area_([0-9]{1,2})")
  curr_name   <- area_names[area_id == curr_area[1], area]
  curr_idx    <- as.integer(curr_area[2])
  write(paste("Processing", curr_name),"")
  bounds      <- area_omaps[[curr_idx]]$grid[, .(xmin = min(xmin), ymin = min(ymin), 
                                                 xmax = max(xmax), ymax = max(ymax))]
  area_res    <- area_omaps[[curr_idx]]$resolution
  
  las <- readLAS(area)
  las@data[, `:=`(X = X*round_mult, Y = Y*round_mult)]
  las@data <- las@data[X %between% c(bounds$xmin,bounds$xmax)  & Y %between% c(bounds$ymin,bounds$ymax), ]
  # Create grid columns 
  las@data[, Xpix := as.integer( (((X-bounds$xmin) %/% area_res) * area_res + bounds$xmin))]
  las@data[, Ypix := as.integer( (((Y-bounds$ymin) %/% area_res) * area_res + bounds$ymin))]
  
  las_grouped  <- las@data[, .(log_pt_dens   = log(.N/(area_res/round_mult)^2),
                               mean_z        = mean(Z-min(Z)),
                               var_z         = ifelse(is.na(var(Z)),0,var(Z)),
                               mean_ndvi     = mean((R-G)/(R+G)),
                               r             = mean(R),
                               g             = mean(G), 
                               b             = mean(B),
                               intensity     = mean(Intensity),
                               var_intensity = ifelse(is.na(var(Intensity)),0,var(Intensity))
                               ), by =c("Xpix", "Ypix") ]
  setkey(las_grouped, Xpix, Ypix)
  
  eval_data[[curr_idx]]<- las_grouped[area_omaps[[curr_idx]]$map[, .(X, Y, category)]][!is.na(log_pt_dens), ]
}

mlr_data <- rbindlist(eval_data)
set.seed(opts$rand_seed)
ngrids    <- nrow(mlr_data)
valid_obs <- sample(x = 1:ngrids, size = floor(ngrids*opts$valid_prop), replace = FALSE)
mlr_train <- mlr_data[setdiff(1:ngrids, valid_obs), -(1:2)]
mlr_valid <- mlr_data[valid_obs[1:floor(length(valid_obs)/2)], -(1:2)] 

mlr <- multinom(category ~ ., data = mlr_train)
valid_prob <- predict(mlr, newdata = mlr_valid[!is.na(log_pt_dens), -("category")], type = "probs")
mlr_valid[,`:=` (ents = apply(valid_prob, 1, function(x) sum(x*log(x))),
                 pred = factor(apply(valid_prob, 1, which.max)-1, levels = true_labels$ID))]

save(mlr_train, mlr_valid, mlr, opts, file = "mlr_data.Rdata")


valid_m <- caret::confusionMatrix(mlr_valid[ents < opts$entropy_limit, pred], 
                                  mlr_valid[ents < opts$entropy_limit ,category])$table
confusion_matrix_xtable("multinomial regression","validation samples",conf_mat = valid_m)
