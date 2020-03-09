suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(lidR))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(RcppCNPy))

option_list <- list( 
  make_option(c("-n", "--npy_source"), type = "character", default="area_output/", 
              help="Directory where source numpy files are located as subfolders [default %default]",
              dest = "npy_source"),
  make_option(c("-l", "--las_output"), type = "character", default="las_output/", 
              help="Directory where to export the las-files [default %default]",
              dest = "las_output"),
  make_option(c("-t", "--test_area"), type = "character", default="Area_15", 
              help="Area name for test area [default %default]",
              dest = "test_area"),
  make_option(c("-v", "--valid_area"), type = "character", default="Area_16", 
              help="Area name for test area [default %default]",
              dest = "valid_area")
)

source("true_colour_codes.R")

opt_parser <- OptionParser(option_list=option_list)
opts       <- parse_args(opt_parser)

if(!dir.exists(opts$las_output)) { dir.create(opts$las_output) }

rgb_max    <- 2^16
intens_max <- 255

true_labels<- create_true_labels()

areas      <- dir(opts$npy_source, full.names = TRUE)
areas      <- areas[!(grepl(x = areas, pattern =  "\\."))]
nareas     <- length(areas)

area_id    <- 0
area_stats <- vector("list", nareas)
for(area in areas){
  area_name  <- sub("area_output/", "", area)
  area_id    <- area_id + 1
  write(paste("Processing", area_name), stdout())
  segments   <- dir(area, full.names = TRUE)
  nseg       <- length(segments)
  seg_all    <- vector("list", nseg)
  seg_id     <- 1
  for( seg in segments){
    seg_xyz    <- npyLoad(paste0(seg, "/xyzrgb.npy"))
    seg_lab    <- npyLoad(paste0(seg,"/label.npy"))
    seg_all[[seg_id]]<- as.data.table(cbind(seg_xyz, seg_lab))
    seg_id     <- seg_id + 1
  }
  
  seg_dt     <- rbindlist(seg_all)
  setnames(seg_dt, c("X", "Y", "Z", "Intensity", "R", "G", "B", "Classification"))
  seg_dt[, Intensity := as.integer(Intensity*intens_max)]
  seg_dt[, R := as.integer(R * rgb_max)][, G := as.integer(G * rgb_max)][, B := as.integer(B * rgb_max)]
  seg_dt[, Classification := as.integer(Classification)]
  setkey(seg_dt, X, Y)
  las <- LAS(seg_dt)
  writeLAS(las, paste0(opts$las_output, area_name, "_joined.las"))
  area_stats[[area_id]] <- data.table(t(c(xmax = max(seg_dt$X), xmin = min(seg_dt$X),
                               ymax = max(seg_dt$Y), ymin = min(seg_dt$Y),
                               unlist(table(true_labels[1+seg_dt$Classification, "category"])))))
  area_stats[[area_id]][, area_name := area_name]
}
area_stats_dt <- rbindlist(area_stats, fill = TRUE, use.names = TRUE)
category_weights <- 1 / colSums(area_stats_dt[!area_name %in% c(opts$test_area, opts$valid_area) , true_labels$category, with=FALSE])
category_weights <- category_weights/sum(category_weights)
npySave(object = category_weights, filename = paste0(opts$npy_source, "category_weights.npy"))
write.csv(x = area_stats_dt, file = paste0(opts$npy_source, "area_stats.csv"))
