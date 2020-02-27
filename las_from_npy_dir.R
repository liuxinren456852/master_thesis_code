suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(lidR))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(RcppCNPy))

option_list <- list( 
  make_option(c("-n", "--npy_source"), type = "character", default="area_output", 
              help="Directory where source numpy files are located as subfolders [default %default]",
              dest = "npy_source"),
  make_option(c("-l", "--las_output"), type = "character", default="las_output/", 
              help="Directory where to export the las-files [default %default]",
              dest = "las_output")
  
)

opt_parser <- OptionParser(option_list=option_list)
opts       <- parse_args(opt_parser)

if(!dir.exists(opts$las_output)) { dir.create(opts$las_output) }

rgb_max    <- 2^16
intens_max <- 255

areas      <- dir(opts$npy_source, full.names = TRUE)
nareas     <- length(areas)
areas      <- areas[!(grepl(x = areas, pattern =  "\\."))]
area_id    <- 0
#area_stats <- matrix(nrow = nareas, ncol = 4)

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
  area_stats[area_id, ] <- c(range(seg_dt$X), range(seg_dt$Y),
                             c(table(true_labels[seg_dt$Classification, "category"])))
}
