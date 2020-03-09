# Function for removing a class label from the output files of las_data_prep.R

suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(RcppCNPy))

option_list <- list( 
  make_option(c("-n", "--npy_source"), type = "character", default="area_output", 
              help="Directory where source numpy files are located as subfolders [default %default]",
              dest = "npy_source"),
  make_option(c("-f", "--label_from"), type = "integer", default=1, 
              help="Label to be replaced (and subsequent to be decreased by 1) [default %default]",
              dest = "label_from"),
  make_option(c("-t", "--label_to"), type = "integer", default=1, 
              help="Label to replace all instances of label_from [default %default]",
              dest = "label_to"),
  make_option(c("-v", "--verbose"), type = "logical", default=TRUE, 
              help="Should progress be printed after each segment? [default %default]",
              dest = "verbose")
)

opt_parser <- OptionParser(option_list=option_list)
opts       <- parse_args(opt_parser)

areas      <- dir(opts$npy_source, full.names = TRUE)
areas      <- areas[!(grepl(x = areas, pattern =  "\\."))]
nareas     <- length(areas)

area_id    <- 0
repl_counter <- vector("integer", nareas)
for(area in areas){
  area_name  <- sub("area_output/", "", area)
  area_id    <- area_id + 1
  write(paste("\n  Processing", area_name), stdout())
  repl_counter[area_id] <- 0
  segments   <- dir(area, full.names = TRUE)
  nseg       <- length(segments)
  seg_all    <- vector("list", nseg)
  seg_id     <- 1
  for( seg in segments){
    seg_lab    <- npyLoad(paste0(seg,"/label.npy"))
    if(length(seg_lab) == 0){ next }
    
    replaced_idx <- seg_lab == opts$label_from
    seg_lab[replaced_idx] <- opts$label_to
    if(opts$label_from != opts$label_to){
      seg_lab[seg_lab > opts$label_from] <- seg_lab[seg_lab > opts$label_from] - 1
    }
    
    npySave(paste0(seg,"/label.npy"), object = seg_lab)
    if(opts$verbose){
      write(paste0("    Replaced ", sum(replaced_idx), " instances of label ", opts$label_from, 
                 " in ", sub(area, "", seg)), stdout())
    }
    repl_counter[area_id] <- repl_counter[area_id] + sum(replaced_idx)
  }
  write(paste0("  Replaced ", repl_counter[area_id], " instances of label ", opts$label_from, 
               " in ", area_name), stdout())
}
write(paste0("Replaced ", sum(repl_counter), " instances of label ", opts$label_from), 
      stdout())