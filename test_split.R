# Helper script to move a subset of segments to validation and test-folders

suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("data.table"))

source("little_helpers.R")

option_list <- list( 
  make_option(c("-s", "--source"), type = "character", 
              default="~/master_thesis_code/area_output2/", 
              help="Directory where source maps are located as subfolders [default %default]",
              dest = "source"),
  make_option(c("-t", "--test"), type = "double", default = 0.15,
              help = "Proportion of segments moved to test set  [default %default]",
              dest = "test_prob"),
  make_option(c("-v", "--valid"), type = "double", default = 0.15,
              help = "Proportion of segments moved to validation set [default %default]",
              dest = "valid_prob"),
  make_option(c("-e", "--seed"), type = "integer", default = 831117,
              help = "Random seed used [default %default]",
              dest = "seed")
)

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

areas      <- dir(opts$source, pattern = "^Area")
nareas     <- length(areas)
test_dir   <- paste0(opts$source, "Area_", nareas+1, "/")
valid_dir  <- paste0(opts$source, "Area_", nareas+2, "/")

dir.create(test_dir)
dir.create(valid_dir)

split_stats<- data.frame(area_id = areas, total = 0, tests = 0, valid = 0)
area_id    <- 0
set.seed(opts$seed)

for (area in areas){
  area_id    <- area_id + 1
  area_dir   <- paste0(opts$source, area, "/")
  datasets   <- dir(area_dir, pattern = "^seg")
  ndatasets  <- length(datasets)
  split_stats[area_id, "total"] <- ndatasets
  splits     <- sample(c("train", "test", "valid"), size = ndatasets, replace = TRUE,
                       prob = c(1-(opts$test_prob + opts$valid_prob), 
                                opts$test_prob, 
                                opts$valid_prob))
  for(id in seq_along(datasets)){
    if(splits[id] == "test"){
      source_dir  <- paste0(area_dir, datasets[id], "/")
      target_dir  <- paste0(test_dir, area,"_", datasets[id], "/")
      dir.create(target_dir)
      files <- dir(source_dir, no.. = TRUE, all.files = TRUE)
      for (f in files){
        file.rename(paste0(source_dir, f), paste0(target_dir, f))
      }
      file.remove(source_dir)
      split_stats[area_id, "tests"] <- split_stats[area_id, "tests"] + 1
    } else if(splits[id] == "valid") {
      source_dir  <- paste0(area_dir, datasets[id], "/")
      target_dir  <- paste0(valid_dir, area,"_", datasets[id], "/")
      dir.create(target_dir)
      files <- dir(source_dir, no.. = TRUE, all.files = TRUE)
      for (f in files){
        file.rename(paste0(source_dir, f), paste0(target_dir, f))
      }
      file.remove(source_dir)
      split_stats[area_id, "valid"] <- split_stats[area_id, "valid"] + 1
    }
  }
}
write(paste0("Test set of ", sum(split_stats$tests), " segments and validation set of ", 
             sum(split_stats$valid)," segments created."), stdout())

write.csv(split_stats, paste0("test_split_stats_t",round(opts$test_prob*100),"v",
                              round(opts$valid_prob*100), "s", opts$seed,".csv"),
          row.names = FALSE)

split_stats_plot(split_stats)
