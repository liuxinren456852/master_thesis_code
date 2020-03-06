suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-s", "--source"), type = "character", default="~/master_thesis_code/pvcnn/data/terrain/h5_output/", 
              help="Directory where source maps are located as subfolders [default %default]",
              dest = "source"),
  make_option(c("-t", "--test"), type = "double", default = 0.2,
              help = "Number of cores to use [default %default]",
              dest = "test_prob"),
  make_option(c("-v", "--valid"), type = "double", default = 0.2,
              help = "Number of cores to use [default %default]",
              dest = "valid_prob")
)

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

areas      <- dir(opts$source, pattern = "^Area")
nareas     <- length(areas)
test_dir  <- paste0(opts$source, "Area_", nareas+1, "/")
valid_dir <- paste0(opts$source, "Area_", nareas+2, "/")

dir.create(test_dir)
dir.create(valid_dir)

for (area in areas){
  area_dir   <- paste0(opts$source, area, "/")
  datasets   <- dir(area_dir, pattern = "^seg")
  ndatasets  <- length(datasets)
  splits     <- sample(c("train", "test", "valid"), size = ndatasets, replace = TRUE,
                       prob = c(1-(opts$test_prob + opts$valid_prob), 
                                opts$test_prob, 
                                opts$valid_prob))
  for(id in seq_along(datasets)){
    if(splits[id] == "test"){
      source_name  <- paste0(area_dir, datasets[id])
      target_name  <- paste0(area_dir, area,"_", datasets[id])
      file.rename(source_name, target_name)
      file.copy(target_name, paste0(test_dir))
    } else if(splits[id] == "valid") {
      
    }
  }
}
