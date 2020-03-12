.png_worldfile_to_transform_matrix <- function(mapname){
  # Reads worldfile into transformation matrix 
  # Source: https://en.wikipedia.org/wiki/World_file
  return(t(matrix(as.numeric(readLines(paste0(mapname,".pgw"))), ncol = 3)))
}

.png_map_to_df <- function(mapname){
  # Reads png-map into data.table
  map_dt <- dcast(as.data.table(readPNG(paste0(mapname,".png"))), 
                  V1 + V2 ~ V3, value.var = "value")
  setnames(map_dt, names(map_dt), c("pY", "pX", "R", "G", "B", "A")[1:length(names(map_dt))])
  return(map_dt)
}

png_map_reader <- function(mapfile, true_categories){
  # Reads a georeferenced png-file into a data.table of X,Y,R,G,B values
  # where X and Y are UTM-coordinates
  mapname <- sub(pattern = ".png$", replacement = "", x = mapfile) 
  stopifnot(all(file.exists(paste0(mapname,c(".png", ".pgw")))))
  suppressPackageStartupMessages(require(png))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(pdist))
  
  map_trns <- .png_worldfile_to_transform_matrix(mapname)
  map_dt   <- .png_map_to_df(mapname)
  # Find closest colour per pixel among true catgories
  cat_dists<- apply(X = as.matrix(pdist(map_dt[,.(R,G,B)], true_categories[, .(R,G,B)])),
                    MARGIN = 1, FUN = which.min)
  
  # Do the affine transform from pixels to coordinates
  map_xy   <- data.table(as.matrix(cbind(map_dt[, .(pX,pY)], 1)) %*% map_trns)
  
  # Adds X and Y to map data. Pixelcoords kept to enable plotting
  map_dt[, c("X", "Y") := map_xy][, colour := rgb(map_dt[,.(R,G,B)])]
  map_dt[, c("cat_colour") := true_categories[cat_dists, .(colour)]]
  map_dt[, c("category") := true_categories[cat_dists, .(ID)]]
  setcolorder(map_dt, c("X", "Y", "category", "R", "G", "B", "colour", "cat_colour", "pX", "pY"))
  setkey(map_dt, X, Y)
  return(map_dt)
}
