.png_worldfile_to_transform_matrix <- function(mapname){
  # Reads worldfile into transformation matrix 
  # Source: https://en.wikipedia.org/wiki/World_file
  return(t(matrix(as.numeric(readLines(paste0(mapname,".pgw"))), ncol = 3)))
}

.png_map_to_df <- function(mapname){
  # Reads png-map into data.table
  map_dt <- dcast(as.data.table(readPNG(paste0(mapname,".png"))), 
                  V1 + V2 ~ V3, value.var = "value")
  setnames(map_dt, names(map_dt), c("pY", "pX", "R", "G", "B", "A"))
  return(map_dt)
}

.png_color_kmeans <- function(mapname, true_categories){
  # Forces colours of read png-map into the true colours, dealiasing the stat way!
  true_centers <- as.matrix(true_categories[, .(R,G,B)])
  pixel_kmeans <- kmeans(x = mapname[, .(R,G,B)], centers = true_centers)
  mapname[, category := true_categories[pixel_kmeans$cluster, category]]
  mapname[, cat_colour := true_categories[pixel_kmeans$cluster, colour]]
  mapname[, cat_id := true_categories[pixel_kmeans$cluster, id]]
}

true_category_dist <- function(x) {
  which.min(pdist(unlist(x), true_labels[, .(R,G,B)])@dist)
}

true_category_dist <- function(x) {
  sum(unlist(x))
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
  map_dt[, c("category") := cat_dists]
  setcolorder(map_dt, c("X", "Y", "category", "R", "G", "B", "colour", "cat_colour", "pX", "pY"))
  setkey(map_dt, X, Y)
  return(map_dt)
}

map_dt_plot <- function(map, x = "pX", y = "pY", colour = "colour"){
  # Helper for plotting the map used
  oldkey <- key(map)
  setkeyv(map, c(x,y))
  colmat <- col2rgb(unlist(map[, ..colour]))/255
  # Using coordinates here will require massive memory... hence pX,pY
  map_array <- array(dim = c(max(map[,..y]), 
                             max(map[,..x]),
                             3))
  map_array[,,1] <- colmat[1, ]
  map_array[,,2] <- colmat[2, ]
  map_array[,,3] <- colmat[3, ]
  # plot.new()
  # grid.raster(map_array)
  png::writePNG(map_array, paste0(colour, "_map.png"))
  setkeyv(map, oldkey)
}
