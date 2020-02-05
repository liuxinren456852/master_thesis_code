png_map_reader <- function(mapname){
  # Reads a georeferenced png-file into a data.table of X,Y,R,G,B values
  # where X and Y are UTM-coordinates
  require(png)
  require(data.table)
  stopifnot(all(file.exists(paste0(mapname,c(".png", ".pgw")))))
  
  .png_worldfile_to_transform_matrix <- function(mapname){
    # Reads worldfile into transformation matrix 
    # Source: https://en.wikipedia.org/wiki/World_file
    return(t(matrix(as.numeric(readLines(paste0(mapname,".pgw"))), ncol = 3)))
  }
  
  .png_map_to_df <- function(mapname){
    # Reads 4-channel png-map into data.table and drops alpha-channel
    map_dt <- dcast(as.data.table(readPNG(paste0(mapname,".png"))), 
                    V1 + V2 ~ V3, value.var = "value")
    setnames(map_dt, names(map_dt), c("pY", "pX", "R", "G", "B", "A"))
    map_dt[, A := NULL]
    return(map_dt)
  }
  
  map_trns <- .png_worldfile_to_transform_matrix(mapname)
  map_dt   <- .png_map_to_df(mapname)
  
  # Does the affine transform from pixels to coordinates
  map_xy   <- data.table(as.matrix(cbind(map_dt[, .(pX,pY)], 1)) %*% map_trns)
  
  # Adds X and Y to map data and removes pixelcoordinates
  map_dt[, c("X", "Y") := map_xy][, c("pX", "pY") := NULL]
  setcolorder(map_dt, c("X", "Y", "R", "G", "B"))
  
  return(map_dt)
}

