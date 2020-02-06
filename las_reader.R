las_filter_string <- function(map_grid, tol=0){
  # Function to build the string used in filter-arg of lidR::readLAS
  # given a the extents of a map
  xmin <- paste("-drop_x_below", min(map_grid$xmin)-tol)
  xmax <- paste("-drop_x_above", max(map_grid$xmax)+tol)
  ymin <- paste("-drop_y_below", min(map_grid$ymin)-tol)
  ymax <- paste("-drop_y_above", max(map_grid$ymax)+tol)
  paste(xmin, xmax, ymin, ymax, sep = ", ")
}

las_reader <- function(map_dir, map_grid, type, tol){
  # Function to read all .las/Z files in dir given type and filter
  # out only the relevant areas
  require(lidR)
  if(type == "las"){
    path          <- paste0(map_dir, "/laserdata/")
    select_string <- "xyzi"
  } else if(type == "sfm"){
    path          <- paste0(map_dir, "/ytmodell/")
    select_string <- "xyzRGB"
  }
  files         <- paste0(path, dir(path, ".la[sz]+$"))
  filter_string <- las_filter_string(map_grid, tol = tol) 
  lasdata       <- readLAS(files = files, select = select_string, filter = filter_string)
  setkey(lasdata@data, X, Y) # To ensure indexing is done
  lasdata
}