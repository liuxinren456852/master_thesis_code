las_filter_string <- function(map_grid, tol=0, zlim ){
  # Function to build the string used in filter-arg of lidR::readLAS
  # given a the extents of a map
  xmin <- paste("-drop_x_below", min(map_grid$xmin)-tol)
  xmax <- paste("-drop_x_above", max(map_grid$xmax)+tol)
  ymin <- paste("-drop_y_below", min(map_grid$ymin)-tol)
  ymax <- paste("-drop_y_above", max(map_grid$ymax)+tol)
  zmin <- paste("-drop_z_below", zlim["min"])
  zmax <- paste("-drop_z_above", zlim["max"])
  paste(xmin, xmax, ymin, ymax, zmin, zmax ,sep = ", ")
}

las_reader <- function(map_cat = NULL, las_dir = NULL, map_grid, select_string, tol, zlim){
  # Function to read all .las/Z files in dir given type and filter
  # out only the relevant areas. Alternatively can read from a catalog.
  suppressPackageStartupMessages(require(lidR))
  filter_string <- las_filter_string(map_grid, tol = tol, zlim) 
  if(!is.null(map_cat)){
    lasdata       <- readLAS(files = map_cat, select = select_string, filter = filter_string)
  } else {
    files         <- paste0(las_dir, dir(las_dir, ".la[sz]+$"))
    lasdata       <- readLAS(files = files, select = select_string, filter = filter_string)
  }
  setkey(lasdata@data, X, Y) # To ensure indexing is done
  lasdata
}
