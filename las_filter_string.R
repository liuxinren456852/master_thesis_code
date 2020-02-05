las_filter_string <- function(map_grid, tol=0){
  # Function to build the string used in filter-arg of lidR::readLAS
  # given a the extents of a map
  xmin <- paste("-drop_x_below", min(map_grid$xmin)-tol)
  xmax <- paste("-drop_x_above", max(map_grid$xmax)+tol)
  ymin <- paste("-drop_y_below", min(map_grid$ymin)-tol)
  ymax <- paste("-drop_y_above", max(map_grid$ymax)+tol)
  paste(xmin, xmax, ymin, ymax, sep = ", ")
}
