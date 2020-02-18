map_grid_maker <- function(map, seg_size, X="X", Y="Y"){
  # Generate database of breakpoints for mapchunks of size seg_size^2
  # DB only includes area on map, not a multiple of seg_size
  
  .dim_to_seq <- function(dim){
    # Helper function to genreate sequence that covers dimension
    drange <- range(map[ ,..dim])
    dseq <- seq(drange[1], drange[2], by = seg_size)
    dseq[length(dseq)] <- drange[2]
    dseq
  }
  
  xseq <- .dim_to_seq(X)
  yseq <- .dim_to_seq(Y)
  xsize <- max(xseq) - min(xseq)
  ysize <- max(yseq) - min(yseq)
  
  # Create separate grids for min/max coord and return their bind
  startgrid <- expand.grid(xmin = xseq[-length(xseq)], ymin = yseq[-length(yseq)])
  endgrid <- expand.grid(xmax = xseq[-1], ymax = yseq[-1])
  write(paste("Map is",round(xsize), "times", round(ysize), "m or", round(xsize*ysize/10^6,2), "sq.km."), stdout())
  cbind(startgrid, endgrid)
}
