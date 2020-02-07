# The below functions make heavy use of data.table and it's speed, don't even consider
# rewriting this with dplyr, it takes forever to do a couple of million filter()-calls.

dt_closest <- function(xy, target_data, target_tol = 1){
  # Retrieves RGB-values from target data for point in target data closest to given
  # coordinates (implicit from source data).

  # Subset all points within distance target_tol from x,y
  closepts <- as.matrix(target_data[X>xy[1]-target_tol &
                                      X<xy[1]+target_tol &
                                      Y>xy[2]-target_tol &
                                      Y<xy[2]+target_tol, ])
  while(dim(closepts)[1] == 0 & target_tol < 16){
    # In the rare event that no points are within tolerance:
    target_tol <- target_tol * 3
    closepts <- as.matrix(target_data[X>xy[1]-target_tol &
                                        X<xy[1]+target_tol &
                                        Y>xy[2]-target_tol &
                                        Y<xy[2]+target_tol, ])
  }
  # Calculate distance matrix for these points and get index of closest one
  # Indexing at the end due to dist really beinga vector and diag is not included
  closest <- which.min(dist(rbind(xy, closepts[,1:2]))[1:nrow(closepts)])
  # Return original coord along with merged data
  c(xy,closepts[closest,-c(1,2)])
}

dt_sq_average <- function(xy, target_data, target_tol = 1){
  # Retrieves average RGB-values from points in target data within target_tol distance
  # from given coordinates (implicit from source data).
  # Subset all points within distance target_tol from x,y
  closepts <- as.matrix(target_data[X>xy[1]-target_tol &
                                      X<xy[1]+target_tol &
                                      Y>xy[2]-target_tol &
                                      Y<xy[2]+target_tol, ])
  
  while(dim(closepts)[1] == 0 & target_tol < 16){
    # In the rare event that no points are within tolerance:
    target_tol <- target_tol * 3
    closepts <- as.matrix(target_data[X>xy[1]-target_tol &
                                        X<xy[1]+target_tol &
                                        Y>xy[2]-target_tol &
                                        Y<xy[2]+target_tol, ])
  }
  # Return original coord along with merged data
  c(xy,colMeans(closepts[closest,-c(1,2)]))
}


seg_lookup_factory <- function(seg_source, seg_target, target_tol, fun){
  # Creates a function that runs dt_closest in the curent segment of data
  function(point_no){
    fun(xy = unlist(seg_source[point_no, ]), #unlist to get it to vector
        target_data = seg_target,
        target_tol  = target_tol)
  }
}

dt_lookup_factory <- function(map_grid, source_data, source_var, target_data, target_var, target_tol, fun, cl){
  # Creates a function subsets data for a segment and then runs closest lookup in those
  # data. For use in lapply.
  suppressPackageStartupMessages(require(parallel))
  function(seg_no){
    write(paste("Segment", seg_no, "started at", Sys.time()), stdout())
    seg_limits <- map_grid[seg_no, ]

    # Create  temporary data sets for source and target
    seg_source <- source_data[X>=seg_limits$xmin &
                                X<=seg_limits$xmax &
                                Y>=seg_limits$ymin &
                                Y<=seg_limits$ymax,
                              ..source_var]
    seg_target <- target_data[X>=seg_limits$xmin-sfm_tol &
                                X<=seg_limits$xmax+sfm_tol &
                                Y>=seg_limits$ymin-sfm_tol &
                                Y<=seg_limits$ymax+sfm_tol,
                              ..target_var]

    seg_lookup <- seg_lookup_factory(seg_source, seg_target, target_tol, fun)

    # Make cluster to speed up searching for values
    looked_up <-parSapply(cl = cl, X = seq.int(1, nrow(seg_source)), FUN = seg_lookup)
    looked_up_dt <- transpose(as.data.table(looked_up))
    return(looked_up_dt)
  }
}
