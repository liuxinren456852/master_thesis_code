# The below functions make heavy use of data.table and it's speed, don't even consider
# rewriting this with dplyr, it takes forever to do a couple of million filter()-calls.

.dt_closest <- function(xy, target_data, target_tol = 1){
  # Retrieves RGB-values from target data for point in target data closest to given
  # coordinates (implicit from source data).

  # Subset all points within distance target_tol from x,y
  closepts <- as.matrix(target_data[X>xy[1]-target_tol &
                                      X<xy[1]+target_tol &
                                      Y>xy[2]-target_tol &
                                      Y<xy[2]+target_tol, ])
  while(dim(closepts)[1] == 0 & target_tol < 11){
    # In the rare event that no points are within tolerance:
    target_tol <- target_tol * 3
    closepts <- as.matrix(target_data[X>xy[1]-target_tol &
                                        X<xy[1]+target_tol &
                                        Y>xy[2]-target_tol &
                                        Y<xy[2]+target_tol, ])
  }
  # Calculate distance matrix for these points and get index of closest one
  # Indexing at the end due to dist really beinga vector and diag is not included
  #closest <- which.min(dist(rbind(xy[1:2], closepts[,1:2]))[1:nrow(closepts)])
  closest <- which.min(pdist::pdist(xy[1:2], closepts[, 1:2])@dist)
  # Return original coord along with merged data or NA of missing (yes xy-width is hardcoded...)
  if(length(closest)==0){
    return(c(xy, rep(NA, ncol(closepts)-2)))
  } else {
    return(c(xy,unlist(closepts[closest,-c(1,2)])))
  }
}

.dt_sq_average <- function(xy, target_data, target_tol = 1){
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

dt_lookup_factory <- function(map_grid, by, source_data, source_var, 
                              target_data, target_var, target_tol, fun, cl){
  # Creates a function subsets data for a segment and then runs closest lookup in those
  # data. For use in lapply.
  #suppressPackageStartupMessages(require(parallel))
  function(seg_no){
    browser()
    write(paste("Segment", seg_no, "started at", Sys.time()), stdout())
    seg_limits <- map_grid[rownum == seg_no, ]
    # Create  temporary data sets for source and target
    if("data.frame" %in% class(source_data)){
      seg_source <- source_data[X>=seg_limits$xmin &
                                  X<=seg_limits$xmax &
                                  Y>=seg_limits$ymin &
                                  Y<=seg_limits$ymax,
                                c(by, source_var), with = FALSE]
    } else {
      seg_source <- source_data[[((seg_no-1)%%10)+1]]
    }
    seg_target <- target_data[X>=seg_limits$xmin-target_tol &
                                X<=seg_limits$xmax+target_tol &
                                Y>=seg_limits$ymin-target_tol &
                                Y<=seg_limits$ymax+target_tol,
                              c(by, target_var), with = FALSE]

    seg_lookup <- seg_lookup_factory(seg_source, seg_target, target_tol, fun)

    # Make cluster to speed up searching for values
    looked_up <-sapply(X = seq.int(1, nrow(seg_source)), FUN = seg_lookup)
    looked_up_dt <- transpose(as.data.table(looked_up))
    setnames(looked_up_dt, c(by, source_var, target_var))
    # Return only rows that aren't missing info. 
    return(looked_up_dt[complete.cases(looked_up_dt[,..target_var]),])
  }
}

dt_fuzzy_join_factory <- function(limits, by, source_data, source_var, 
                          target_data, target_var, target_tol, fun, cl){
  function(seg_no){
    seg_limits <- unlist(limits[rownum == seg_no, ])
    looked_up <- as.matrix(source_data[X>=seg_limits["xmin"] &
                                         X<=seg_limits["xmax"] &
                                         Y>=seg_limits["ymin"] &
                                         Y<=seg_limits["ymax"],
                                       c(by, source_var), with = FALSE])
    for(t_id in seq_along(target_data)){
      init_time <- Sys.time()
      seg_target <- target_data[[t_id]][X>=seg_limits["xmin"]-target_tol[[t_id]] &
                                          X<=seg_limits["xmax"]+target_tol[[t_id]] &
                                          Y>=seg_limits["ymin"]-target_tol[[t_id]] &
                                          Y<=seg_limits["ymax"]+target_tol[[t_id]],
                                        c(by, target_var[[t_id]]), with = FALSE]
      #write(paste(t_id, "target subset"), stdout())
      seg_lookup <- seg_lookup_factory(looked_up, seg_target, target_tol[[t_id]], .dt_closest)
      
      # Make cluster to speed up searching for values
      looked_up <-t(parSapply(X = seq.int(1, nrow(looked_up)), FUN = seg_lookup, cl = cl))
      #write(paste("target", t_id, "joined in", round(difftime(Sys.time(), init_time, units = "secs"),0),"s."), stdout())
    }
    #looked_up_dt <- as.data.table(looked_up)
    #setnames(looked_up_dt, c(by, source_var, unlist(target_var)))
    as.data.table(looked_up)
  }
}
