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
  setkey(map_dt, pX, pY)
  return(map_dt)
}

.pixel_surround_majority <- function(rowid, map_dt, weights, yres, xres, tol = 2){
  # Since the map_dt is sorted first by X and then Y, we can create a matrix of 
  # rowindexes to subset that represent the surrounding of the current pixel
  tol_seq     <- seq.int(-tol, tol)
  tol_ones    <- t(rep(1,(2*tol+1)))
  rowids      <- t(yres * tol_seq %*% tol_ones) + rowid + tol_seq %*% tol_ones
  rowids      <- rowids[rowids > 1]
  curr_row    <- unlist(map_dt[rowid, c("pX", "pY")])
  # Tabulate category counts and weigh by 
  surround      <- map_dt[c(rowids), ][pX >= curr_row[1]-tol & pX <= curr_row[1]+tol &
                                         pY >= curr_row[2]-tol & pY <= curr_row[2]+tol]
  
  counts        <- surround[,.N,by=cats][,N_w := N*weights[cats]][order(-N_w)][1,cats]
  return(counts)
}

png_map_reader <- function(mapfile, true_categories){
  # Reads a georeferenced png-file into a data.table of X,Y,R,G,B values
  # where X and Y are UTM-coordinates
  mapname <- sub(pattern = ".png$", replacement = "", x = mapfile) 
  stopifnot(all(file.exists(paste0(mapname,c(".png", ".pgw")))))
  suppressPackageStartupMessages(require(png))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(pdist))
   
  weights  <- 1/c(roads = 2.6, water = 2.6, marsh = 3, openland = 7,
                  building = 3.6, trail = 2.6, medforest = 7, forest = 8)
  map_trns <- .png_worldfile_to_transform_matrix(mapname)
  map_dt   <- .png_map_to_df(mapname)
  # Find closest colour per pixel among true catgories
  yres     <- max(map_dt$pY)
  xres     <- max(map_dt$pX)
  map_dt[, cats := apply(X = as.matrix(pdist(map_dt[,.(R,G,B)], true_categories[, .(R,G,B)])),
                    MARGIN = 1, FUN = which.min)]
  map_dt[, catcl := sapply(seq_along(map_dt[,pX]), .pixel_surround_majority, 
                           map_dt = map_dt, weights = weights, yres = yres, xres = xres)]
  
  # Do the affine transform from pixels to coordinates
  map_xy   <- data.table(as.matrix(cbind(map_dt[, .(pX,pY)], 1)) %*% map_trns)
  
  # Adds X and Y to map data. Pixelcoords kept to enable plotting
  map_dt[, c("X", "Y") := map_xy][, colour := rgb(map_dt[,.(R,G,B)])]
  map_dt[, c("cat_colour") := true_categories[catcl, .(colour)]]
  map_dt[, c("category") := true_categories[catcl, .(ID)]]
  map_dt[, catcl := NULL][, cats := NULL]
  setcolorder(map_dt, c("X", "Y", "category", "R", "G", "B", "colour", "cat_colour", "pX", "pY"))
  setkey(map_dt, X, Y)
  return(map_dt)
}







# library(ggplot2)
# ggplot(surround, aes(pX,pY, fill = as.character(cats-1))) + geom_tile(col="black") +scale_fill_manual("",values= cat_col)
# cat_col <- setNames(true_categories$colour, true_categories$ID)
