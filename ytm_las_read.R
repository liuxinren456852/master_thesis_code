library(lidR)
library(viridis)
pal <- cividis(50)
setwd("kartor")
laspath <- "kvarn_data/laserdata/"
ytmpath <- "kvarn_data/ytmodell/"
lasfiles <- paste0(laspath, dir(laspath, ".la[sz]+$"))
ytmfiles <- paste0(ytmpath, dir(ytmpath, ".la[sz]+$"))

curr_tile <- 2
ytm <- readLAS(ytmfiles[curr_tile], select = "xyzRGB")#, filter = "-drop_x_above 518000, -drop_y_above 6495500")
#plot(ytm, colorPalette = pal, color = "B")

las <- readLAS(lasfiles[curr_tile], select = "xyzi", filter = "-drop_z_above 140 -drop_x_above 518000, -drop_y_above 6495500")
hist(las@data$X)
hist(las@data$Y)

plot(las, colorPalette = pal,)

  library(magrittr)
library(data.table)

dt_closest <- function(x, y, tol = 1.5){
  closepts <- as.matrix(ytm@data[X>x-tol & X<x+tol & Y>y-tol & Y<y+tol, .(X,Y,R,G,B)])
  closest <- which.min(dist(rbind(c(x,y), closepts[,1:2]))[1:nrow(closepts)])
  closepts[closest,3:5]
}


library(parallel)
setkey(ytm@data, X, Y)
cl <- makeForkCluster(8)
seg <- 1L
seg_size <- 15000
seg_max <- seg_min <- 0
las_rgb <- vector("list", nrow(las@data) %/% seg_size + 1)
while(seg_max < 31000){
  seg_max <- min(seg*seg_size, nrow(las@data) )
  seg_min <- (seg-1)*seg_size + 1
  cat(paste("Segment", seg, "; obs", seg_min, "to", seg_max, "started at", Sys.time(),"\n"), 
      file= "write_log.txt", append=TRUE)
  las_rgb[seg] <- unlist(parLapply(cl = cl, X = seq.int(seg_min, seg_max), fun = function(i){
    dt_closest(las@data[i,X], las@data[i,Y])
    }))
  seg <- seg + 1L
} 

las_rgb_combined <- data.table(t(do.call(cbind, las_rgb)))
las@data[,(c("R","G","B")):=las_rgb_combined]

#dt_closest(las@data$X[15001], las@data$Y[15001])
#x<-las@data$X[1]; y<- las@data$Y[1]
