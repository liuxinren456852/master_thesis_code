library(lidR)
paths <- dir("~/laslager",full.names = TRUE)
paths <- paths[!grepl("\\.", paths)]
paths <- paste0(paths, "/laserdata")
nsegs <- 1024

intervals <- list()
for(area in 1:4){
  lascat <- catalog(paths[area])
x <- readLAS(lascat, select = "X")@data$X
tol <- ((max(x) - min(x))/nsegs)/2

densx <- density(x, n = nsegs)
mean_y <- mean(densx$y)
plot(densx)
abline(h=mean_y)
xabove <- (densx$x[which(densx$y > mean_y)])

dens_y_scaled <- densx$y/sum(densx$y)

thresh <- mean(dens_y_scaled)
width <- length(xabove)/nsegs
dens_y_order <- order(densx$y, decreasing = TRUE)

hpd_idx <- dens_y_order[1:(nsegs*width)]

hpd_order <- sort(hpd_idx)
hpd_lag <- hpd_order+1



intervals[[area]] <- data.frame(xmin = densx$x[setdiff(hpd_order,hpd_lag)]+tol,
                        xmax = densx$x[setdiff(hpd_lag,hpd_order)])

gc()
}
area <- area+1

intervalsbind <- data.table::rbindlist(intervals)
library(data.table)
test <- data.table(X = seq(536860,542000,25), Y =seq(6437500,6442000, length.out = 206), dens = "L")

for(rowno in seq(1, nrow(intervalsbind))){
test[X %between% intervalsbind[rowno,], dens := "H"]
test[Y %between% intervalsbind[rowno,], dens := "H"]
}

save(file="hdpintervals.Rdata", intervalsbind)
