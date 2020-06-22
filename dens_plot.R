library(ggplot2)
library(lidR)
library(data.table)

las <-  readLAS("~/Downloads/Area_3_joined(1).las")
load("~/Downloads/hdpintervals.Rdata")
ggplot(las@data, aes(x= X)) + 
  geom_density(fill = viridisLite::viridis(1, begin=1/4), adjust = 1/4) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Density estimate of points along X-axis for area Åkerbo SV") +
  scale_x_continuous("X-coordinate", breaks= seq(539000,max(las@data$X), 200))
# 
# xdens <- density(las@data$X, n =703, adjust = 1/4)
# dens_data <- data.table(x = xdens$x, y=xdens$y, pdens = "Low")
# for(rowno in seq(11, 16)){
#   bounds <- unlist(intervalsbind[rowno,])
#   dens_data[x %between% bounds, pdens := "High"]
# }
# plot(xdens)
# 
# ggplot(dens_data, aes(x=x, y=y, fill=pdens, colour =pdens)) + geom_col(col=NA) + 
#   scale_fill_viridis_d(begin = 1/6, end = 19/20, aesthetics = c("fill", "colour")) + 
#   theme_minimal() + 
#   scale_x_continuous(breaks = seq(539000,540500,100))
# 
