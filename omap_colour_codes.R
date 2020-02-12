create_true_labels <- function(){
  # Create DT of colour codes and export those that are named
  suppressPackageStartupMessages(require(data.table))
  col_codes <- matrix(ncol = 3, byrow=TRUE, dimnames = list(c(), c("R", "G", "B")), c(
    000,  000,  000, 
    100,  240,  255, 
    020,  120,  200, 
    150,  255,  138, 
    051,  160,  044, 
    251,  050,  153,  
    227,  026,  040, 
    253,  191,  111, 
    255,  127,  000, 
    220,  150,  230, 
    106,  061,  154, 
    255,  255,  255  
  ))/255
  ccodes <- data.table(col_codes)[, colour := rgb(R,G,B)]
  ccodes[, category := c("roads", "water", "marsh", "openground", 
                         "denseforest", "building", "trail", "unused", 
                         "mediumforest", "unused", "mountains" , "forest")]
  
  ccodes[category!="unused", ][, ID:= 1:.N]
}

.plot_categories <- function(true_labels){
  library(ggplot2)
  ggplot(true_labels, aes(y=1:length(colour), x=0, width = 1, height = 1)) +
    geom_tile(fill = true_labels$colour, col = "black", size = rel(1)) +
    geom_label(aes(label = colour), size = rel(5), nudge_x = -.45,hjust = "inward") +
    geom_label(aes(label = category), size = rel(5), nudge_x = .45, hjust="inward") +
    theme_void()
}


#round(as.matrix(dist(col_codes)))
#min(dist(col_codes))
# plotly::plot_ly(x=ccodes[,R], y=ccodes[,G], z=ccodes[,B], color=ccodes[,ID], mode="marker")
# 
# library(ggplot2)
# ggplot(ccodes, aes(x=ID, fill=category,y=0,height=1,width=1)) +
#   geom_tile(col="grey50") +
#   scale_fill_manual(values=setNames(ccodes$HEX,ccodes$category)) + 
#   theme_void()
