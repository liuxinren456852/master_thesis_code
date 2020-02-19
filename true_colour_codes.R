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
    geom_label(aes(label = colour, family ="serif"), size = rel(3), nudge_x = -.45,hjust = "inward") +
    geom_label(aes(label = category, family = "serif"), size = rel(3), nudge_x = .45, hjust="inward", ) +
    theme_void()
}
