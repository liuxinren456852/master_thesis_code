create_true_labels <- function(){
  # Function for creating the set of true laels and their colours
  suppressPackageStartupMessages(require(data.table))
  .colour_matrix <- matrix(ncol = 4, byrow = TRUE,data = c(
    1.00,1.00,1.00,"forest",
    1.00,0.73,0.21,"open_ground",
    1.00,0.87,0.60,"dense_forest", #doublecheck!
    0.50,0.50,0.50,"building",
    1.00,0.00,1.00,"marsh",
    0.13,0.82,1.00,"water",
    0.24,1.00,0.09,"very_dense_forest", #doublecheck!
    0.77,1.00,0.73,"easy_forest", #doublecheck!
    0.91,0.65,0.45,"trail",
    0.00,0.00,0.00,"road"
  ))
  
  
  true_labels <- data.table(R = as.numeric(.colour_matrix[,1]),
                            G = as.numeric(.colour_matrix[,2]),
                            B = as.numeric(.colour_matrix[,3]),
                            category = .colour_matrix[,4])
  
  true_labels[, c("colour", "id") := list(rgb(true_labels[, .(R,G,B)]), 1:.N)]
  setkey(true_labels, colour)
  write.csv(true_labels[, .(category, id)], "true_labels.csv", row.names = FALSE)
  return(true_labels)
}

.plot_categories <- function(true_labels){
  library(ggplot2)
  ggplot(true_labels, aes(y=1:length(colour), x=0, width = 1, height = 1)) +
    geom_tile(fill = true_labels$colour, col = "black", size = rel(1)) +
    geom_label(aes(label = colour), size = rel(5), nudge_x = -.45,hjust = "inward") +
    geom_label(aes(label = category), size = rel(5), nudge_x = .45, hjust="inward") +
    theme_void()
}

col_codes <- matrix(ncol = 3, byrow=TRUE, c(
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
  ))
colnames(col_codes) <- c("R", "G", "B")

round(as.matrix(dist(col_codes)))
min(dist(col_codes))

ccodes <- data.table(col_codes)[, ID := 1:.N][, HEX := rgb(R,G,B, maxColorValue = 255)]
plotly::plot_ly(x=ccodes[,R], y=ccodes[,G], z=ccodes[,B], color=ccodes[,ID], mode="marker")
ccodes[, category := c("roads", paste0("latent",0:9), "forest")]
library(ggplot2)
ggplot(ccodes, aes(x=ID, fill=category,y=0,height=1,width=1)) +
  geom_tile(col="grey50") +
  scale_fill_manual(values=setNames(ccodes$HEX,ccodes$category)) + 
  theme_void()
