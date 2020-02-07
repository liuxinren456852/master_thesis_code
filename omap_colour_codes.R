create_true_labels <- function(){
  # Function for creating the set of true laels and their colours
  require(data.table)
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
  
  true_labels[, colour := rgb(true_labels[, .(R,G,B)])]
  setkey(true_labels, colour)
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