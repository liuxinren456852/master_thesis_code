point_dist_plot <- function(data, column, true_labels, save_formats = c("png", "pdf")){
    # Little helper to plot the distribution of terrain points from a las data set
    suppressPackageStartupMessages(require(ggplot2))
    label_vec   <- setNames(true_labels$colour, true_labels$category)
    
    # Set theme outside fo clarity
    theme_set(theme_minimal(base_family = "serif", base_size = 12) +
                  theme(axis.title.x = element_text(angle = 0, hjust = 1),
                        axis.title.y = element_text(angle = 0, vjust = 1, hjust = 1, 
                                                    margin = margin(0,-20,0,0)),
                        legend.position = "none",
                        axis.text.x = element_text(angle = 30, hjust = 1, size = 11),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank()
                        ))
    
    ggplot(data, aes_string(x = column, fill = column)) + 
        geom_bar(col = "black", width = 0.5) + 
        scale_fill_manual("", values = label_vec) +
        scale_y_continuous("No. of points",expand = c(0,0)) +
        labs(title = paste("Number of points per terrain", column, "\n"),
             x = "Terrain")
    
    # Save specified file formats
    lapply(save_formats, function(format){
        ggsave(paste0("point_category_distribution.", format), 
               height = 9, width = 20, units = "cm")
    })
}

map_dist_plot <- function(data, column, true_labels, save_formats = c("png", "pdf")){
    # Little helper to plot the distribution of terrain points from omap totals
    suppressPackageStartupMessages(require(ggplot2))
    label_vec   <- setNames(true_labels$colour, true_labels$category)
    
    # Set theme outside fo clarity
    theme_set(theme_minimal(base_family = "serif", base_size = 12) +
                  theme(axis.title.x = element_text(angle = 0, hjust = 1),
                        axis.title.y = element_text(angle = 0, vjust = 1, hjust = 1, 
                                                    margin = margin(0,-20,0,0)),
                        legend.position = "none",
                        axis.text.x = element_text(angle = 30, hjust = 1, size = 11),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank()
                  ))
    
    ggplot(data, aes_string(x = column, fill = column)) + 
        geom_bar(col = "black", width = 0.5) + 
        scale_fill_manual("", values = label_vec) +
        scale_y_continuous("No. of points",expand = c(0,0)) +
        labs(title = paste("Number of points per terrain", column, "\n"),
             x = "Terrain")
    
    # Save specified file formats
    lapply(save_formats, function(format){
        ggsave(paste0("point_category_distribution.", format), 
               height = 9, width = 20, units = "cm")
    })
}


timing_writer <- function(init_time, end_time, nobs){
    total_time <- difftime(end_time, init_time, units = "mins")
    record_time<- as.numeric(total_time) * 60 / (nobs/1000)
    time_record<- nobs / as.numeric(total_time)
    alarm()
    write(paste("Stage complete.\nTotal time:", round(total_time, 2), 
                "minutes.\nTime per 1K points:", round(record_time, 4), 
                "seconds\nRecords per minute:", round(time_record, 1),"\n"), 
          stdout())
}

map_dt_plot <- function(map, x = "pX", y = "pY", colour = "colour", dirname = ""){
    # Helper for plotting the map used
    oldkey <- key(map)
    setkeyv(map, c(x,y))
    colmat <- col2rgb(unlist(map[, ..colour]))/255
    # Using coordinates here will require massive memory... hence pX,pY
    map_array <- array(dim = c(max(map[,..y]), 
                               max(map[,..x]),
                               3))
    map_array[,,1] <- colmat[1, ]
    map_array[,,2] <- colmat[2, ]
    map_array[,,3] <- colmat[3, ]
    setkeyv(map, oldkey)
    png::writePNG(map_array, paste0(dirname, "_", colour, "_map.png"))
}
