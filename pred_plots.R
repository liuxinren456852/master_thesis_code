suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-p", "--pred_dir"), type="character", default="~/master_thesis_code/pvcnn/data/terrain/h5_nosplit/", 
              help="Directory where predictions are located in subfolders [default %default]",
              dest = "pred_dir"),
  make_option(c("-w", "--width"), type = "double", default = c(1,0.5,0.25),
              help = "Width multiplier whose predictions should be evaluated [default %default]",
              dest = "width"),
  make_option(c("-e", "--entropy_limit"), type = "double", default = 1.2,
              help = "Maximum entropy for points to be considered in grid classification [default %default]",
              dest = "entropy_limit"),
  make_option(c("-a", "--area"), type = "character", default = "Area_3",
              help = "Area for which to do predictions [default %default]",
              dest = "area")
)
opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("patchwork"))
source("true_colour_codes.R")
source("little_helpers.R")
true_labels <- create_true_labels()

na_colour <- "grey80"
theme_set(theme_grey()+  
            theme(panel.background = element_rect(fill = na_colour),
                  panel.border = element_rect(fill = "transparent", colour = "black", size = 1.5),
                  legend.position = "none", legend.title = element_blank(),
                  axis.title = element_blank(), axis.text = element_blank(),
                  axis.ticks = element_blank(), panel.grid = element_blank()))
no_xexpand <- scale_x_continuous(expand = c(0,0)) 
no_yexpand <- scale_y_continuous(expand = c(0,0)) 

if(is.null(opts$area)){
  areas      <- dir(opts$pred_dir, pattern = "Area_[0-9]{1,3}", full.names = TRUE)
} else {
  areas      <- paste0(opts$pred_dir, opts$area)
}

for(width in opts$width){
  width_pfx  <- paste0("c",gsub("\\.", "p", width))
  
  for(ent_lim in opts$entropy){
    entropy_pfx<- gsub("\\.", "p", ent_lim)
    
    area_pb      <- txtProgressBar(0, length(areas),width = 20, style = 3)
    area_count   <- 0
    
    for(area in areas){
      area_text  <- str_extract(area, "Area_[0-9]{1,3}")
      area_name  <- area_names[area_id == area_text, area]
      pred_path  <- paste0(area, "/eval/")
      
      load(paste0(pred_path, width_pfx, ".Rdata"))
      
      # Creation of the plots
      # Plot of predictions
      pred_plot <-   ggplot(area_grouped, 
                            aes_string(x = "Xpix", y = "Ypix", 
                                       fill = as.character(paste0("pred_cat",entropy_pfx)))) +
        geom_tile()  +
        scale_fill_manual(values = setNames(as.character(true_labels$colour),
                                            true_labels$ID),
                          label = setNames(as.character(true_labels$category),
                                           true_labels$ID),aesthetics =c("colour", "fill")) +
        labs(title = paste("Predictions for", area_name,", c=", width, ", max entropy:", ent_lim)) +
        no_xexpand + no_yexpand 
      
      # Ground truth plot
      true_plot<- ggplot(area_grouped,
                         aes(x = Xpix, y = Ypix, fill = as.character(category))) +
        geom_tile()  +
        scale_fill_manual(values = setNames(as.character(true_labels$colour),
                                            true_labels$ID),
                          label = setNames(as.character(true_labels$category),
                                           true_labels$ID),aesthetics =c("colour", "fill")) +
        labs(title = paste("Ground Truth")) +
        no_xexpand + no_yexpand  
      
      # Entropy plot
      ents_plot <-  ggplot(area_grouped, aes(x = Xpix, y = Ypix, fill = mean_entropy)) +
        geom_tile()  +
        scale_fill_viridis_c(direction = -1, option = "D", na.value = "transparent" ,
                             breaks = seq(0, 2, 0.5), limits = c(0, 2), 
                             aesthetics =c("colour", "fill"))  +
        labs(title = "Average entropy per grid unit") +
        no_xexpand + no_yexpand + theme(legend.position = "none")
      
      # Plot Settings
      plot_ratio <- (max(area_grouped$Ypix) - min(area_grouped$Ypix))/(max(area_grouped$Xpix) - min(area_grouped$Xpix))
      
      plot_width <- 6.5
      plot_height<- 1.5 * plot_width 
      
      if(plot_ratio > 1){ 
        plot_width <- plot_width*1.5/plot_ratio 
        plot_matrix <- c("AAB\nAAC")
      } else {
        plot_height <- plot_height*plot_ratio 
        plot_matrix <- c("AA\nAA\nBC")
      }
      
      
      # Put the plots together and save
      plot_sheet <- pred_plot + true_plot + ents_plot + plot_layout(desig = plot_matrix)
      
      ggsave(filename = paste0(opts$pred_dir,"plots/", area_name, "_", width_pfx, "_e", entropy_pfx,"_plot.png"), 
             plot = plot_sheet, width = plot_width, height = plot_height, units = "in", dpi = 150)
      area_count <- area_count + 1
      setTxtProgressBar(area_pb, area_count)
    }
  }
}