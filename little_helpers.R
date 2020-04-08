point_dist_plot <- function(data, column,true_labels, save_formats = c("png", "pdf")){
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

#data = area_stats_dt[area_names, nomatch=0, on = c("area_name == area_id"), ][,area_name:=NULL]
map_dist_plot <- function(data,  true_labels, addname = "", subtitle ="", save_formats = c("pdf")){
    # Little helper to plot the distribution of terrain points from omap totals
    suppressPackageStartupMessages(require(ggplot2))
    label_vec   <- setNames(true_labels$colour, true_labels$category)
    #data <- area_stats[area != "total"]
    data <- melt(data, id.vars = "area" )[variable %in% names(label_vec),]
    nudgey <- max(data$value) / 20
    # Set theme outside fo clarity
    theme_set(theme_bw(base_family = "serif", base_size = 11) +
                  theme(axis.title.x = element_text(angle = 0, hjust = 1),
                        # axis.title.y = element_text(angle = 90, vjust = 1, hjust = 1, 
                        #                             margin = margin(0,-20,0,0)),
                        legend.position = "none",
                        axis.text.x = element_text(angle = 30, hjust = 1),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        panel.grid.minor.y = element_blank(),
                        strip.text = element_text(face="bold"),
                        strip.placement = "outside",
                        strip.background = element_rect(fill = "transparent",colour = "transparent")
                  ))
    
    ggplot(data, aes(x = variable, y = value, fill = variable)) + 
        geom_col(col = "black", width = 0.5) +
        geom_text(aes(label = round(value,2)), nudge_y = nudgey, size = 2.5, family = "serif") +
        scale_fill_manual("", values = label_vec) +
        scale_y_continuous("Proportion",expand = c(0,0), limits = c(0,0.7)) +
        scale_x_discrete(limits = names(label_vec)) + 
        labs(title = "Distribution of terrain types per split",
             x = "", subtitle = subtitle) +
        facet_wrap(.~area, ncol = 3)
    
            # Save specified file formats
    lapply(save_formats, function(format){
        invisible(ggsave(paste0(addname ,"_pixel_dist.", format), 
                         height = 11*0.3, width = 8.5, units = "in"))
    })
}



timing_writer <- function(init_time, end_time, seg_grp, end_grp, nobs){
    total_time <- difftime(end_time, init_time, units = "mins")
    #record_time<- as.numeric(total_time) * 60 / (nobs/1000)
    time_record<- nobs / as.numeric(total_time)
    alarm()
    write(paste("Segment set", seg_grp,"/", end_grp, "completed in", round(total_time, 2),
                "minutes. Points per minute:", round(time_record, 1)),
          stdout())
}

map_dt_plot <- function(map, x = "pX", y = "pY", colour = "colour", dirname = ""){
    # Helper for plotting the map used
    oldkey <- key(map)
    setkeyv(map, c(x,y))
    colmat <- col2rgb(unlist(map[, ..colour]))/255
    # Using coordinates here will require massive memory... hence pX,pY
    map_array <- array(dim = c(max(map[,..y]-min(map[,..y])+1), 
                               max(map[,..x]-min(map[,..x])+1),
                               3))
    map_array[,,1] <- colmat[1, ]
    map_array[,,2] <- colmat[2, ]
    map_array[,,3] <- colmat[3, ]
    setkeyv(map, oldkey)
    png::writePNG(image = map_array, target = paste0(dirname, "_", colour, "_map.png"), dpi = 150)
}

confusion_matrix_xtable <- function(model = NULL, test_area = NULL, cm_path=NULL){
    # Helper to print the conf-matrix outout of eval in a slightly prettier way
    # Requires the following in the LATEX preamble
    # \usepackage{booktabs}
    # \usepackage{xcolor}
    # \usepackage{changepage}
    #
    # \definecolor{truepositive}{HTML}{DBBEE8}
    # \definecolor{majorerror}{HTML}{5B127A}
    # 
    # \newenvironment{widetable}
    # {\begin{table}[ht] \begin{adjustwidth}{-1in}{-1in} }
    # {\end{adjustwidth} \end{table}}
    # 
    if(is.null(cm_path)){
        cm_path <- paste0("pvcnn/runs/[configs+terrain.",model,".",test_area,"]/best.conf_mat.npy")
    } 
    if(is.null(model)){ model <- "unknown" }
    if(is.null(test_area)){ test_area <- "unknown" }
    

    suppressPackageStartupMessages(library(RcppCNPy))
    suppressPackageStartupMessages(library(xtable))
    source("true_colour_codes.R")
    
    true_labels <- create_true_labels()$category
    conf_mat     <- npyLoad(cm_path)
    
    rsums        <- rowSums(conf_mat)
    csums        <- colSums(conf_mat)
    truepos      <- diag(conf_mat)
    classacc     <- round(truepos / csums,2)
    totalacc     <- round(sum(truepos)/sum(conf_mat),2)
    classiou     <- round(truepos / (rsums + csums - truepos),2)
    rlabs        <- c(true_labels, "Total", "Accuracy", "IoU")
    clabs        <- c(true_labels)
    
    char_mat     <- rbind(conf_mat, as.character(csums), as.character(classacc), as.character(classiou))

    for(col in seq(1,length(true_labels))){
        char_mat[col,col] <- paste0("\\colorbox{truepositive}{", char_mat[col,col], "}")
        gt_truepos <- which(conf_mat[, col] > diag(conf_mat)[col])
        char_mat[gt_truepos, col] <- paste0("\\textcolor{majorerror}{\\textbf{", char_mat[gt_truepos,col], "}}")
    }
    dimnames(char_mat) <- list(rlabs, clabs)
    shortcaption <- paste("Confusion matrix for", model, "evaluated on", test_area)
    longcaption  <- paste("\\centering ", shortcaption,".\nAccuracy:", totalacc, ", IoU:", round(mean(classiou),2))
    cm_label     <- paste0("tab:res_",model, "_", test_area)
    
    print.xtable(xtable(char_mat, digits = 0, 
                        caption = c(longcaption, shortcaption), 
                        label = cm_label, align = c("l",rep("r", ncol(char_mat)))),
                 booktabs = TRUE, 
                 add.to.row = list(pos=list(8), command = c("\\midrule ")),
                 sanitize.text.function = function(x){x},
                 floating.environment = "widetable",
                 table.placement = NULL,
                 caption.placement = "top")
}

area_names <- data.table::data.table(area_id = paste0("Area_", 1:16), 
                         area = c("akerbo_nv","akerbo_o","akerbo_sv","grytstorp_nv",
                                  "grytstorp_v","linkoping_s","linkoping_sv",
                                  "linkoping_vidingsjo","prasttomta_nv","prasttomta_v",
                                  "sodero_mitt","sodero_nv","sodero_sv","valla",
                                  "test_set", "validation_set"))

split_stats_plot <- function(split_stats){
    # Helper to plot the split into test/validation in test_split.R
    split_stats <- data.table(split_stats)[area_names, on = "area_id"]
    split_stats[, train := total -(tests + valid)][, area_id := NULL][, total := NULL]
    setcolorder(split_stats, c("area", "train", "tests", "valid"))
    split_stats <- melt(split_stats, id.vars = "area")
    fills <- c(train = "mediumorchid4",tests ="mediumorchid3", valid ="orchid1")
    ggplot(split_stats, aes(x=area, y = value, fill = variable)) + 
        geom_col(position = position_dodge2(padding=.3), col = "grey20", width = .7) +
        scale_fill_manual("", values = fills) + 
        theme_minimal(base_family = "serif", base_size = 11) +
        theme(legend.position = "right", 
              axis.text.x = element_text(angle = 30, hjust = 1, size = 11),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x ="", y = "No. of segments", title = "No of segments per area and split") +
        scale_y_continuous(breaks = seq(0, 200,50), minor_breaks = seq(0, 50, 10)) 
        
    ggsave("test_split.pdf", units= "in", height = 3.5, width= 8.2)
}
