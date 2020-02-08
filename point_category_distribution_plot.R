library(ggplot2)
source("omap_colour_codes.R")

load("~/kartor/kvarn_liten/las_lookup_2.Rdata")
true_labels <- create_true_labels()
label_vec   <- setNames(true_labels$colour, true_labels$category)

theme_set(theme_minimal(base_family = "serif", base_size = 12) +
              theme(axis.title.x = element_text(angle = 0, hjust = 1),
                    axis.title.y = element_text(angle = 0, vjust = 1, hjust = 1, 
                                                margin = margin(0,-20,0,0))))

ggplot(las_omap_join, aes(x=category, fill = category)) + 
    geom_bar(col = "black", width = 0.5) + 
    scale_fill_manual("", values = label_vec) +
    scale_y_continuous("No. of points",expand = c(0,0)) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1, size = 11),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          ) +
    labs(title = "Number of points per terrain category\n",
         x = "Terrain")

ggsave("point_category_distribution.pdf", height = 9, width = 20, units = "cm")
ggsave("point_category_distribution.png", height = 9, width = 20, units = "cm")
