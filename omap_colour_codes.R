# Not to be rerun!
stop("Don't! Will destroy class mapping file, only for documentation")
# Copied from main file
source("png_map_reader.R")
map_dir    <- "~/kartor/kvarn_liten"
map        <- png_map_reader(paste0(map_dir, "/omap_ren_raw"))
# End of copyting
maplevels <- data.table(col_code = unique(map$colour), name = NA_character_)
setkey(maplevels, col_code)
  #        col_code name
  # 1:     #C1F9BA <NA>
  # 2:     #848074 <NA>
  # 3:     #FFDD9B <NA>
  # 4:     #FFFFFF <NA>
  # 5:     #0A0906 <NA>
  # 6:     #28D2FE <NA>
  # 7:     #FF08FF <NA>
  # 8:     #FFBA37 <NA>

ggplot(maplevels, aes(y=1:length(col_code), x=0, width = length(col_code), height = 1)) +
  geom_tile(fill = maplevels$col_code, col = "black", size = rel(2)) +
  geom_label(aes(label = col_code), size = rel(5)) +
  theme_void()

maplevels[, category := c("Forest", "Open_area", )]


