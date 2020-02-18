seg_list_writer_factory <- function(output_dir, 
                                    data_list,
                                    seg_grp,
                                    data_vars = c("X","Y", "Z", "Intensity", "R", "G", "B"), 
                                    label_vars = "category"){
  # Function for writing xyzdata.npy and labels.npy for each room in each area
  suppressPackageStartupMessages(require(RcppCNPy))
  seg_list_writer <- function(segment_id){
    #browser()
    seg_dir <- paste0(output_dir, "/segment_", seg_grp*10+segment_id, "/")
    if(!dir.exists(seg_dir)) { dir.create(seg_dir) }
    npySave(paste0(seg_dir, "xyzrgb.npy"), as.matrix(data_list[[segment_id]][, ..data_vars]))
    npySave(paste0(seg_dir, "label.npy"), as.matrix(data_list[[segment_id]][, ..label_vars]))
    file.create(paste0(seg_dir, ".labels"))
    #write(paste0("Segment ", segment_id, " saved."), stdout())
    }
}
