import os
import numpy as np
from shutil import copyfile

#area_id = "9"
h5_path = "/media/guslun/storage/bra_att_ha/h5_notsplit/"
label_path = "/media/guslun/storage/bra_att_ha/area_output2_notsplit/"

for area_no in  range(1,15):
    area_id = str(area_no)
    h5_folder = os.path.join(h5_path, "Area_" + area_id)
    label_folder = os.path.join(label_path, "Area_" + area_id)
    print(h5_folder)
    print(label_folder)
    h5_segments = [segment for segment in os.listdir(h5_folder)]
    label_segments = [segment for segment in os.listdir(label_folder)]

    common_segments = np.intersect1d(h5_segments, label_segments)
    print("Segments in only one set: ")
    print(np.setdiff1d(h5_segments, label_segments))

    for segment in common_segments:
        print("Copying labels for: " + segment)
        source = os.path.join(label_folder, segment, "xyzrgb.npy")
        target = os.path.join(h5_folder, segment, "xyzrgb.npy")
        copyfile(source, target)
