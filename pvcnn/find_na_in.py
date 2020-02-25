import h5py
import numpy as np
import os

folder = "/home/guslun/master_thesis_code/pvcnn/data/terrain/h5_output"
areas = sorted([area for area in os.listdir(folder)])
for area in areas:
    area_folder = os.path.join(folder, area)
    print(area)
    segments = sorted([segment for segment in os.listdir(area_folder)])

    for segment in segments:
        seg_folder = os.path.join(area_folder, segment)
        h5files = [h5file for h5file in os.listdir(seg_folder)]
        h5files.remove(".dataset")
        for file in h5files:
            filepath = os.path.join(seg_folder, file)
            h5file = h5py.File(filepath, 'r')
            h5nas = [np.argwhere(np.isnan(h5file[key][()])).shape[0] for key in h5file.keys()]
            if np.sum(h5nas) > 0:
                print(filepath)


