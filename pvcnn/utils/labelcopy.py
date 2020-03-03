import os
import numpy as np
from shutil import copyfile

area_id = "9"
h5_path = "../pvcnn/data/terrain/h5_output"
label_path = "../area_output"

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
    source = os.path.join(label_folder, segment, "label.npy")
    target = os.path.join(h5_folder, segment, "label.npy")
    copyfile(source, target)
