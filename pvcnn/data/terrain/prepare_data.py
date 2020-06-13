from typing import Any

import argparse
import math
import os
from datetime import datetime

import h5py
import numpy as np
import plyfile
from matplotlib import cm

from pandas import read_csv

def prepare_label(data_dir, output_dir):
    # Handled in R: las_data_prep.R

    object_dict = {
        'roads': 0,
        'water': 1,
        'marsh': 2,
        'openland': 3,
        'building': 4,
        'trail': 5,
        'medforest': 6,
        'forest': 7,
        'NA': 99
        }

    #tldf = read_csv(data_dir + "/true_labels.csv", names=["category", "id"])
    #object_dict = dict(zip(list(tldf.category), list(tldf.id)))

    for area in os.listdir(data_dir):
        path_area = os.path.join(data_dir, area)
        if not os.path.isdir(path_area):
            continue

        path_dir_rooms = os.listdir(path_area)
        for room in path_dir_rooms:
            path_annotations = os.path.join(data_dir, area, room, 'Annotations')
            if not os.path.isdir(path_annotations):
                continue

            print(path_annotations)
            path_prepare_label = os.path.join(output_dir, area, room)
            if os.path.exists(os.path.join(path_prepare_label, '.labels')):
                print(f'{path_prepare_label} already processed, skipping')
                continue

            xyz_room = np.zeros((1, 6))
            label_room = np.zeros((1, 1))
            # make store directories
            if not os.path.exists(path_prepare_label):
                os.makedirs(path_prepare_label)

            path_objects = os.listdir(path_annotations)
            for obj in path_objects:
                object_key = obj.split('_', 1)[0]
                try:
                    val = object_dict[object_key]
                except KeyError:
                    continue
                print(f'{room}/{obj[:-4]}')
                xyz_object_path = os.path.join(path_annotations, obj)
                try:
                    xyz_object = np.loadtxt(xyz_object_path)[:, :]  # (N,6) # läser in xyzrgb-filen
                except ValueError as e:
                    print(f'ERROR: cannot load {xyz_object_path}: {e}')
                    continue
                # bygger label out genom att läsa ala punkter från annotation dir och stacka klass-id lika många gånger
                # som det är punkter i prylen.
                label_object = np.tile(val, (xyz_object.shape[0], 1))  # (N,1)
                xyz_room = np.vstack((xyz_room, xyz_object)) # rbindar ihop alla punkter i objektet med föreg objekt.
                label_room = np.vstack((label_room, label_object)) #rbind:ar ihop objektets klass-vektor med föreg.klass

            xyz_room = np.delete(xyz_room, [0], 0)
            label_room = np.delete(label_room, [0], 0)

            np.save(path_prepare_label + '/xyzrgb.npy', xyz_room)
            np.save(path_prepare_label + '/label.npy', label_room)

            # Marker indicating we've processed this room
            open(os.path.join(path_prepare_label, '.labels'), 'w').close()


def main():
    default_data_dir = 'area_output2'
    default_output_dir = 'area_output2'
    default_h5output_dir = 'pvcnn/data/terrain/h5_2' #tillagt för att få ut h5filerna separat
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--data', dest='data_dir', default=default_data_dir,
                        help=f'Path to Terrain data (default is {default_data_dir})')
    parser.add_argument('-f', '--folder', dest='output_dir', default=default_output_dir,
                        help=f'Folder to write labels (default is {default_output_dir})')
    parser.add_argument('-o', '--h5folder', dest='h5output_dir', default=default_h5output_dir,
                        help=f'Folder to write h5-files (default is {default_h5output_dir})')
    parser.add_argument('--max_num_points', '-m', help='Max point number of each sample', type=int, default=4096) #ändrat för färre segment
    parser.add_argument('--block_size', '-b', help='Block size', type=float, default=20) # lower resolution
    parser.add_argument('--grid_size', '-g', help='Grid size', type=float, default=0.03)
    parser.add_argument('--save_ply', '-s', help='Convert .pts to .ply', action='store_true')
    parser.add_argument('--last_area', help='Highest area number to include', default=None)
    parser.add_argument('--max_segments', help='Highest number of segments to include', default=None)

    args = parser.parse_args()
    print(args)

    prepare_label(data_dir=args.data_dir, output_dir=args.output_dir)

    root = args.output_dir
    h5output = args.h5output_dir
    max_num_points = args.max_num_points

    batch_size = 2048
    data = np.zeros((batch_size, max_num_points, 10))  #dim 3 needs to be change to match input+norm(xyz)
    data_num = np.zeros(batch_size, dtype=np.int32)
    label = np.zeros(batch_size, dtype=np.int32)
    label_seg = np.zeros((batch_size, max_num_points), dtype=np.int32)
    indices_split_to_full = np.zeros((batch_size, max_num_points), dtype=np.int32)

    if not os.path.isdir(h5output):
        os.makedirs(h5output)

    ## Tar in .npy-filerna och spottar ut hd5-filerna.
    areas = os.listdir(args.data_dir)
    #areas.remove("true_labels.csv")
    if args.last_area is not None and int(args.last_area) <= len(areas)+1:
        last_area = int(args.last_area)+1
    else:
        last_area = len(areas)+1

    for area_idx in range(1, last_area): # Loopar genom areornas
        folder = os.path.join(root, 'Area_%d' % area_idx) # Sökväg till areans målmapp
        h5folder = os.path.join(h5output, 'Area_%d' % area_idx) # Tillagt för att få ut h5filerna nån annanstans
        if not os.path.isdir(h5folder):
            os.makedirs(h5folder)

        datasets = sorted([dataset for dataset in os.listdir(folder)])
        # Tillagt för att ta hand om R's kontrollfil
        if ".area" in datasets :
            datasets.remove(".area")

        #Tillagt för att skapa mindre test-set
        if args.max_segments is not None:
            max_segment = min(int(args.max_segments)+1, len(datasets))
            datasets = datasets[:max_segment]

        for dataset_idx, dataset in enumerate(datasets): # för varje rum...
            h5dataset_folder = os.path.join(h5folder, dataset)
            if not os.path.isdir(h5dataset_folder):
                os.makedirs(h5dataset_folder)
            dataset_marker = os.path.join(h5folder, dataset, '.dataset') # kollar om .dataset-filen finns och skippar rummet isåfall
            if os.path.exists(dataset_marker):
                print(f'{datetime.now()}-{folder}/{dataset} already processed, skipping')
                continue
            filename_data = os.path.join(folder, dataset, 'xyzrgb.npy') #
            print(f'{datetime.now()}-Loading {filename_data}...')
            # Modified according to PointNet convensions.
            xyzrgb = np.load(filename_data)
            if xyzrgb.shape[0]==0:
                print(f'{datetime.now()}-{folder}/{dataset} is empty, skipping')
                os.rmdir(h5dataset_folder)
                continue
            xyzrgb[:, 0:3] -= np.amin(xyzrgb, axis=0)[0:3] # Drar bort minsta xyz-värde

            filename_labels = os.path.join(folder, dataset, 'label.npy')
            print(f'{datetime.now()}-Loading {filename_labels}...')
            labels = np.load(filename_labels).astype(int).flatten()

            xyz, rgb = np.split(xyzrgb, [3], axis=-1)
            xyz_min = np.amin(xyz, axis=0, keepdims=True)
            xyz_max = np.amax(xyz, axis=0, keepdims=True)
            xyz_center = (xyz_min + xyz_max) / 2
            xyz_center[0][-1] = xyz_min[0][-1]
            # Remark: Don't do global alignment.
            # xyz = xyz - xyz_center
            # rgb = rgb / 255.0 # Behövs inte eftersom det hanteras av R
            max_room_x = np.max(xyz[:, 0])
            max_room_y = np.max(xyz[:, 1])
            max_room_z = np.max(xyz[:, 2])

            offsets = [('zero', 0.0), ('half', args.block_size / 2)]
            offset_name: Any
            for offset_name, offset in offsets:
                idx_h5 = 0
                idx = 0

                print(f'{datetime.now()}-Computing block id of {xyzrgb.shape[0]} points...')
                xyz_min = np.amin(xyz, axis=0, keepdims=True) - offset
                xyz_max = np.amax(xyz, axis=0, keepdims=True)
                block_size = (args.block_size, args.block_size, 2 * (xyz_max[0, -1] - xyz_min[0, -1]))
                # Note: Don't split over z axis.
                xyz_blocks = np.floor((xyz - xyz_min) / block_size).astype(np.int)

                print(f'{datetime.now()}-Collecting points belong to each block...')
                blocks, point_block_indices, block_point_counts = np.unique(xyz_blocks, return_inverse=True,
                                                                            return_counts=True, axis=0)
                block_point_indices = np.split(np.argsort(point_block_indices), np.cumsum(block_point_counts[:-1]))
                print(f'{datetime.now()}-{dataset} is split into {blocks.shape[0]} blocks.')

                block_to_block_idx_map = dict()
                for block_idx in range(blocks.shape[0]):
                    block = (blocks[block_idx][0], blocks[block_idx][1])
                    block_to_block_idx_map[(block[0], block[1])] = block_idx

                # merge small blocks into one of their big neighbors
                block_point_count_threshold = max_num_points/10
                nbr_block_offsets = [(0, 1), (1, 0), (0, -1), (-1, 0), (-1, 1), (1, 1), (1, -1), (-1, -1)]
                block_merge_count = 0
                for block_idx in range(blocks.shape[0]):
                    if block_point_counts[block_idx] >= block_point_count_threshold:
                        continue

                    block = (blocks[block_idx][0], blocks[block_idx][1])
                    for x, y in nbr_block_offsets:
                        nbr_block = (block[0] + x, block[1] + y)
                        if nbr_block not in block_to_block_idx_map:
                            continue

                        nbr_block_idx = block_to_block_idx_map[nbr_block]
                        if block_point_counts[nbr_block_idx] < block_point_count_threshold:
                            continue

                        block_point_indices[nbr_block_idx] = np.concatenate(
                            [block_point_indices[nbr_block_idx], block_point_indices[block_idx]], axis=-1)
                        block_point_indices[block_idx] = np.array([], dtype=np.int)
                        block_merge_count = block_merge_count + 1
                        break
                print(f'{datetime.now()}-{block_merge_count} of {blocks.shape[0]} blocks are merged.')

                idx_last_non_empty_block = 0
                for block_idx in reversed(range(blocks.shape[0])):
                    if block_point_indices[block_idx].shape[0] != 0:
                        idx_last_non_empty_block = block_idx
                        break

                # uniformly sample each block
                for block_idx in range(idx_last_non_empty_block + 1):
                    point_indices = block_point_indices[block_idx]
                    if point_indices.shape[0] == 0:
                        continue
                    block_points = xyz[point_indices]
                    block_min = np.amin(block_points, axis=0, keepdims=True)
                    xyz_grids = np.floor((block_points - block_min) / args.grid_size).astype(np.int)
                    grids, point_grid_indices, grid_point_counts = np.unique(xyz_grids, return_inverse=True,
                                                                             return_counts=True, axis=0)
                    grid_point_indices = np.split(np.argsort(point_grid_indices), np.cumsum(grid_point_counts[:-1]))
                    grid_point_count_avg = int(np.average(grid_point_counts))
                    point_indices_repeated = []
                    for grid_idx in range(grids.shape[0]):
                        point_indices_in_block = grid_point_indices[grid_idx]
                        repeat_num = math.ceil(grid_point_count_avg / point_indices_in_block.shape[0])
                        if repeat_num > 1:
                            point_indices_in_block = np.repeat(point_indices_in_block, repeat_num)
                            np.random.shuffle(point_indices_in_block)
                            point_indices_in_block = point_indices_in_block[:grid_point_count_avg]
                        point_indices_repeated.extend(list(point_indices[point_indices_in_block]))
                    block_point_indices[block_idx] = np.array(point_indices_repeated)
                    block_point_counts[block_idx] = len(point_indices_repeated)

                for block_idx in range(idx_last_non_empty_block + 1):
                    point_indices = block_point_indices[block_idx]
                    if point_indices.shape[0] == 0:
                        continue

                    block_point_num = point_indices.shape[0]
                    block_split_num = int(math.ceil(block_point_num * 1.0 / max_num_points))
                    point_num_avg = int(math.ceil(block_point_num * 1.0 / block_split_num))
                    point_nums = [point_num_avg] * block_split_num
                    point_nums[-1] = block_point_num - (point_num_avg * (block_split_num - 1)) #Sista täcker hela range:n
                    starts = [0] + list(np.cumsum(point_nums))

                    # Modified following convensions of PointNet.
                    np.random.shuffle(point_indices)
                    block_points = xyz[point_indices]
                    block_rgb = rgb[point_indices]
                    block_labels = labels[point_indices]
                    x, y, z = np.split(block_points, (1, 2), axis=-1)
                    # Normaliserar så att xyz hamnar i [0,1]
                    norm_x = x / max_room_x
                    norm_y = y / max_room_y
                    norm_z = z / max_room_z

                    minx = np.min(x)
                    miny = np.min(y)
                    x = x - (minx + args.block_size / 2)
                    y = y - (miny + args.block_size / 2)

                    block_xyzrgb = np.concatenate([x, y, z, block_rgb, norm_x, norm_y, norm_z], axis=-1)
                    for block_split_idx in range(block_split_num):
                        start = starts[block_split_idx]
                        point_num = point_nums[block_split_idx]
                        end = start + point_num
                        idx_in_batch = idx % batch_size
                        data[idx_in_batch, 0:point_num, ...] = block_xyzrgb[start:end, :]
                        data_num[idx_in_batch] = point_num
                        label[idx_in_batch] = dataset_idx  # won't be used...
                        label_seg[idx_in_batch, 0:point_num] = block_labels[start:end]
                        indices_split_to_full[idx_in_batch, 0:point_num] = point_indices[start:end]

                        if ((idx + 1) % batch_size == 0) or \
                                (block_idx == idx_last_non_empty_block and block_split_idx == block_split_num - 1):
                            item_num = idx_in_batch + 1
                            filename_h5: Any = os.path.join(h5folder, dataset, f'{offset_name}_{idx_h5:d}.h5')
                            print(f'{datetime.now()}-Saving {filename_h5}...')

                            file = h5py.File(filename_h5, 'w')
                            file.create_dataset('data', data=data[0:item_num, ...])
                            file.create_dataset('data_num', data=data_num[0:item_num, ...])
                            file.create_dataset('label', data=label[0:item_num, ...])
                            file.create_dataset('label_seg', data=label_seg[0:item_num, ...])
                            file.create_dataset('indices_split_to_full', data=indices_split_to_full[0:item_num, ...])
                            file.close()

                            if args.save_ply: #Sparar polygonformat av saker...
                                print(f'{datetime.now()}-Saving ply of {filename_h5}...')
                                filepath_label_ply = os.path.join(folder, dataset, 'ply_label',
                                                                  f'label_{offset_name}_{idx_h5:d}')
                                save_ply_property_batch(data[0:item_num, :, 0:3], label_seg[0:item_num, ...],
                                                        filepath_label_ply, data_num[0:item_num, ...], 14)

                                filepath_rgb_ply = os.path.join(folder, dataset, 'ply_rgb',
                                                                f'rgb_{offset_name}_{idx_h5:d}')
                                save_ply_color_batch(data[0:item_num, :, 0:3], data[0:item_num, :, 3:6] * 255,
                                                     filepath_rgb_ply, data_num[0:item_num, ...])

                            idx_h5 = idx_h5 + 1
                        idx = idx + 1

            # Marker indicating we've processed this dataset
            datamarker = open(dataset_marker, 'w')
            datamarker.write(str(xyzrgb.shape[0]))
            datamarker.close()
    print(f'{datetime.now()}-Done.')


def save_ply(points, filename, colors=None, normals=None):
    vertex = np.core.records.fromarrays(points.transpose(), names='x, y, z', formats='f4, f4, f4')
    n = len(vertex)
    desc = vertex.dtype.descr

    if normals is not None:
        vertex_normal = np.core.records.fromarrays(normals.transpose(), names='nx, ny, nz', formats='f4, f4, f4')
        assert len(vertex_normal) == n
        desc = desc + vertex_normal.dtype.descr

    if colors is not None:
        vertex_color = np.core.records.fromarrays(colors.transpose() * 255, names='red, green, blue',
                                                  formats='u1, u1, u1')
        assert len(vertex_color) == n
        desc = desc + vertex_color.dtype.descr

    vertex_all = np.empty(n, dtype=desc)

    for prop in vertex.dtype.names:
        vertex_all[prop] = vertex[prop]

    if normals is not None:
        for prop in vertex_normal.dtype.names:
            vertex_all[prop] = vertex_normal[prop]

    if colors is not None:
        for prop in vertex_color.dtype.names:
            vertex_all[prop] = vertex_color[prop]

    ply = plyfile.PlyData([plyfile.PlyElement.describe(vertex_all, 'vertex')], text=False)
    if not os.path.exists(os.path.dirname(filename)):
        os.makedirs(os.path.dirname(filename))
    ply.write(filename)


def save_ply_property(points, property, property_max, filename, cmap_name='tab20'):
    point_num = points.shape[0]
    colors = np.full(points.shape, 0.5)
    cmap = cm.get_cmap(cmap_name)
    for point_idx in range(point_num):
        if property[point_idx] == 0:
            colors[point_idx] = np.array([0, 0, 0])
        else:
            colors[point_idx] = cmap(property[point_idx] / property_max)[:3]
    save_ply(points, filename, colors)


def save_ply_color_batch(points_batch, colors_batch, file_path, points_num=None):
    batch_size = points_batch.shape[0]
    if not isinstance(file_path, (list, tuple)):
        basename = os.path.splitext(file_path)[0]
        ext = '.ply'
    for batch_idx in range(batch_size):
        point_num = points_batch.shape[1] if points_num is None else points_num[batch_idx]
        if isinstance(file_path, (list, tuple)):
            save_ply(points_batch[batch_idx][:point_num], file_path[batch_idx], colors_batch[batch_idx][:point_num])
        else:
            save_ply(points_batch[batch_idx][:point_num], f'{basename}_{batch_idx:04d}{ext}',
                     colors_batch[batch_idx][:point_num])


def save_ply_property_batch(points_batch, property_batch, file_path, points_num=None, property_max=None,
                            cmap_name='tab20'):
    batch_size = points_batch.shape[0]
    if not isinstance(file_path, (list, tuple)):
        basename = os.path.splitext(file_path)[0]
        ext = '.ply'
    property_max = np.max(property_batch) if property_max is None else property_max
    for batch_idx in range(batch_size):
        point_num = points_batch.shape[1] if points_num is None else points_num[batch_idx]
        if isinstance(file_path, (list, tuple)):
            save_ply_property(points_batch[batch_idx][:point_num], property_batch[batch_idx][:point_num],
                              property_max, file_path[batch_idx], cmap_name)
        else:
            save_ply_property(points_batch[batch_idx][:point_num], property_batch[batch_idx][:point_num],
                              property_max, f'{basename}_{batch_idx:04d}{ext}', cmap_name)


if __name__ == '__main__':
    main()
