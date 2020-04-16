import torch.nn as nn
import torch.optim as optim

from datasets.terrain import TERRAIN
from meters.terrain import MeterTERRAIN
from evaluate.terrain.eval import evaluate
from predict.terrain.pred import predict
from utils.config import Config, configs

configs.data.num_classes = 8

# dataset configs
configs.dataset = Config(TERRAIN)
configs.dataset.root = 'data/terrain/h5_output'
configs.dataset.with_normalized_coords = True
# configs.dataset.num_points = 2048
# configs.dataset.holdout_area = 5

# evaluate configs
configs.evaluate = Config()
configs.evaluate.fn = evaluate
configs.evaluate.num_votes = 10
# configs.evaluate.batch_size = 16 # moved to the models instead
configs.evaluate.dataset = Config(split='test')

# predict configs
configs.predict = Config()
configs.predict.fn = predict
configs.predict.root = 'data/terrain/predict'
configs.predict.num_votes = 1
configs.predict.dataset = Config(split='test')
configs.predict.batch_size = 1

# train configs
configs.train = Config()
configs.train.num_epochs = 4
# configs.train.batch_size = 16 moved to the models instead

# train: meters
configs.train.meters = Config()
configs.train.meters['acc/iou_{}'] = Config(MeterTERRAIN, metric='iou', num_classes=configs.data.num_classes)
configs.train.meters['acc/acc_{}'] = Config(MeterTERRAIN, metric='overall', num_classes=configs.data.num_classes)

# train: metric for save best checkpoint
configs.train.metric = 'acc/iou_test'

# train: criterion
configs.train.criterion = Config(nn.CrossEntropyLoss)

# train: optimizer
configs.train.optimizer = Config(optim.Adam)
configs.train.optimizer.lr = 1e-3
