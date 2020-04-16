import torch.optim as optim

from models.terrain import PVCNN
from utils.config import Config, configs

# model
configs.model = Config(PVCNN)
configs.model.num_classes = configs.data.num_classes
configs.model.extra_feature_channels = 7  # ändrat pga intesitet tillagt
configs.dataset.num_points = 4096

configs.train.optimizer.weight_decay = 1e-5
# train: scheduler
configs.train.scheduler = Config(optim.lr_scheduler.CosineAnnealingLR)
configs.train.scheduler.T_max = configs.train.num_epochs
configs.train.batch_size = 16
configs.evaluate.batch_size = 6
