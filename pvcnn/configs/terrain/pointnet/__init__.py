import torch.optim as optim

from models.terrain import PointNet
from utils.config import Config, configs

# model
configs.model = Config(PointNet)
configs.model.num_classes = configs.data.num_classes
configs.model.extra_feature_channels = 7
configs.dataset.num_points = 8196



# configs.train.scheduler = Config(optim.lr_scheduler.StepLR)
# configs.train.scheduler.step_size = 5  # learning rate clip = 1e-5
configs.train.scheduler = Config(optim.lr_scheduler.MultiStepLR)
configs.train.scheduler.milestones = [5, 10, 15, 20, 25, 30, 35]
configs.train.scheduler.gamma = 0.5

configs.train.batch_size = 16
configs.evaluate.batch_size = 6