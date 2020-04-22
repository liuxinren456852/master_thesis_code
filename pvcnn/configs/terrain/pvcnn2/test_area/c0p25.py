from utils.config import configs

configs.model.width_multiplier = 0.25
configs.train.weight_type = 1

configs.train.batch_size = 96
configs.evaluate.batch_size = 16

configs.dataset.num_points = 8192*2
