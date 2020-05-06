PVCNNc1iou <- c(0.29333,0.29512,0.38878,0.57021,0.3454,0.09505,0.40611,0.50183);
PVCNNc1acc <- c(0.87264,0.73276,0.78103,0.67462,0.91016,0.28645,0.65862,0.53238);
PVCNNc1prr <- c(0.68320987654321,0.345576400432729,0.695649573331207,0.668770861392516,0.724157146837559,0.527259533228774,0.667528776216369,0.590510815038567,0.610621208190034)
PVCNNc05iou <- c(0.29667,0.28982,0.37997,0.56199,0.30534,0.07861,0.38285,0.50046);
PVCNNc05acc <- c(0.86916,0.71092,0.79227,0.70073,0.90485,0.2023,0.6415,0.53104);
PVCNNc05prr <- c(0.684641975308642,0.346209662524077,0.720212935640801,0.679123967910054,0.723321259403734,0.532058334698846,0.69323317562319,0.624547170829063,0.634843524872065)
PVCNNc025iou <- c(0.30539,0.26114,0.38001,0.56946,0.33054,0.01415,0.38467,0.60644);
PVCNNc025acc <- c(0.87145,0.69181,0.80994,0.66947,0.91551,0.01711,0.60629,0.65859);
PVCNNc025prr <- c(0.658814814814815,0.324230190770205,0.684604035409522,0.627434561105581,0.713987183059348,0.441840867061495,0.604062213120542,0.521428340255677,0.553379299684786)
multinomialregressioniou <- c(0.00071,0.33428,0,0.42237,0.27444,3e-05,0.00017,0.61336);
multinomialregressionacc <- c(0.00073,0.37734,0,0.60448,0.32902,3e-05,0.00017,0.94669);


model_comp_data <- rbind(
  melt(data.table(terrain = factor(create_true_labels()$category), vartype = "Accuracy",
                  multinomialregressionacc, PVCNNc025acc, PVCNNc05acc, PVCNNc1acc), 
       id.vars = c("terrain", "vartype"), variable.name = "Model"),
  melt(data.table(terrain = factor(create_true_labels()$category), vartype = "IoU",
                  multinomialregressioniou, PVCNNc025iou, PVCNNc05iou, PVCNNc1iou), 
       id.vars = c("terrain", "vartype"), variable.name = "Model"),
  melt(data.table(terrain = factor(c(create_true_labels()$category, "Total")), vartype = "prediction rate",
                  PVCNNc025prr, PVCNNc05prr, PVCNNc1prr), 
       id.vars = c("terrain", "vartype"), variable.name = "Model")
)[, Model := factor(gsub('.{3}$', '', Model))]

ggplot(model_comp_data, aes(x = terrain, y = value, group = Model, fill = Model)) + 
  geom_col(position = position_dodge(width = 0.8), col = "grey20", width = 0.6) + 
  theme_minimal(base_family = "serif", base_size = 15) +
  coord_flip()+
  facet_grid(cols = vars(vartype), ) +
  scale_fill_viridis_d(direction = -1, option = "A", 
                       labels = c("Multinomial regression", "PVCNN++, c=0.25", 
                                  "PVCNN++, c=0.5", "PVCNN++, c=1")) +
  scale_y_continuous("", limits = c(0,1), expand =c(0,0), breaks = seq(0,1,0.2)) + 
  scale_x_discrete(limits = c(rev(create_true_labels()$category), "Total")) +
  xlab("") + theme(legend.position = "bottom", panel.border = element_rect(fill = NA),
                   panel.grid.major.y = element_blank(),panel.spacing = unit(2, "lines"))


ggsave(filename = "model_comp.pdf", width = 30, height = 11, units = "cm")


for(metric in unique(model_comp_data$vartype)){
print(xtable(dcast(model_comp_data, vartype+ Model ~ terrain)[vartype == metric,  c("Model", true_labels$category, "Total"), with=FALSE], 
             label = paste0("model_comp_", metric), 
             caption = paste0("Comparison of model ",metric)),
      booktabs = TRUE, caption.placement = "top", floating.environment = "widetable",
      table.placement = NULL, include.rownames = FALSE)
  } 

