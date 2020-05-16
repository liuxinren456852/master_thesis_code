
###### Entropy Limits #####
all_stats <- data.table(matrix(nrow = 0, ncol = 7))
setnames(all_stats, c("width", "entropy", "pred_iou", "total_iou", "predict_rate", "pred_acc", "total_acc"))
for(width in opts$width){
  prefix <- paste0("c",gsub("\\.","p",width))
  for(ent_lim in  opts$entropy_limit){
    entropy_limit <- ent_lim
    ent_pfx<-paste0("el",gsub("\\.","p",entropy_limit))
    load(paste0(opts$pred_dir, prefix,"_",ent_pfx,"_grid_eval_stats.RData"))
    conf_mat_total <- Reduce(`+`, conf_mats)
    unpredicted_dt <- rbindlist(unpredicted)[order(category), .(unknowns = sum(unpredicted)), by = "category"]
    unpredicted_dt[, `:=`(predicted = colSums(conf_mat_total), category = true_labels$category[category], correct = diag(conf_mat_total),
                          pred_iou = diag(conf_mat_total)/(colSums(conf_mat_total)+rowSums(conf_mat_total)-diag(conf_mat_total)),
                          total_iou = diag(conf_mat_total)/(colSums(conf_mat_total)+rowSums(conf_mat_total)-diag(conf_mat_total)+unknowns))]
    
    totals <- data.table(category  = "Overall",                 unknowns  = sum(unpredicted_dt[, 2]), 
                         predicted = sum(unpredicted_dt[, 3]),  correct   = sum(unpredicted_dt[,4]),
                         pred_iou  = sum(unpredicted_dt[, 5])/nrow(unpredicted_dt), 
                         total_iou = sum(unpredicted_dt[, 6])/nrow(unpredicted_dt))
    unpredicted_dt <- rbind(unpredicted_dt, totals)
    unpredicted_dt[, `:=`(predict_rate = predicted/(unknowns+predicted), pred_acc = correct/predicted, total_acc = correct/(predicted+unknowns))]
    all_stats <- rbind(all_stats, data.table(width = prefix, entropy = ent_lim, unpredicted_dt[category == "Overall", 5:9]))
    print(paste0("PVCNN",gsub("p","",prefix),"prr <- c(",paste0(unpredicted_dt$predict_rate, collapse = ","),")"))
    confusion_matrix_xtable(paste0("PVCNN++, ", prefix), "validation data grid units", conf_mat = conf_mat_total)
    # confusion_matrix_xtable(paste0("PVCNN++, c=", opts$width), "validation data points",
    #                         cm_path = paste0("pvcnn/runs/terrain.pvcnn2.test_area.",prefix,"/best_area_16.conf_mat.npy"))
  }
}
xtable(all_stats[, c(1,2,5,3,4,6,7)], digits = c(0,0,1,rep(3,5)))


###### Logistic regression ######
width <-0.5;  prefix <- paste0("c", gsub("\\.","p",width)); 
ent_lim <- 1.2; ent_pfx <- paste0("el",gsub("\\.","p",ent_lim));
load(paste0(opts$pred_dir, prefix,"_",ent_pfx,"_grid_eval_stats.RData"))

full_eval <- rbindlist(eval_list)[, dens := exp(log_dens)][, std_dens := (dens-mean(dens, na.rm =TRUE))/sd(dens, na.rm =TRUE), by = category][, hdp :="Low"]
setkey(full_eval, Xpix, Ypix)

for(rowno in seq(1, nrow(intervalsbind))){
  bounds <- unlist(intervalsbind[rowno,])*round_mult
  full_eval[Xpix %between% bounds, hdp := "High"]
  full_eval[Ypix %between% bounds, hdp := "High"]
}

full_eval[, correct:=factor(pred_cat==category, levels =c(FALSE, TRUE))]
full_eval[,hdp := factor(hdp,levels = c("Low", "High"))]
eval_glm <- glm(correct ~category+hdp+hdp*category, data = full_eval, family=binomial(link = "logit"))
xtable::xtable(broom::tidy(eval_glm) )
broom::tidy(anova(eval_glm, test="Chisq"))

aaa <- full_eval[, .(correct = mean(pred_cat == category, na.rm=TRUE), count =.N), by=.(hdp, category)]
ggplot(aaa, aes(x=category, y=correct,group=hdp, fill = hdp, label = count, col=hdp)) +
  geom_col(position = position_dodge(.9),col="grey30", width = .8)+
  geom_text(aes(y = 0.07), position = position_dodge(.9), fontface ="bold",
            family ="serif", size = 2.85, show.legend=FALSE ) +
  theme_minimal(base_family="serif") +
  scale_x_discrete(labels=true_labels$category) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits =c(0,1)) +
  scale_fill_viridis_d(begin = 1/6, "Point density") +
  scale_colour_viridis_d(begin = 1/6, "Point density",direction = -1)+
  labs(x="", y="",title = "Proportion of correctly predicted grid units by density and category",
       subtitle = paste0("c = ", width, ", entropy limit = ", ent_lim)) +
  theme(panel.grid.major.x = element_blank(), legend.position = "bottom")
ggsave(paste0(prefix,"_",ent_pfx,"_correct_grid_prop.pdf"), width = 22, height = 9.5, units = "cm")

# 
#  full_eval[, .(mean_ent= mean(mean_entropy, na.rm=TRUE), sd_ent = sd(mean_entropy, na.rm=TRUE), count = .N), by =pred_cat][order(pred_cat)]
# 
#  full_eval[,.(mean_dens =mean(dens, na.rm=TRUE), median_dens = median(dens, na.rm=TRUE), mean_ent= mean(mean_entropy, na.rm=TRUE)), by = category][order(-category)]
#  full_eval[,.(category = category, mean_dens =mean(dens, na.rm=TRUE), median_dens = median(dens, na.rm=TRUE), mean_ent= mean(mean_entropy, na.rm=TRUE), correct = mean(pred_cat==category, na.rm=TRUE)), by = .(hdp,category)][order(category)]
# full_eval[,.( median_dens = median(dens, na.rm=TRUE), mean_ent= mean(mean_entropy, na.rm=TRUE), correct = mean(pred_cat==category, na.rm=TRUE)), by = .(hdp,category)][order(category)]

# exp(cbind(Odd_Ratio = coef(eval_glm), confint(eval_glm)))
# 
#eval_mat<- model.matrix(correct ~category+hdp+category*hdp, full_eval[!is.na(correct), .(hdp, category, correct)])
# fit <- glmnet(eval_mat[,-1], as.matrix(full_eval[!is.na(correct), correct]), family="binomial")
# 
# effsize::cohen.d(full_eval$correct, full_eval$hdp, na.rm=TRUE)
# 
#   ggplot(full_eval, aes(x=dens, y=category, fill = hdp)) + ggridges::geom_density_ridges2() +theme_minimal() + scale_x_continuous(limits = c(0,5), breaks = seq(0,2,0.4)) + scale_y_discrete(labels=true_labels$category) + scale_fill_viridis_d()
# 
#   ggplot(full_eval, aes(x=mean_entropy, y=category, fill = hdp)) +
#     ggridges::geom_density_ridges2(alpha=0.4, col="grey30")+
#     theme_minimal() +
#     scale_x_continuous(limits = c(0,-log(1/8)), breaks = seq(0,2,0.4)) +
#     scale_y_discrete(labels=true_labels$category) +
#     scale_fill_viridis_d()
# 
# 
# 
# # # hist(full_eval[std_dens < 5, std_dens])
# # # 
# # # ggplot(full_eval[sample(1:nrow(full_eval),5000),], aes(x = std_dens, y = mean_entropy, col = category)) + geom_point() +theme_bw()
# # # 
# # # aa <- lm(mean_entropy ~ category + std_dens +0, data = full_eval)
# # # summary(aa)
# # # hist(resid(aa))
# # # 
# # pred_rate <- sapply(1:seg_count, function(seg){sum(conf_mats[[seg]])/grid_counts[seg]})
# # 
# # 
# # ggplot(data.table(pred_rate=pred_rate, seg = 1:length(pred_rate)), aes(x=pred_rate)) +
# #   geom_density(col = "grey50", fill = "mediumorchid2") + theme_minimal()
# # 
# # 
# # accuracy  <- data.table(t(sapply(1:seg_count, function(seg){diag(conf_mats[[seg]])/colSums(conf_mats[[seg]])})))
# # iou       <- data.table(t(sapply(1:seg_count, function(seg){diag(conf_mats[[seg]])/(colSums(conf_mats[[seg]])+rowSums(conf_mats[[seg]])-diag(conf_mats[[seg]]))})))
# # setnames(accuracy, true_labels$category)
# # setnames(iou, true_labels$category)
# # 
# # 
# # library(ggridges)
# # library(patchwork)
# # 
# # aaa<-ggplot(melt(accuracy), aes(x=value, y = variable, fill = variable)) + 
# #   ggridges::geom_density_ridges2(scale = .85, col = "grey50") +  theme_minimal() +
# #   scale_x_continuous("Accuracy",limits=c(0,1)) + 
# #   scale_fill_manual(values = setNames(true_labels$colour, true_labels$category)) +
# #   theme(legend.position = "none") + ylab("") 
# # bbb<-ggplot(melt(iou), aes(x=value, y = variable, fill = variable)) + 
# #   ggridges::geom_density_ridges2(scale = .85, col = "grey50") +  theme_minimal() +
# #   scale_x_continuous("IoU",limits=c(0,1)) + 
# #   scale_fill_manual(values = setNames(true_labels$colour, true_labels$category)) +
# #   theme(legend.position = "none") + ylab("")
# #   
# #   aaa/bbb
