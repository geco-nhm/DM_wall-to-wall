
####### RUNE - NEW FIGURE ####
library(ggplot2)
fig_data <- read.csv("C:/Users/peterhor/Documents/GitHub_UiO/Project_1.5/PH3_mse3_FigForslag.csv")
str(fig_data)

# DM_maxval
fig_maxval <- ggplot(fig_data, aes(x=area_perc, y=DM_maxval, colour=eval_data)) +  #[fig_data$eval_data=="AR 18x18",]
  # geom_boxplot(alpha=0.8) +
  geom_point(aes(fill = VT.code), alpha=0.5, size = 3, shape = 21) + 
  geom_text(data=fig_data, aes(label=VT.code), nudge_y=-0.01, check_overlap = TRUE) +
  labs(title="DM_maxval",
       x="prevalence (area in %)",
       y="individual kappa")
fig_maxval
ggsave(filename = "kappa_vs_preval_DM_maxval.png",plot = fig_maxval, device = "png", path = "C:/Users/peterhor/OneDrive - Universitetet i Oslo/Paper 1.5/ggplots/",dpi = 300 )

fig2 <- ggplot(fig_data[fig_data$eval_data=="AR 18x18",], aes(x=area_perc, y=DM_maxval, colour=eval_data)) +  #
  # geom_boxplot(alpha=0.8) +
  geom_point(aes(fill = VT.code), alpha=0.5, size = 3, shape = 21) + 
  geom_text(data=fig_data[fig_data$eval_data=="AR 18x18",] , aes(label=VT.code), nudge_y=-0.01, check_overlap = TRUE) +
  labs(title="DM_maxval at evaluation AR18x18",
       x="prevalence (area in %)",
       y="individual kappa")
fig2


# DM_AUC
fig_auc <- ggplot(fig_data, aes(x=area_perc, y=DM_AUC, colour=eval_data)) +  #[fig_data$eval_data=="AR 18x18",]
  # geom_boxplot(alpha=0.8) +
  geom_point(aes(fill = VT.code), alpha=0.5, size = 3, shape = 21) + 
  geom_text(data=fig_data, aes(label=VT.code), nudge_y=-0.01, check_overlap = TRUE) +
  labs(title="DM_AUC",
       x="prevalence (area in %)",
       y="individual kappa")
fig_auc
ggsave(filename = "kappa_vs_preval_DM_AUC.png",plot = fig_auc, device = "png", path = "C:/Users/peterhor/OneDrive - Universitetet i Oslo/Paper 1.5/ggplots/",dpi = 300 )

# DM_preval
fig_preval <- ggplot(fig_data, aes(x=area_perc, y=DM_preval, colour=eval_data)) +  #[fig_data$eval_data=="AR 18x18",]
  # geom_boxplot(alpha=0.8) +
  geom_point(aes(fill = VT.code), alpha=0.5, size = 3, shape = 21) + 
  geom_text(data=fig_data, aes(label=VT.code), nudge_y=-0.01, check_overlap = TRUE) +
  labs(title="DM_preval",
       x="prevalence (area in %)",
       y="individual kappa")
fig_preval
ggsave(filename = "kappa_vs_preval_DM_preval.png",plot = fig_preval, device = "png", path = "C:/Users/peterhor/OneDrive - Universitetet i Oslo/Paper 1.5/ggplots/",dpi = 300 )



##### log adjusted scale ####
# DM_maxval
fig_maxval_log <- ggplot(fig_data, aes(x=log(area_perc), y=DM_maxval, colour=eval_data)) +  #[fig_data$eval_data=="AR 18x18",]
  # geom_boxplot(alpha=0.8) +
  geom_point(aes(fill = VT.code), alpha=0.5, size = 3, shape = 21) + 
  geom_text(data=fig_data, aes(label=VT.code), nudge_y=-0.01, check_overlap = TRUE) +
  labs(title="DM_maxval",
       x="log(prevalence)",
       y="individual kappa")
fig_maxval_log
ggsave(filename = "kappa_vs_log_preval_DM_maxval.png",plot = fig_maxval_log, device = "png", path = "C:/Users/peterhor/OneDrive - Universitetet i Oslo/Paper 1.5/ggplots/",dpi = 300 )


# DM_AUC
fig_auc_log <- ggplot(fig_data, aes(x=log(area_perc), y=DM_AUC, colour=eval_data)) +  #[fig_data$eval_data=="AR 18x18",]
  # geom_boxplot(alpha=0.8) +
  geom_point(aes(fill = VT.code), alpha=0.5, size = 3, shape = 21) + 
  geom_text(data=fig_data, aes(label=VT.code), nudge_y=-0.01, check_overlap = TRUE) +
  labs(title="DM_AUC",
       x="log(prevalence)",
       y="individual kappa")
fig_auc_log
ggsave(filename = "kappa_vs_log_preval_DM_AUC.png",plot = fig_auc_log, device = "png", path = "C:/Users/peterhor/OneDrive - Universitetet i Oslo/Paper 1.5/ggplots/",dpi = 300 )

# DM_preval
fig_preval_log <- ggplot(fig_data, aes(x=log(area_perc), y=DM_preval, colour=eval_data)) +  #[fig_data$eval_data=="AR 18x18",]
  # geom_boxplot(alpha=0.8) +
  geom_point(aes(fill = VT.code), alpha=0.5, size = 3, shape = 21) + 
  geom_text(data=fig_data, aes(label=VT.code), nudge_y=-0.01, check_overlap = TRUE) +
  labs(title="DM_preval",
       x="log(prevalence)",
       y="individual kappa")
fig_preval_log
ggsave(filename = "kappa_vs_log_preval_DM_preval.png",plot = fig_preval_log, device = "png", path = "C:/Users/peterhor/OneDrive - Universitetet i Oslo/Paper 1.5/ggplots/",dpi = 300 )










