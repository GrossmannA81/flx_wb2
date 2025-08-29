
###---------RANDOM FOREST:::----------------

install.packages("caret")
library(caret)
install.packages("ranger")
library(ranger)
install.packages("vip")
library(vip)



ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)
#------------------------------------
#with all predictors::-----
set.seed(123)
stat_rf_all <- train(
  res_cond ~ pet_p_cond + cti + igbp_land_use + thickness,
  data = df_stats,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3,4),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 1 × 4 × 2 = 8 model configurations with 5 trainconrtols
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

plot(stat_rf_all)

summary(stat_rf_all)
print(stat_rf_all)
vip(stat_rf_all)

vip_plot <- vip(stat_rf_all)
ggsave(
  filename = here::here("analysis/pics/stat_rf_vip.png"),
  plot = vip_plot,
  width = 8,
  height = 6,
  dpi = 300
)

#------------------------------------
#only igbp_land_use::-----
set.seed(123)
stat_rf_igbp_land_use <- train(
  res_cond ~ pet_p_cond + igbp_land_use,
  data = df_stats,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 1 × 4 × 2 = 8 model configurations with 5 trainconrtols
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_rf_igbp_land_use)
vip(stat_rf_igbp_land_use)


#------------------------------------
#------only cti::-----
set.seed(123)
stat_rf_cti <- train(
  res_cond ~ pet_p_cond + cti,
  data = df_stats,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 1 × 4 × 2 = 8 model configurations with 5 trainconrtols
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_rf_cti)
vip(stat_rf_cti)


#------------------------------------
#------only thickness::-----
set.seed(123)
stat_rf_thk <- train(
  res_cond ~ pet_p_cond + thickness,
  data = df_stats,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 1 × 4 × 2 = 8 model configurations with 5 trainconrtols
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_rf_thk)
vip(stat_rf_thk)



###Random Forest - LE_CORR-----------------------------------------------------------------------------


#with all predictors::-----
set.seed(123)
stat_rf_corr_all <- train(
  res_corr_cond ~ pet_p_cond + cti + igbp_land_use + thickness,
  data = df_stats,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3,4),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 1 × 4 × 2 = 8 model configurations with 5 trainconrtols
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_rf_corr_all)
vip(stat_rf_corr_all)


vip_plot_corr <- vip(stat_rf_corr_all)

ggsave(
  filename = here::here("analysis/pics/stat_rf_vip_corr.png"),
  plot = vip_plot_corr,
  width = 8,
  height = 6,
  dpi = 300
)

#------------------------------------
#only igbp_land_use::-----
set.seed(123)
stat_rf_corr_igbp_land_use <- train(
  res_corr_cond ~ pet_p_cond + igbp_land_use,
  data = df_stats,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 1 × 4 × 2 = 8 model configurations with 5 trainconrtols
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_rf_corr_igbp_land_use)
vip(stat_rf_corr_igbp_land_use)


#------------------------------------
#------only cti::-----
set.seed(123)
stat_rf_corr_cti <- train(
  res_corr_cond ~ pet_p_cond + cti,
  data = df_stats,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 1 × 4 × 2 = 8 model configurations with 5 trainconrtols
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_rf_corr_cti)
vip(stat_rf_corr_cti)


#------------------------------------
#------only thickness::-----
set.seed(123)
stat_rf_corr_thk <- train(
  res_corr_cond ~ pet_p_cond + thickness,
  data = df_stats,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 1 × 4 × 2 = 8 model configurations with 5 trainconrtols
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_rf_corr_thk)
vip(stat_rf_corr_thk)



#-------------------------------------------------
#------Comparison::-----



model_perf <- tibble::tribble(
  ~Model,       ~RMSE,    ~Rsquared, ~MAE,
  "LCT",        0.2009,   0.1273,    0.1418,
  "LCT-CORR",   0.2740,   0.1491,    0.1939,
  "CTI",        0.2029,   0.0883,    0.1392,
  "CTI-CORR",   0.2873,   0.0898,    0.1971,
  "THK",        0.1991,   0.1229,    0.1385,
  "THK-CORR",   0.2890,   0.0752,    0.2014,
  "ALL",        0.1942,   0.1584,    0.1352,
  "ALL-CORR",   0.2727,   0.1496,    0.1916
)


model_long <- model_perf |>
  pivot_longer(cols = c(RMSE, Rsquared), names_to = "Metric", values_to = "Value")


plot_stat_rf_comparison <-
  ggplot(model_long, aes(x = reorder(Model, Value), y = Value, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("RMSE" = "#E15759", "Rsquared" = "#4E79A7")) +
  geom_text(aes(label = round(Value, 3)),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  expand_limits(y = max(model_long$Value) + 0.05) +
  theme_minimal() +
  labs(
    title = "Random Forest: Model Performance: RMSE vs R² for all Predictors",
    x = "Model",
    y = "Metric Value",
    fill = "Metric"
  ) +
  theme(legend.position = "bottom")


ggsave(
  filename = here::here("analysis/pics/stat_rf_comparison_ALL.png"),
  plot = plot_stat_rf_comparison,
  width = 8,
  height = 6,
  dpi = 300
)














