install.packages("broom")
library(broom)
library(ggplot2)
library(dplyr)
library(forcats)
library(here)



df_stats <- df_budyko |>
  select(
    sitename,
    cti,
    igbp_land_use,
    res_cond,
    res_corr_cond,
    whc,
    pet_p_cond
  ) |>
  left_join(df_thickness, by = "sitename")



#------------------------------------
##LM:----------------
#linear model LE_F_MDS

df_stats$igbp_land_use <- as.factor(df_stats$igbp_land_use)
df_stats$igbp_land_use <- relevel(df_stats$igbp_land_use, ref = "GRA")


stat_lm <- lm(res_cond ~ pet_p_cond + cti + whc + igbp_land_use + thickness, data = df_stats)
summary(stat_lm)






# Extract tidy model results
df_coef_lm<- tidy(stat_lm, conf.int = TRUE)

# Reorder for plotting and filter out intercept
coef_lm_plot <- df_coef_lm |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, estimate))

# Create the plot
plot_stat_lm <- ggplot(coef_lm_plot, aes(x = estimate, y = term)) +
  geom_point(color = "firebrick4", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = " Linear Model: Estimates of Coefficients LE_F_MDS",
    x = "Effect on Residuals",
    y = NULL
    ) +
  theme_minimal(base_size = 14)


ggsave(
  filename = here::here("analysis/pics/stat_lm_coefficients.png"),
  plot = plot_stat_lm,
  width = 8,
  height = 6,
  dpi = 300
)


#LE_CORR:-----------------

stat_lm_corr <- lm(res_corr_cond ~ pet_p_cond + cti + whc + igbp_land_use + thickness, data = df_stats)
summary(stat_lm_corr)




# Extract tidy model results
df_coef_lm_corr<- tidy(stat_lm_corr, conf.int = TRUE)

# Reorder for plotting and filter out intercept
coef_lm_corr_plot <- df_coef_lm_corr |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, estimate))

# Create the plot
plot_stat_corr_lm <- ggplot(coef_lm_corr_plot, aes(x = estimate, y = term)) +
  geom_point(color = "firebrick4", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = " Linear Model: Estimates of Coefficients LE_CORR",
    x = "Effect on Residuals",
    y = NULL
  ) +
  theme_minimal(base_size = 14)


ggsave(
  filename = here::here("analysis/pics/stat_lm_coefficients_corr.png"),
  plot = plot_stat_corr_lm,
  width = 8,
  height = 6,
  dpi = 300
)




## Linear model only with CTI ----------------------------------------------------------------
#
stat_lm_cti <- lm(res_cond ~ pet_p_cond + cti, data = df_stats)
summary(stat_lm_cti)


# > summary(stat_lm_cti)
#
# Call:
#   lm(formula = res_cond ~ pet_p_cond + cti, data = df_stats)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.52121 -0.11371 -0.04382  0.07994  0.87482
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.094302   0.044576  -2.116   0.0353 *
#   pet_p_cond   0.031662   0.020586   1.538   0.1252
# cti          0.009190   0.006226   1.476   0.1411
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2094 on 268 degrees of freedom
# Multiple R-squared:  0.01839,	Adjusted R-squared:  0.01106
# F-statistic:  2.51 on 2 and 268 DF,  p-value: 0.0832

## Linear model only with CTI ----------------------------------------------------------------
#
stat_lm_thk <- lm(res_cond ~ pet_p_cond + thickness, data = df_stats)
summary(stat_lm_thk)

# > summary(stat_lm_thk)
#
# Call:
#   lm(formula = res_cond ~ pet_p_cond + thickness, data = df_stats)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.50674 -0.11853 -0.02970  0.08125  0.87107
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.0685684  0.0300058  -2.285  0.02308 *
#   pet_p_cond   0.0294903  0.0203873   1.447  0.14921
# thickness    0.0013488  0.0004986   2.705  0.00727 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2074 on 268 degrees of freedom
# Multiple R-squared:  0.0367,	Adjusted R-squared:  0.02951
# F-statistic: 5.106 on 2 and 268 DF,  p-value: 0.006666

#---------------------------------------------------------------------------
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

print(stat_rf_all)
vip(stat_rf_all)

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

# > stat_rf_igbp_land_use$bestTune
# mtry splitrule min.node.size
# 10    2  variance             3



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



#------------------------------------
#------Comparison::-----


stat_rf_results <- resamples(list(
  All = stat_rf_all,
  CTI = stat_rf_cti,
  LCT = stat_rf_igbp_land_use,
  THK = stat_rf_thk
))

summary(stat_rf_results)

bwplot(stat_rf_results, metric = "RMSE")
bwplot(stat_rf_results, metric = "Rsquared")



#Plot:

library(ggplot2)
library(tidyr)
library(dplyr)

# Manually input mean values from stat_rf_results summary
stat_metrics <- tibble::tibble(
  stat_Model = c("All", "CTI", "LCT", "THK"),
  stat_RMSE = c(0.1942, 0.2030, 0.2010, 0.1991),
  stat_R2 = c(0.1585, 0.0883, 0.1273, 0.1229)
)

# Convert to long format for ggplot
stat_metrics_long <- stat_metrics |>
  pivot_longer(cols = c(stat_RMSE, stat_R2), names_to = "Metric", values_to = "Value")|>
  mutate(Metric = recode(Metric,
                        stat_RMSE = "RMSE",
                        stat_R2 = "R²"))

  # Plot: grouped barplot
ggplot(stat_metrics_long, aes(x = stat_Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("RMSE" = "firebrick", "R²" = "darkblue")) +
  labs(
    title = "Random Forest Performance by Predictor - LE_F_MDS",
    y = "Value", x = "Model",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 13)






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

# > stat_rf_igbp_land_use$bestTune
# mtry splitrule min.node.size
# 10    2  variance             3



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



#------------------------------------
#------Comparison::-----


stat_rf_corr_results <- resamples(list(
  All = stat_rf_corr_all,
  CTI = stat_rf_corr_cti,
  LCT = stat_rf_corr_igbp_land_use,
  THK = stat_rf_corr_thk
))

summary(stat_rf_corr_results)

bwplot(stat_rf_corr_results, metric = "RMSE")
bwplot(stat_rf_corr_results, metric = "Rsquared")



#Plot:

library(ggplot2)
library(tidyr)
library(dplyr)

# Manually input mean values from stat_rf_results summary
stat_corr_metrics <- tibble::tibble(
  stat_corr_Model = c("All", "CTI", "LCT", "THK"),
  stat_corr_RMSE = c(0.27477, 0.28733, 0.27404, 0.28909),
  stat_corr_R2 = c(0.14966, 0.08989, 0.14913, 0.07527)
)

# Convert to long format for ggplot
stat_corr_metrics_long <- stat_corr_metrics |>
  pivot_longer(cols = c(stat_corr_RMSE, stat_corr_R2), names_to = "Metric", values_to = "Value")|>
  mutate(Metric = recode(Metric,
                         stat_corr_RMSE = "RMSE",
                         stat_corr_R2 = "R²"))

# Plot: grouped barplot
ggplot(stat_corr_metrics_long, aes(x = stat_corr_Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("RMSE" = "firebrick", "R²" = "darkblue")) +
  labs(
    title = "Random Forest Performance by Predictor - LE_CORR",
    y = "Value", x = "Model",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 13)






