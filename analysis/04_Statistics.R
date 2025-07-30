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
# No strongly significant effect of CTI
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
  res_cond ~ pet_p_cond + cti + igbp_land_use,
  data = df_budyko,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3),
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
  data = df_budyko,
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
  data = df_budyko,
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
#------Comparison::-----


stat_rf_results <- resamples(list(
  All = stat_rf_all,
  CTI = stat_rf_cti,
  LCT = stat_rf_igbp_land_use
))

summary(stat_rf_results)

bwplot(stat_rf_results, metric = "RMSE")
bwplot(stat_rf_results, metric = "Rsquared")


