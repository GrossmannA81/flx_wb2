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


stat_lm <- lm(res_cond ~ pet_p_cond + cti + igbp_land_use + thickness, data = df_stats)
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

plot(plot_stat_lm)


ggsave(
  filename = here::here("analysis/pics/stat_lm_coefficients.png"),
  plot = plot_stat_lm,
  width = 8,
  height = 6,
  dpi = 300
)


#LE_CORR:-----------------

stat_lm_corr <- lm(res_corr_cond ~ pet_p_cond + cti + igbp_land_use + thickness, data = df_stats)
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

plot(plot_stat_corr_lm)

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














