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





###---------RANDOM FOREST:::----------------






