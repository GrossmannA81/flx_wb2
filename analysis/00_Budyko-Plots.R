



library(ggplot2)
library(cowplot)
library(gghighlight)
library(here)




# Plot 1: No Condensation with LE_F_MDS
bud_p1 <- ggplot(df_budyko, aes(x = pet_p, y = aet_p)) +
  geom_function(fun = fun_fu_equation, args = list(omega = coef(out_nocond)), color = "firebrick") +
  geom_point(size = 0.5) +
  geom_abline(slope = 1, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 5) + ylim(0, 5) +
  labs(title = "No Cond | LE_F_MDS", x = "PET/P", y = "AET/P")



# Plot 2: Condensation with LE_F_MDS
bud_p2 <- ggplot(df_budyko, aes(x = pet_p_cond, y = aet_p_cond)) +
  geom_function(fun = fun_fu_equation, args = list(omega = coef(out_cond)), color = "firebrick") +
  geom_point(size = 0.5) +
  geom_abline(slope = 1, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 5) + ylim(0, 5) +
  labs(title = "Cond | LE_F_MDS", x = "PET/P", y = "AET/P")



# Plot 3: No Condensation and LE_CORR
bud_p3 <- ggplot(df_budyko, aes(x = pet_p, y = aet_corr_p)) +
  geom_function(fun = fun_fu_equation, args = list(omega = coef(out_nocond_corr)), color = "firebrick") +
  geom_point(size = 0.5) +
  geom_abline(slope = 1, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 5) + ylim(0, 5) +
  labs(title = "No Cond | LE_CORR", x = "PET/P", y = "AET/P")



# Plot 4: Condensation with LE_CORR
bud_p4 <- ggplot(df_budyko, aes(x = pet_p_cond, y = aet_corr_p_cond)) +
  geom_function(fun = fun_fu_equation, args = list(omega = coef(out_cond_corr)), color = "firebrick") +
  geom_point(size = 0.5) +
  geom_abline(slope = 1, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 5) + ylim(0, 5) +
  labs(title = "Cond | LE_CORR", x = "PET/P", y = "AET/P")



bud_all <- plot_grid(bud_p1, bud_p3, bud_p2, bud_p4, ncol = 2)

bud_title <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Budyko Curve Fits Across Variants",
    fontface = "bold",
    size = 16,
    x = 0.5,
    hjust = 0.5
  )

bud_all_with_title <- cowplot::plot_grid(
  bud_title,
  bud_all,
  ncol = 1,
  rel_heights = c(0.1, 1)
)


print(bud_all_with_title)


ggsave(
  filename = here::here("flx_wb2/flx_wb2/analysis/pics/Budyko_Comparison_ALL.png"),
  plot = bud_all,
  width = 5,
  height = 5,
  dpi = 300
)



#-----------HISTOGRAMS RESIDUALS::---------------------------

all_res <- c(df_budyko$res, df_budyko$res_corr, df_budyko$res_cond, df_budyko$res_corr_cond)
xlim_min <- floor(min(all_res, na.rm = TRUE) * 10) / 10
xlim_max <- ceiling(max(all_res, na.rm = TRUE) * 10) / 10

# 1. No Cond with LE_F_MDS
bud_hist_nocond <- ggplot(df_budyko, aes(x = res)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey70", bins = 40) +
  labs(title = "No Cond/LE_F_MDS", x = "Residual", y = "Density") +
  theme_classic()

# 2. No Cond with LE_CORR
bud_hist_nocond_corr <- ggplot(df_budyko, aes(x = res_corr)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey70", bins = 40) +
  labs(title = "No Cond/LE_CORR", x = "Residual", y = "Density") +
  theme_classic()

# 3. Cond with LE_F_MDS
bud_hist_cond <- ggplot(df_budyko, aes(x = res_cond)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey70", bins = 40) +
  labs(title = "Cond/LE_F_MDS", x = "Residual", y = "Density") +
  theme_classic()

# 4. Cond with LE_CORR
bud_hist_cond_corr <- ggplot(df_budyko, aes(x = res_corr_cond)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey70", bins = 40) +
  labs(title = "Cond/LE_CORR", x = "Residual", y = "Density") +
  theme_classic()

# Combine all 4 plots in grid
plot_residuals_grid <- cowplot::plot_grid(
  bud_hist_nocond,
  bud_hist_nocond_corr,
  bud_hist_cond,
  bud_hist_cond_corr,
  ncol = 2,
  labels = NULL
)

plot_residuals_title <- cowplot::ggdraw() +
  cowplot::draw_label("Residuals of Budyko Model Fits", fontface = "bold", size = 16, x = 0.5, hjust = 0.5)

plot_residuals_final <- cowplot::plot_grid(
  plot_residuals_title,
  plot_residuals_grid,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

print(plot_residuals_final)

ggsave(
  filename = here::here("flx_wb2/flx_wb2/analysis/pics/Budyko_Residuals_ALL.png"),
  plot = plot_residuals_grid,
  width = 10,
  height = 8,
  dpi = 300
)


## Statistical Summaryof Residuals:##


library(dplyr)
library(tibble)
install.packages("moments")
library(moments)
library(broom)
library(tidyr)
install.packages("writexl")
library(writexl)

# Create a long-format residuals table
df_residuals_long <- df_budyko |>
  select(res, res_corr, res_cond, res_corr_cond) |>
  pivot_longer(cols = everything(), names_to = "variant", values_to = "residual")

# Summarise each variant
bud_summary_table <- df_residuals_long |>
  group_by(variant) |>
  summarise(
    Mean     = round(mean(residual, na.rm = TRUE), 4),
    SD       = round(sd(residual, na.rm = TRUE), 4),
    Median   = round(median(residual, na.rm = TRUE), 4),
    IQR      = round(IQR(residual, na.rm = TRUE), 4),
    Skewness = round(skewness(residual, na.rm = TRUE), 4),
    Shapiro_p = round(shapiro.test(residual)$p.value, 4)
  )

print(bud_summary_table)



# > print(bud_summary_table)
# # A tibble: 4 × 7
# variant          Mean    SD  Median   IQR Skewness Shapiro_p
# <chr>           <dbl> <dbl>   <dbl> <dbl>    <dbl>     <dbl>
#   1 res            0.114  0.894 -0.0698 0.327     6.51         0
# 2 res_cond      -0.0003 0.215 -0.0405 0.204     1.59         0
# 3 res_corr       0.263  1.25   0.0024 0.461     6.22         0
# 4 res_corr_cond -0.0076 0.300 -0.0583 0.277     1.43         0


write_xlsx(bud_summary_table, path = "analysis/tables/bud_summary_table.xlsx")

