
library(dplyr)
library(cowplot)

# df_budyko <- df_budyko |>
#   left_join(
#     df_sites |>
#       select(sitename, cti, igbp_land_use, canopy_height, whc ),
#     by = "sitename"
#   )

# Berechne CTI-Quantile (5 Klassen)
cti_quants <- quantile(df_budyko$cti, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
aridity_quants_nocond <- quantile(df_budyko$pet_p, probs = seq(0,1, length.out= 6), na.rm=TRUE)
aridity_quants_cond <- quantile(df_budyko$pet_p_cond, probs = seq(0,1, length.out= 6), na.rm=TRUE)

df_budyko <- df_budyko |>
  mutate(
    aridity_class_nocond = factor(
      cut(pet_p,
          breaks = c(-Inf, 1, 2, 4, Inf),
          labels = c("H", "SH", "SA", "A")),
      levels = c("H", "SH", "SA", "A")
    ),
    aridity_class = factor(
      cut(pet_p_cond,
          breaks = c(-Inf, 1, 2, 4, Inf),
          labels = c("H", "SH", "SA", "A")),
      levels = c("H", "SH", "SA", "A")
    ),
    cti_class = cut(cti,
                    breaks = cti_quants,
                    include.lowest = TRUE,
                    labels = c("Very Low", "Low", "Medium", "High", "Very High")
    )
  )




####----------------BOXPLOTS::---------------------------------------



ai_ylim_range_residuals <- range(df_budyko$res,df_budyko$res_corr, df_budyko$res_cond, df_budyko$res_corr_cond, na.rm = TRUE)
ai_cols <- RColorBrewer::brewer.pal(4, "YlOrRd")
names(cols) <- c("H", "SH", "SA", "A")


#Boxplot of Aridity Classes and Deviations WITHOUT Condensation - LE_F_MDS
ai_nocond <- df_budyko |>
  ggplot(aes(x = aridity_class, y = res, fill = aridity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +
  coord_cartesian(ylim = ai_ylim_range_residuals) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),  # Axis title size
    axis.text = element_text(size = 10),   # Axis tick label size
    plot.title = element_text(size = 13, face = "bold")  # Title size
  ) +
  labs(
    x = NULL,
    y = expression(epsilon * "'"),
    title = "No Cond | LE_F_MDS") +
  scale_fill_manual(
    values = ai_cols,
    labels = c(
      "H" = "H = Humid",
      "SH" = "SH = Semi-Humid",
      "SA" = "SA = Semi-Arid",
      "A" = "A = Arid"
    ),
    name = "Aridity Class"
  )



#Boxplot of Aridity Classes and Deviations WITHOUT Condensation - LE_CORR
ai_nocond_corr <- df_budyko |>
  ggplot(aes(x = aridity_class, y = res_corr, fill = aridity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +
  coord_cartesian(ylim = ai_ylim_range_residuals) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),  # Axis title size
    axis.text = element_text(size = 10),   # Axis tick label size
    plot.title = element_text(size = 13, face = "bold")  # Title size
  ) +
  labs(
    x = NULL,
    y = expression(epsilon * "'"),
    title = "No Cond | LE_CORR") +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(4, "YlOrRd"),
    labels = c(
      "H" = "H = Humid",
      "SH" = "SH = Semi-Humid",
      "SA" = "SA = Semi-Arid",
      "A" = "A = Arid"
    ),
    name = "Aridity Class"
  )



#Boxplot of Aridity Classes and Deviations WITH Condensation and LE_F_MDS

ai_cond <- df_budyko |>
  ggplot(aes(x = aridity_class, y = res_cond, fill = aridity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +
  coord_cartesian(ylim = ai_ylim_range_residuals) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold")
  ) +
  labs(
    x = NULL,
    y = expression(epsilon * "'"),
    title = "Cond | LE_F_MDS") +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(4, "YlOrRd"),
    labels = c(
      "H" = "H = Humid",
      "SH" = "SH = Semi-Humid",
      "SA" = "SA = Semi-Arid",
      "A" = "A = Arid"
    ),
    name = "Aridity Class"
  )



#Boxplot of Aridity Classes and Deviations WITH Condensation and LE_CORR
ai_cond_corr <- df_budyko |>
  ggplot(aes(x = aridity_class, y = res_corr_cond, fill = aridity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +
  coord_cartesian(ylim = ai_ylim_range_residuals) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold")
  ) +
  labs(
    x = NULL,
    y = expression(epsilon * "'"),
    title = "Cond | LE_CORR") +

  scale_fill_manual(
    values = RColorBrewer::brewer.pal(4, "YlOrRd"),
    labels = c(
      "H" = "H = Humid",
      "SH" = "SH = Semi-Humid",
      "SA" = "SA = Semi-Arid",
      "A" = "A = Arid"
    ),
    name = "Aridity Class"
  )







#Plot:

ai_legend <- cowplot::get_legend(
  ai_nocond + theme(legend.position = "bottom")
)

ai_combined <- cowplot::plot_grid(
  ai_nocond + theme(legend.position = "none"),
  ai_nocond_corr + theme(legend.position = "none"),
  ai_cond+ theme(legend.position = "none"),
  ai_cond_corr + theme(legend.position = "none"),
  labels = c("A", "B", "C", "D"),
  ncol = 2
)

ai_title <- cowplot::ggdraw()+
  cowplot::draw_label(
    "Aridity Classes and Deviations - Boxplots",
    fontface = "bold",
    size= 16,
    x= 0.5,
    hjust= 0.5
  )

ai_combined_final <- cowplot::plot_grid(
  ai_title,
  ai_combined,
  ai_legend,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.1)
)

print(ai_combined_final)


ggsave(here::here("analysis/pics/AI_Boxplot_ALL.png"))







#-------------------------------HEATMAP all sites-----------------------------------------------------------

#all with condensation


library(patchwork)
library(cowplot)
library(ggplot2)

df_hm_bin1 <- df_budyko |>
  # filter(!is.na(res_cond)) |>
  group_by(cti_class, aridity_class_nocond) |>
  summarise(
    mean_res = mean(res, na.rm = TRUE),
#    count = n(),
    .groups = "drop"
  ) |>
  complete(
    cti_class = factor(c("Very Low", "Low", "Medium", "High", "Very High"),
                       levels = c("Very Low", "Low", "Medium", "High", "Very High")),
    aridity_class_nocond = factor(c("H", "SH", "SA", "A"),
                           levels = c("H", "SH", "SA", "A")),
    fill = list(mean_res = NA)
  )

df_hm_bin2 <- df_budyko |>
  #  filter(!is.na(res_corr_cond)) |>
  group_by(cti_class, aridity_class_nocond) |>
  summarise(
    mean_res_corr = mean(res_corr, na.rm = TRUE),
#    count = n(),
    .groups = "drop"
  ) |>
  complete(
    cti_class = factor(c("Very Low", "Low", "Medium", "High", "Very High"),
                       levels = c("Very Low", "Low", "Medium", "High", "Very High")),
    aridity_class_nocond = factor(c("H", "SH", "SA", "A"),
                           levels = c("H", "SH", "SA", "A")),
    fill = list(mean_res_corr = NA)
  )


df_hm_bin3 <- df_budyko |>
 # filter(!is.na(res_cond)) |>
  group_by(cti_class, aridity_class) |>
  summarise(
    mean_res_cond = mean(res_cond, na.rm = TRUE),
#    count = n(),
    .groups = "drop"
  ) |>
  complete(
    cti_class = factor(c("Very Low", "Low", "Medium", "High", "Very High"),
                       levels = c("Very Low", "Low", "Medium", "High", "Very High")),
    aridity_class = factor(c("H", "SH", "SA", "A"),
                           levels = c("H", "SH", "SA", "A")),
    fill = list(mean_res_cond = NA)
  )

df_hm_bin4 <- df_budyko |>
#  filter(!is.na(res_corr_cond)) |>
  group_by(cti_class, aridity_class) |>
  summarise(
    mean_res_corr_cond = mean(res_corr_cond, na.rm = TRUE),
#    count = n(),
    .groups = "drop"
  ) |>
  complete(
    cti_class = factor(c("Very Low", "Low", "Medium", "High", "Very High"),
                       levels = c("Very Low", "Low", "Medium", "High", "Very High")),
    aridity_class = factor(c("H", "SH", "SA", "A"),
                           levels = c("H", "SH", "SA", "A")),
    fill = list(mean_res_corr_cond = NA)
  )





#set limits to prevent outliers from influencing resudial scale / distribution
res_limits <- df_budyko |>
  summarise(
    lower = quantile(res, 0.05, na.rm = TRUE),
    upper = quantile(res, 0.95, na.rm = TRUE)
  )

common_heatmap_scale <- scale_fill_gradient2(
  low = "darkslategrey",
  mid = "beige",
  high = "hotpink4",
  midpoint = 0,
  name = expression(epsilon*"′"),
  limits = c(res_limits$lower, res_limits$upper),
  labels = scales::label_number(accuracy = 0.1),
  breaks = waiver (),#seq(-0.3, 0.3, by = 0.1),
  na.value = "white"
)


heatmap_bin1 <- ggplot(df_hm_bin1, aes(
  x = aridity_class_nocond, y = cti_class, fill = mean_res)) +
  geom_tile(color = "white") +
#  geom_text(aes(label = ifelse(count == 0, "NA", count)), size = 3) +
  common_heatmap_scale +
  labs(title = "No Cond | LE_F_MDS", x = "Aridity Class", y = "CTI Class") +
  theme_classic()+
  theme(legend.position = "none")



heatmap_bin2 <- ggplot(df_hm_bin2, aes(
  x = aridity_class_nocond, y = cti_class, fill = mean_res_corr)) +
  geom_tile(color = "white") +
#  geom_text(aes(label = ifelse(count == 0, "NA", count)), size = 3) +
  common_heatmap_scale +
  labs(title = "No Cond | LE_CORR", x = "Aridity Class", y = "CTI Class") +
  theme_classic()+
  theme(legend.position = "none")


heatmap_bin3 <- ggplot(df_hm_bin3, aes(
  x = aridity_class, y = cti_class, fill = mean_res_cond)) +
  geom_tile(color = "white") +
#  geom_text(aes(label = ifelse(count == 0, "NA", count)), size = 3) +
  common_heatmap_scale +
  labs(title = "Cond | LE_F_MDS", x = "Aridity Class", y = "CTI Class") +
  theme_classic()+
  theme(legend.position = "none")



heatmap_bin4 <- ggplot(df_hm_bin4, aes(
  x = aridity_class, y = cti_class, fill = mean_res_corr_cond)) +
  geom_tile(color = "white") +
#  geom_text(aes(label = ifelse(count == 0, "NA", count)), size = 3) +
  common_heatmap_scale +
  labs(title = "Cond | LE_CORR", x = "Aridity Class", y = "CTI Class") +
  theme_classic()+
  theme(legend.position = "none")



legend <- cowplot::get_legend(
  ggplot(df_hm_bin1, aes(
    x = aridity_class_nocond, y = cti_class, fill = mean_res)) +
    geom_tile() +
    common_heatmap_scale +
    theme(legend.position = "right")  # Legende wird nur zum extrahieren angezeigt
)

heatmap_grid_only <- cowplot::plot_grid(
  heatmap_bin1,
  heatmap_bin2,
  heatmap_bin3,
  heatmap_bin4,
  ncol = 2,
  labels = NULL
)


plot_hm_grid <- cowplot::plot_grid(
  heatmap_grid_only,
  legend,
  ncol = 2,
  rel_widths = c(1, 0.15)
  )

plot_hm_title <- cowplot::ggdraw() +
  cowplot::draw_label("Heatmaps of Deviations with CTI- and Aricity Classes", fontface = "bold", size = 16, x = 0.5, hjust = 0.5)


plot_hm_final <- cowplot::plot_grid(
  plot_hm_title,
  plot_hm_grid,
  ncol = 1,
  rel_heights = c(0.1,1)
)


print(plot_hm_final)


ggsave(
  filename = here::here("analysis/pics/Heatmap_ALL.png"),
  plot = plot_hm_final,
  width = 10,
  height = 5
)





