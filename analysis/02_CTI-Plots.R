

library(dplyr)

##-------------------HEATMAPS::-----------------------------


#Deviation and Aridity-Classes WITHOUT Condensation
# df_heatmap <- df_budyko |>
#   mutate(cti_class = cut(
#     cti,
#     breaks = c(2, 6, 9, 12, 15),
#     labels = c("low", "medium-low", "medium-high", "high"),
#     right = TRUE
#   )) |>
#   group_by(aridity_class, cti_class) |>
#   summarise(
#     mean_delta_nocond = mean(delta_nocond, na.rm = TRUE),
#     mean_delta_cond = mean(delta_cond, na.rm = TRUE),
#     .groups = "drop"
#     )


# common_heatmap_scale <- scale_fill_gradient2(
#   low = "darkslategrey",
#   mid = "beige",
#   high = "hotpink4",
#   midpoint = 0,
#   name = expression(epsilon*"′ (Mean Deviation)"),
#   limits = c(-0.4, 1.4),
#   labels = scales::label_number(accuracy = 0.1),
#   breaks = seq(-0.4, 1.4, by = 0.4)
# )


df_budyko <- df_budyko |>
  summarise(
    mean_res = mean(res, na.rm = TRUE),
    mean_res_corr = mean(res_corr, na.rm = TRUE),
    mean_res_cond = mean(res_cond, na.rm = TRUE),
    mean_res_corr_cond = mean(res_corr, na.rm = TRUE),
    .groups = "drop"
    )


gg_cti_heat_nocond <- df_budyko |>
  ggplot(
    aes (
      x = aridity_class,
      y = cti_class,
      fill = mean_delta_nocond)) +
  geom_tile(color = "white") +
  common_heatmap_scale +
  labs(
    x = "Aridity Classes (PET/P)",
    y = "CTI-Classes",
    title = " CTI- und Aridity Classes (No-Cond)"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0)
  )

 plot(gg_cti_heat_nocond)
#
# ggsave(here::here("analysis/pics/AI_CTI_Heatmap_Nocond.png"))



#Deviation and Aridity / CTI WITH Condensation
gg_cti_heat_cond <- df_heatmap |>
  ggplot(
    aes (
      x = aridity_class,
      y = cti_class,
      fill = mean_delta_cond)) +
  geom_tile(color = "white") +
  common_heatmap_scale +
  labs(
    x = "Aridity Classes (PET/P)",
    y = "CTI-Classes",
    title = " CTI- und Aridity Classes (Cond)"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0)
  )

# plot(gg_cti_heat_cond)
#
ggsave(here::here("analysis/pics/AI_CTI_Heatmap_Cond.png"))




gg_cti_heat_Comparison <- cowplot :: plot_grid(
  gg_cti_heat_nocond,
  gg_cti_heat_cond,
  labels = c("A", "B"),
  ncol = 2
)

#plot(gg_cti_heat_Comparison)

gg_cti_heat_final <- cowplot::plot_grid(
  gg_cti_heat_Comparison,
  width = 12,
  height = 5,
  rel_widths = c(1, 0.15),
  ncol = 2
  )

plot(gg_cti_heat_final)

ggsave(
  filename = here::here("analysis/pics/AI_CTI_Heatmap_Comparison.png"),
  plot = gg_cti_heat_final,
  width = 12,
  height = 5
)





###-------------------SCATTERs---------------------------

# Scatterplot CTI and Deviation (No Co)
ggplot(df_budyko, aes(x = cti, y = delta_nocond)) +
  geom_point(alpha = 0.7, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_classic() +
  labs(x = "CTI", y = expression(Delta), title = "Budyko-Deviation vs. CTI")






#--------------------Heatmap EVAPORATIVE INDEX and Cond:-------------

gg_cti_heat_cond_EA <- df_heatmap |>
  ggplot(
    aes (
      x = evaporative_class,
      y = cti_class,
      fill = mean_delta_cond)) +
  geom_tile(color = "white") +
  common_heatmap_scale +
  labs(
    x = "Evaporative Classes (AET/P)",
    y = "CTI-Classes",
    title = " CTI- und Evaporative Classes (with Condensation)"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0)
  )

#
plot(gg_cti_heat_cond_EA)
#
ggsave(here::here("analysis/pics/EI_CTI_Heatmap_Cond.png"))


