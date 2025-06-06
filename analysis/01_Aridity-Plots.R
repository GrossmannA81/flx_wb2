
library(dplyr)
library(cowplot)

df_budyko <- df_budyko |>
  left_join(
    df_sites |>
      select(sitename, cti, igbp_land_use, canopy_height, whc, mat ),
    by = "sitename"
  )





####----------------BOXPLOTS::---------------------------------------
#Boxplot of Aridity Classes and Deviations without Condensation

df_budyko <- df_budyko |>
  mutate(
    aridity_class = cut(pet_p,
                        breaks = c(-Inf, 1, 2, 4, Inf),
                        labels = c("H", "SH", "SA", "A")
    )
  )

ylim_range <- range(df_budyko$delta_nocond, df_budyko$delta_cond, na.rm = TRUE)

#Boxplot of Aridity Classes and Deviations without Condensation
gg_ai_nocond <- df_budyko |>
  ggplot(aes(x = aridity_class, y = delta_nocond, fill = aridity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +
  coord_cartesian(ylim = ylim_range) +   # hier die y-Achse setzen
  theme_classic() +
  theme(legend.position = "none") +       # Legende ausblenden
  labs(
    x = "Aridity Classes",
    y = expression(Delta),
    title = "Budyko-Deviation accross AI (WITHOUT Condensation)") +
  scale_fill_brewer(palette = "YlOrRd", direction = -1)

ggsave(here::here("analysis/pics/AI_Boxplot_NoCondensation.png"))


#Boxplot of Aridity Classes and Deviations with Condensation
gg_ai_cond <- df_budyko |>
  ggplot(aes(x = aridity_class, y = delta_cond, fill = aridity_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +
  coord_cartesian(ylim = ylim_range) +   # gleiche y-Achse
  theme_classic() +
  theme(legend.position = "none") +       # Legende auch hier aus
  labs(
    x = "Aridity Classes",
    y = expression(Delta),
    title = "Budyko-Deviation accross AI (WITH Condensation)") +
  scale_fill_brewer(palette = "YlOrRd", direction = -1)

ggsave(here::here("analysis/pics/AI_BoxplotCondensation.png"))

gg_ai_combined <- cowplot::plot_grid(
  gg_ai_nocond,
  gg_ai_cond,
  labels = c("A", "B"),     # optionale Beschriftung
  ncol = 2
)


ggsave(
  filename = here::here("analysis/pics/AI_Cond_Comparison.png"),
  plot = gg_ai_combined,
  width = 10,
  height = 5
  )


#------------------------------------------------------------------------------------------

