


df_budyko <- df_budyko |>
  mutate(
    evaporative_class = cut(
      aet_p_cond,
      breaks = c(-Inf, 1, Inf),
      labels = c(" < 1", " > 1")
    )
  )


#Boxplot of Evaporative Classes and Deviations with Condensation
gg_ei_cond <- df_budyko |>
  ggplot(aes(x = evaporative_class, y = delta_cond, fill = evaporative_class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +
#  coord_cartesian(ylim = ylim_range) +   # gleiche y-Achse
  theme_classic() +
  theme(legend.position = "none") +       # Legende auch hier aus
  labs(
    x = "Aridity Classes",
    y = expression(Delta),
    title = "Budyko-Deviation accross EI (WITH Condensation)") +
  scale_fill_brewer(palette = "YlOrRd", direction = -1)

gg_ei_cond
ggsave(here::here("analysis/pics/EI_BoxplotCondensation.png"))




##---------------------- EVAPORTATIVE INDEX FACET MAP::---------------------


#--by Landcover Type
evap_class <- 1

df_facet_map_ei <- df_budyko |>
  select(sitename, aet_p_cond, cti, delta_cond, igbp_land_use) |>
  mutate(
    highlight = ifelse(aet_p_cond > evap_class, igbp_land_use, NA)  # alternatively replace land_cover by sitenames
  )



# Plot
gg_cti_ei_diagram <- df_facet_map_ei |>
  ggplot(aes(x = cti, y = aet_p_cond)) +
  geom_point(aes(color = delta_cond), size = 2) +
  geom_text_repel(
    data = df_facet_map_ei |> filter(aet_p_cond > evap_class),
    aes(label = highlight),
    size = 3, fontface = "bold"
  ) +
  scale_color_gradient2(
    low = "darkslategrey",
    mid = "beige",
    high = "hotpink4",
    midpoint = 0,
    limits = c(-0.4, 1.4),
    breaks = seq(-0.4, 1.4, by = 0.4),
    labels = scales::label_number(accuracy = 0.1),
    name = expression(epsilon*"′ (Deviation)")
  ) +
  labs(
    x = "CTI (Compound Topographic Index)",
    y = "Evaporative Index (AET / P)",
    title = "Sites AET > P and Budyko Deviation by CTI",
    color = expression(epsilon*"′")
  ) +
  theme_classic()
  # theme(
  #   strip.background = element_rect(fill = "grey95", color = NA),
  #   strip.text = element_text(size = 12, face = "bold"),
  #   legend.position = "right"
  # )


plot(gg_cti_ei_diagram)

ggsave(
  filename = here::here("analysis/pics/EI_CTI_Diagram_by_Landcover.png"),
  plot = gg_cti_ei_diagram,
  width = 12,
  height = 5
)



#by Sitename--:
gg_cti_ei_diagram <- df_facet_map_ei |>
  ggplot(aes(x = cti, y = aet_p_cond)) +
  geom_point(aes(color = delta_cond), size = 2) +
  geom_text_repel(
    data = df_facet_map_ei |> filter(aet_p_cond > evap_class),
    aes(label = highlight),
    size = 3, fontface = "bold"
  ) +
  scale_color_gradient2(
    low = "darkslategrey",
    mid = "beige",
    high = "hotpink4",
    midpoint = 0,
    limits = c(-0.4, 1.4),
    breaks = seq(-0.4, 1.4, by = 0.4),
    labels = scales::label_number(accuracy = 0.1),
    name = expression(epsilon*"′ (Deviation)")
  ) +
  labs(
    x = "CTI (Compound Topographic Index)",
    y = "Evaporative Index (AET / P)",
    title = "Sites AET > P and Budyko Deviation by CTI",
    color = expression(epsilon*"′")
  ) +
  theme_classic()
# theme(
#   strip.background = element_rect(fill = "grey95", color = NA),
#   strip.text = element_text(size = 12, face = "bold"),
#   legend.position = "right"
# )


plot(gg_cti_ei_diagram)

ggsave(
  filename = here::here("analysis/pics/EI_CTI_Diagram_by_Landcover.png"),
  plot = gg_cti_ei_diagram,
  width = 12,
  height = 5
)


























df_highlight_landuse_evap <- df_budyko |>
  select(sitename, aet_p_cond, cti, delta_cond, igbp_land_use) |>
  mutate(
    highlight = ifelse(aet_p_cond > evap_class, igbp_land_use, NA)
  )

df_highlight_sitename_evap <- df_budyko |>
  select(sitename, aet_p_cond, cti, delta_cond, igbp_land_use) |>
  mutate(
    highlight = ifelse(aet_p_cond > evap_class, sitename, NA)
  )


gg_cti_ei_diagram_landuse <- df_highlight_landuse_evap |>
  ggplot(aes(x = cti, y = aet_p_cond)) +
  geom_point(aes(color = delta_cond), size = 2) +
  geom_text_repel(
    data = df_highlight_landuse_evap |> filter(aet_p_cond > evap_class),
    aes(label = highlight),
    size = 3, fontface = "bold"
  ) +
  scale_color_gradient2(
    low = "darkslategrey", mid = "beige", high = "hotpink4", midpoint = 0,
    limits = c(-0.4, 1.4), breaks = seq(-0.4, 1.4, by = 0.4),
    labels = scales::label_number(accuracy = 0.1),
    name = expression(epsilon*"′")
  ) +
  labs(
    x = "CTI", y = "Evaporative Index (AET / P)",
    title = "Highlight: Land Use"
  ) +
  theme_classic()


# Plot mit Sitename
gg_cti_ei_diagram_sitename <- df_highlight_sitename_evap |>
  ggplot(aes(x = cti, y = aet_p_cond)) +
  geom_point(aes(color = delta_cond), size = 2) +
  geom_text_repel(
    data = df_highlight_sitename_evap |> filter(aet_p_cond > evap_class),
    aes(label = highlight),
    size = 3, fontface = "bold"
  ) +
  scale_color_gradient2(
    low = "darkslategrey", mid = "beige", high = "hotpink4", midpoint = 0,
    limits = c(-0.4, 1.4), breaks = seq(-0.4, 1.4, by = 0.4),
    labels = scales::label_number(accuracy = 0.1),
    name = expression(epsilon*"′")
  ) +
  labs(
    x = "CTI", y = "Evaporative Index (AET / P)",
    title = "Highlight: Site Name"
  ) +
  theme_classic()

#plot(gg_cti_ei_diagram_sitename)
cowplot::plot_grid(gg_cti_ei_diagram_landuse, gg_cti_ei_diagram_sitename, labels = c("A", "B"))

ggsave(
  filename = here::here("analysis/pics/EI_CTI_Diagram_by_Landcover_and_Site.png"),
  plot = gg_cti_ei_diagram,
  width = 12,
  height = 5
)
