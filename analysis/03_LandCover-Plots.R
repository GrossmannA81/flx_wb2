
###-------------------CTI and Aridity Deviation and Landcovertypes -  facet Heatmap---------------------------




library(ggrepel)



pet_p_threshold <- quantile(df_budyko$pet_p, probs = 0.95, na.rm = TRUE)

df_facet_map <- df_budyko |>
  select(sitename, pet_p, pet_p_cond, cti, delta_nocond, delta_cond, igbp_land_use) |>
  rename(`No Condensation` = delta_nocond,
         `With Condensation` = delta_cond) |>
  pivot_longer(
    cols = c(`No Condensation`, `With Condensation`),
    names_to = "Condensation",
    values_to = "delta"
  ) |>
  mutate(
    pet_p_val = case_when(
      Condensation == "No Condensation" ~ pet_p,
      Condensation == "With Condensation" ~ pet_p_cond
    ),
    highlight = ifelse(pet_p > pet_p_threshold, igbp_land_use, NA)  # alternatively replace land_cover by sitenames
  )

# Plot
gg_cti_ai_diagram <- df_facet_map |>
  ggplot(aes(x = cti, y = pet_p_val)) +
  geom_point(aes(color = delta), size = 2) +
  geom_text_repel(
    data = df_facet_map |> filter(pet_p > pet_p_threshold),
    aes(label = highlight),
    size = 3, fontface = "bold"
  ) +
  facet_wrap(~Condensation) +
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
    y = "Aridity (PET / P)",
    title = "Deviation from Budyko Curve by CTI and Aridity",
    color = expression(epsilon*"′")
  ) +
  theme_classic() +
  theme(
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )


plot(gg_cti_ai_diagram)

ggsave(
  filename = here::here("analysis/pics/AI_CTI_Diagram_by_Landcover.png"),
  plot = gg_cti_ai_diagram,
  width = 12,
  height = 5
)


###-------------------CTI and Aridity Deviation and Landcovertypes -  individually---------------------------


# install.packages("patchwork")
# library(patchwork)
#
#
# df_facet_by_covertype <- df_budyko |>
#   select(sitename, pet_p, pet_p_cond, cti, delta_nocond, delta_cond, igbp_land_use) |>
#   rename(
#     `No Condensation` = delta_nocond,
#     `With Condensation` = delta_cond
#   ) |>
#   pivot_longer(
#     cols = c(`No Condensation`, `With Condensation`),
#     names_to = "Condensation",
#     values_to = "delta"
#   ) |>
#   mutate(
#     pet_p_val = case_when(
#       Condensation == "No Condensation" ~ pet_p,
#       Condensation == "With Condensation" ~ pet_p_cond
#     ),
#     highlight = ifelse(pet_p > pet_p_threshold, igbp_land_use, NA)
#   )
#
# land_cover_types <- unique(df_facet_by_covertype$igbp_land_use)
#
# plots_by_cover <- list()
#
#
# for (lc in land_cover_types) {
#   df_filtered <- df_facet_by_covertype |> filter(igbp_land_use == lc)
#
#   gg_cti_ai_covertypes <- ggplot(df_filtered, aes(x = cti, y = pet_p_val)) +
#     geom_point(aes(color = delta), size = 2) +
#     geom_text_repel(
#       data = df_filtered |> filter(pet_p > pet_p_threshold),
#       aes(label = highlight),
#       size = 3, fontface = "bold"
#     ) +
#     facet_wrap(~Condensation) +
#     scale_color_gradient2(
#       low = "darkslategrey", mid = "beige", high = "hotpink4",
#       midpoint = 0, limits = c(-0.4, 1.4), breaks = seq(-0.4, 1.4, 0.4),
#       labels = scales::label_number(accuracy = 0.1),
#       name = expression(epsilon*"′ (Deviation)")
#     ) +
#     labs(
#       title = paste("CTI vs. Aridity –", lc),
#       x = "CTI (Compound Topographic Index)",
#       y = "Aridity (PET / P)",
#       color = expression(epsilon*"′")
#     ) +
#     theme_classic() +
#     theme(
#       strip.background = element_rect(fill = "grey95", color = NA),
#       strip.text = element_text(size = 12, face = "bold"),
#       legend.position = "right"
#     )
#
#   plots_by_cover[[lc]] <- gg_cti_ai_covertypes
# }
#
#
# combined_plot <- wrap_plots(plots_by_cover, ncol = 3)
#
# print(combined_plot)




###-------------------Landcovertypes and CTI with highest AridityIndex-  TABLE---------------------------


install.packages("writexl")
library(writexl)

#--------------------------------------------------------------------------------------------------------

#WITHOUT

pet_p_90th <- quantile(df_budyko$pet_p, 0.9, na.rm = TRUE) #df_budyko -> n = 237 (top 10 = 24)


df_landcover_NC <- df_budyko |>
  filter(pet_p >= pet_p_90th) |>
  select(sitename, igbp_land_use, cti, pet_p, pet_p_cond)

land_use_summary_NC <- df_landcover_NC |>
  count(igbp_land_use, name = "n") |>
  mutate(percent = round(100 * n / sum(n), 1)) |>
  arrange(desc(n))

df_landcover_NC <- df_landcover_NC |>
  mutate(igbp_land_use = factor(igbp_land_use, levels = land_use_summary_NC$igbp_land_use)) |>
  arrange(igbp_land_use)

df_landcover_table_detail_NC <- df_landcover_NC |>
  arrange(desc(pet_p)) |>
  mutate(
    AI      = round(pet_p, 3),
    CTI     = round(cti, 3),
    High_Aridity = pet_p >=2,
    High_CTI = cti >=7
  ) |>
  select(
    AI,
    LandCover = igbp_land_use,
    CTI,
    High_Aridity,
    High_CTI,
    Sitename = sitename
  )

df_landcover_table_summary_NC <- land_use_summary_NC |>
  rename(
    LandCover = igbp_land_use,
    Count = n,
    Percent = percent
  )





#WITH

pet_p_cond_90th <- quantile(df_budyko$pet_p_cond, na.rm = TRUE, probs = 0.9)

df_landcover_WC <- df_budyko |>
  filter(pet_p_cond >= pet_p_cond_90th) |>
  select(sitename, igbp_land_use, cti, pet_p, pet_p_cond)

land_use_summary_WC <- df_landcover_WC |>
  count(igbp_land_use, name = "n") |>
  mutate(percent = round(100 * n / sum(n), 1)) |>
  arrange(desc(n))

df_landcover_WC <- df_landcover_WC |>
  mutate(igbp_land_use = factor(igbp_land_use, levels = land_use_summary_WC$igbp_land_use)) |>
  arrange(igbp_land_use)

df_landcover_table_detail_WC <- df_landcover_WC |>
  arrange(desc(pet_p_cond)) |>
  mutate(
    AI_cond = round(pet_p_cond, 3),
    CTI     = round(cti, 3),
    High_Aridity = pet_p >=2,
    High_CTI = cti >=7
  ) |>
  select(
    # AI,
    AI_cond,
    LandCover = igbp_land_use,
    CTI,
    High_Aridity,
    High_CTI,
    Sitename = sitename
  )

df_landcover_table_summary_WC <- land_use_summary_WC |>
  rename(
    LandCover = igbp_land_use,
    Count = n,
    Percent = percent
  )


write_xlsx(
  list(
    Detail_NC = df_landcover_table_detail_NC,
    Detail_WC = df_landcover_table_detail_WC,
    Summary_NC = df_landcover_table_summary_NC,
    Summary_WC = df_landcover_table_summary_WC
  ),
  path = here::here("analysis", "tables", "Landcover_Aridity_Comparison_Top10percent.xlsx")
)





