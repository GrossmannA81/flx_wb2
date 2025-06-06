
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




###-------------------Landcovertypes, CTI, Canopy Height, MAT and WHC with highest AridityIndex-  TABLE---------------------------


install.packages("writexl")
library(writexl)

#--------------------------------------------------------------------------------------------------------

df_additional_vars <- df_budyko |>
  select(
    sitename,
    canopy_height,
    whc,
    mat
  ) |>
  distinct(sitename, .keep_all = TRUE)

#WITHOUT

pet_p_80th <- quantile(df_budyko$pet_p, 0.8, na.rm = TRUE) #df_budyko -> n = 253 (top 20 = 50-51)


df_landcover_NC <- df_budyko |>
  filter(pet_p >= pet_p_80th) |>
  select(sitename, igbp_land_use, cti, pet_p, pet_p_cond) |>
  left_join(df_additional_vars, by = "sitename")

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
    Canopy = round(canopy_height, 2),
    WHC = round (whc, 2),
    MAT = round (mat, 2),
    High_Aridity = pet_p >=2,
    High_CTI = cti >=7
  ) |>
  select(
    AI,
    LandCover = igbp_land_use,
    CTI,
    Canopy,
    WHC,
    MAT,
    High_Aridity,
    High_CTI,
    sitename
  ) |>
  distinct()


df_landcover_table_summary_NC <- land_use_summary_NC |>
  rename(
    LandCover = igbp_land_use,
    Count = n,
    Percent = percent
  )





#WITH CONDENSATION

pet_p_cond_80th <- quantile(df_budyko$pet_p_cond, na.rm = TRUE, probs = 0.8)

df_landcover_WC <- df_budyko |>
  filter(pet_p_cond >= pet_p_cond_80th) |>
  select(sitename, igbp_land_use, cti, pet_p, pet_p_cond) |>
  left_join(df_additional_vars, by = "sitename")

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
    Canopy = round (canopy_height, 2),
    WHC = round (whc, 2),
    MAT = round (mat, 2),
    High_Aridity = pet_p >=2,
    High_CTI = cti >=7
  ) |>
  select(
    # AI,
    AI_cond,
    LandCover = igbp_land_use,
    CTI,
    Canopy,
    WHC,
    MAT,
    High_Aridity,
    High_CTI,
    sitename
  ) |>
  distinct() |>
  left_join(
    df_sites |>
      select(sitename, lon, lat
      ),
    by="sitename"
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




###----------------------------PLOTS---------------------------

##PELLETIER:

library(terra)
install.packages("sf")
library(sf)
library (ggplot2)
install.packages("viridis")
library(viridis)

rast_pelletier_hs_bv <- rast("/data/archive/soil_pelletier_2016/data/Global_Soil_Regolith_Sediment_1304/data/hill-slope_valley-bottom.tif")

rast_pelletier_b_ll <- rast ("/data/archive/soil_pelletier_2016/data/Global_Soil_Regolith_Sediment_1304/data/upland_valley-bottom_and_lowland_sedimentary_deposit_thickness.tif")

rast_pelletier_avg <- rast ("/data/archive/soil_pelletier_2016/data/Global_Soil_Regolith_Sediment_1304/data/average_soil_and_sedimentary-deposit_thickness.tif")

plot(rast_pelletier_b_ll)


sites_sf <- st_as_sf(
  df_landcover_table_detail_WC,
  coords = c("lon", "lat"),
  crs = 4326)  # WGS84

rast_pelletier_proj <- project(rast_pelletier_avg, crs(sites_sf))
rast_pelletier_proj <- aggregate(rast_pelletier_proj, fact = 5)
rast_pelletier_proj <- crop(rast_pelletier_proj, ext(sites_sf) + 0.1)


rast_pelletier_df <- as.data.frame(rast_pelletier_proj, xy = TRUE, na.rm = TRUE)
names(rast_pelletier_df)[3] <- "thickness"



# ggplot() +
#   geom_tile(data = rast_pelletier_df, aes(x = x, y = y, fill = thickness)) +
#   scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
#   geom_sf(data = sites_sf, color = "red", size = 1) +
#   coord_sf(expand = FALSE) +
#   labs(
#     title = "Pelletier Raster + High Aridity Sites",
#     fill = "Soil Thickness (m)",
#     x = NULL, y = NULL
#   ) +
#   theme_minimal()
#
#
#
#
# plot(rast_pelletier_avg, main = "Pelletier Raster + High Aridity Sites")
# points(df_landcover_table_detail_WC$lon, df_landcover_table_detail_WC$lat, pch = 19, col = "red")
# # text(df_landcover_table_detail_WC$lon, df_landcover_table_detail_WC$lat,
# #      labels = df_landcover_table_detail_WC$sitename, pos = 3, cex = 0.6)
#
#
# plot(rast_pelletier_b_ll, main = "Pelletier Raster + High Aridity Sites")
# points(df_landcover_table_detail_WC$lon, df_landcover_table_detail_WC$lat, pch = 19, col = "red")
# # text(df_landcover_table_detail_WC$lon, df_landcover_table_detail_WC$lat,
# #      labels = df_landcover_table_detail_WC$sitename, pos = 3, cex = 0.6)
#


# AUSTRALIA---------------------------------------------------------------------

extent_aus <- ext(110, 155, -45, -10)

rast_aus <- crop(rast_pelletier_avg, extent_aus) # Crop Raster
rast_aus <- aggregate(rast_aus, fact = 5)

rast_aus_df <- as.data.frame(rast_aus, xy = TRUE, na.rm = TRUE)
names(rast_aus_df)[3] <- "thickness"

sites_aus <- df_landcover_table_detail_WC |>
  filter(lon >= 110, lon <= 155, lat >= -45, lat <= -10)

ggplot() +
  geom_raster(data = rast_aus_df, aes(x = x, y = y, fill = thickness)) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  geom_point(data = sites_aus, aes(x = lon, y = lat), color = "red", size = 2) +
  labs(
    title = "Australia – Soil Thickness (Pelletier)",
    fill = "thickness (m)"
  ) +
  theme_minimal()

ggsave(
  here::here("analysis/pics/Pelletier_AUSTRALIA.png"),
  width = 8, height = 6, dpi = 300
)



#NORTH AMERICA------------------------------------------------------------------
extent_america <- ext(-130, -70, 15, 70)

# Crop Raster
rast_america <- crop(rast_pelletier_avg, extent_america)
rast_america <- aggregate(rast_america, fact = 5)

rast_america_df <- as.data.frame(rast_america, xy = TRUE, na.rm = TRUE)
names(rast_america_df)[3] <- "thickness"

sites_america <- df_landcover_table_detail_WC |>
  filter(lon >= -130, lon <= -70, lat >= 15, lat <= 70)

# Plot
ggplot() +
  geom_raster(data = rast_america_df, aes(x = x, y = y, fill = thickness)) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  geom_point(data = sites_america, aes(x = lon, y = lat), color = "red", size = 2) +
  labs(
    title = "USA – Soil Thickness (Pelletier)",
    fill = "thickness (m)"
  ) +
  theme_minimal()

ggsave(
  here::here("analysis/pics/Pelletier_USA.png"),
  width = 8, height = 6, dpi = 300
)



#SPAIN--------------------------------------------------------------------------
extent_spain <- ext(-10, 5, 35, 45)

# Crop Raster
rast_spain <- crop(rast_pelletier_avg, extent_spain)
# rast_spain <- aggregate(rast_spain, fact = 2) # not necessary for this "small" country

rast_spain_df <- as.data.frame(rast_spain, xy = TRUE, na.rm = TRUE)
names(rast_spain_df) [3] <- "thickness"

sites_spain <- df_landcover_table_detail_WC |>
  filter(lon >= -10, lon <= 5, lat >= 35, lat <= 45)

ggplot() +
  geom_raster(data = rast_spain_df, aes(x = x, y = y, fill = thickness)) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  geom_point(data = sites_spain, aes(x = lon, y = lat), color = "red", size = 2) +
  labs(
    title = "SPAIN – Soil Thickness (Pelletier)",
    fill = "thickness (m)"
  ) +
  theme_minimal()

ggsave(
  here::here("analysis/pics/Pelletier_SPAIN.png"),
  width = 8, height = 6, dpi = 300
)









###---------------------------Statistics---------------------------


lm_site_chr <- lm(AI_cond ~ WHC + Canopy + CTI, data = df_landcover_table_detail_WC)
summary(lm_site_chr)


ggplot(df_lm_chr, aes(x = WHC, y = delta_nocond, color = CTI)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Aridity Index vs. WHC",
       x = "Water Holding Capacity",
       y = "AI",
       color = "CTI")





