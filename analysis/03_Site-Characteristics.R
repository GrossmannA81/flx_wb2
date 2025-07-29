

df_facet_map <- df_budyko |>
  select(
    sitename,
    pet_p,
    pet_p_cond,
    cti,
    res,
    res_cond,
    res_corr_cond
    )


gg_cti_ai_diagram <- df_facet_map |>
  filter(pet_p_cond <= 7) |>
  ggplot(aes(x = pet_p_cond, y = cti)) +
  geom_point(aes(color = res_cond), size = 2) +
  scale_color_gradient2(
    low = "darkslategrey",
    mid = "beige",
    high = "hotpink4",
    midpoint = 0,
    limits = c(-0.4, 1.4),
    breaks = seq(-0.4, 1.4, by = 0.4),
    labels = scales::label_number(accuracy = 0.1),
    name = expression(epsilon*"′")
  ) +
  labs(
    y = "CTI",
    x = "Aridity Index",
    title = "ε′ from Budyko - LE_F_MDS",
    color = expression(epsilon*"′")
  ) +
  theme_classic() +
  theme(
    legend.position = "right"
  )


# Show plot
plot(gg_cti_ai_diagram)


gg_cti_ai_diagram_cond <- df_facet_map |>
  filter(pet_p_cond <= 7) |>
  ggplot(aes(x = pet_p_cond, y = cti)) +
  geom_point(aes(color = res_corr_cond), size = 2) +
  scale_color_gradient2(
    low = "darkslategrey",
    mid = "beige",
    high = "hotpink4",
    midpoint = 0,
    limits = c(-0.4, 1.4),
    breaks = seq(-0.4, 1.4, by = 0.4),
    labels = scales::label_number(accuracy = 0.1),
    name = expression(epsilon*"′")
  ) +
  labs(
    y = "CTI",
    x = "Aridity Index",
    title = "ε′ from Budyko - LE_CORR",
    color = expression(epsilon*"′")
  ) +
  theme_classic() +
  theme(
    legend.position = "right"
  )

plot(gg_cti_ai_diagram_cond)


gg_cti_ai_combined <- gg_cti_ai_diagram + gg_cti_ai_diagram_cond

print(gg_cti_ai_combined)

# Save plot
ggsave(
  filename = here::here("analysis/pics/AI_CTI_Diagram_ALL.png"),
  plot = gg_cti_ai_diagram,
  width = 10,
  height = 6
)





###-------------------EXCEL with Condensation:---------------------------

library(dplyr)
library(writexl)


#LE_F_MDS:::

df_excel <- df_budyko |>
  filter(
    cti_class %in% c("Medium", "High", "Very High"),
    aridity_class %in% c("SA", "A"),
    res_cond > 0,
  ) |>
  select(
    sitename,
    igbp_land_use,
    cti,
    res_cond,
    aridity_class,
    cti_class
  )|>
  arrange(desc(cti))



write_xlsx(
  list("filtered_sites_cti_ranking" = df_excel),
  path = "analysis/tables/table_topsites.xlsx"
)



#LE_CORR:::

df_excel_corr <- df_budyko |>
  filter(
    cti_class %in% c("Medium", "High", "Very High"),
    aridity_class %in% c("SA", "A"),
    res_corr_cond > 0,
  ) |>
  select(
    sitename,
    igbp_land_use,
    cti,
    res_corr_cond,
    aridity_class,
    cti_class
  )|>
arrange(desc(cti))


write_xlsx(
  list("filtered_sites_with_ranking_corrected" = df_excel_corr),
  path = "analysis/tables/table_topsites_corr.xlsx"
)


print(df_excel)
print(df_excel_corr)




###----------------------------PLOTS-PELLETIER:--------------------------


install.packages("terra")
library(terra)
install.packages("sf")
library(sf)
library (ggplot2)
install.packages("viridis")
library(viridis)
library(readr)
library(here)
install.packages("ggrepel")
library(ggrepel)

#include lon/lat
df_site_coords <- read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv") |>
  select(sitename, lon, lat) |>
  filter(!(sitename %in% c("MX-Tes", "US-KS3")))

df_budyko <- df_budyko |>
  left_join(df_site_coords, by = "sitename")



rast_pelletier_avg <- rast ("/data/archive/soil_pelletier_2016/data/Global_Soil_Regolith_Sediment_1304/data/average_soil_and_sedimentary-deposit_thickness.tif")
# plot(rast_pelletier_avg)


extract_coords <- df_budyko |>
  dplyr::select(sitename, lon, lat) |>
  distinct()


thickness_values <- terra::extract(rast_pelletier_avg, extract_coords |>
                                     dplyr::select(lon, lat))

df_thickness <- bind_cols(extract_coords, thickness = thickness_values [, 2])


df_excel <- df_excel |>
  left_join(df_thickness, by = "sitename")

df_excel_corr <- df_excel_corr |>
  left_join(df_thickness, by = "sitename")

# plot(rast_pelletier_avg, main = "High Aridity and CTI Sites - LE_CORR")
# points(df_excel_corr$lon, df_excel_corr$lat, pch = 19, col = "red")




rast_lowres <- terra::aggregate(rast_pelletier_avg, fact = 10)

df_rast <- as.data.frame(rast_lowres, xy = TRUE, na.rm = TRUE)
colnames(df_rast)[3] <- "thickness"


ggplot() +
  geom_raster(data = df_rast, aes(x = x, y = y, fill = thickness)) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_point(data = df_excel, aes(x = lon, y = lat), color = "red", size = 1.5) +
  geom_text_repel(
    data = df_excel,
    aes(
      x = lon,
      y = lat,
      label = paste0(sitename, "\n", round(thickness, 1), " m")
    ),
    color = "red",
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = NA
    ) +
  coord_sf(
    xlim = range(df_excel$lon, na.rm = TRUE) + c(-2, 2),
    ylim = range(df_excel$lat, na.rm = TRUE) + c(-2, 2),
    expand = FALSE
  ) +
  labs(
    title = "High Aridity and CTI Sites - LE_F_MDS",
    x = "Longitude",
    y = "Latitude",
    fill = "Thickness"
  ) +
  theme_minimal() -> p


ggsave(
  filename = here::here("analysis/pics/Pelletier_LE_F_MDS.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)




plot(rast_pelletier_avg, main = "High Aridity and CTI Sites - LE_CORR")
points(df_excel_corr$lon, df_excel_corr$lat, pch = 19, col = "red")

ggsave(
  here::here("analysis/pics/....png"),
  width = 8, height = 6, dpi = 300
)




sites_sf <- st_as_sf(
  df_excel,
  coords = c("lon", "lat"),
  crs = 4326)  # WGS84


ggsave(
  here::here("analysis/pics/....png"),
  width = 8, height = 6, dpi = 300
)



#
# rast_pelletier_proj <- project(rast_pelletier_avg, crs(sites_sf))
# rast_pelletier_proj <- aggregate(rast_pelletier_proj, fact = 5)
# rast_pelletier_proj <- crop(rast_pelletier_proj, ext(sites_sf) + 0.1)
#
#
# rast_pelletier_df <- as.data.frame(rast_pelletier_proj, xy = TRUE, na.rm = TRUE)
# names(rast_pelletier_df)[3] <- "thickness"
#


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






plot(rast_pelletier_b_ll, main = "Pelletier Raster + High Aridity Sites")
points(df_landcover_table_detail_WC$lon, df_landcover_table_detail_WC$lat, pch = 19, col = "red")
# text(df_landcover_table_detail_WC$lon, df_landcover_table_detail_WC$lat,
#      labels = df_landcover_table_detail_WC$sitename, pos = 3, cex = 0.6)



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





