

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



#Plot LE_F_MDS
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




#Plot LE_CORR::

ggplot() +
  geom_raster(data = df_rast, aes(x = x, y = y, fill = thickness)) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_point(data = df_excel_corr, aes(x = lon, y = lat), color = "red", size = 1.5) +
  geom_text_repel(
    data = df_excel_corr,
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
    xlim = range(df_excel_corr$lon, na.rm = TRUE) + c(-2, 2),
    ylim = range(df_excel_corr$lat, na.rm = TRUE) + c(-2, 2),
    expand = FALSE
  ) +
  labs(
    title = "High Aridity and CTI Sites - LE_CORR",
    x = "Longitude",
    y = "Latitude",
    fill = "Thickness"
  ) +
  theme_minimal() -> p


ggsave(
  filename = here::here("analysis/pics/Pelletier_LE_CORR.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)



# plot(rast_pelletier_avg, main = "High Aridity and CTI Sites - LE_CORR")
# points(df_excel_corr$lon, df_excel_corr$lat, pch = 19, col = "red")






