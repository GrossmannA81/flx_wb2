library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(gghighlight)
library(cowplot)
library(optimx)
library(remotes)
# remotes::install_github("geco-bern/cwd")
library(cwd)
# remotes::install_github("geco-bern/FluxDataKit")
library(FluxDataKit)

# Get site meta info -----------------
sites <- FluxDataKit::fdk_site_info |>
  # readr::read_csv("~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv") |>
  filter(!(sitename %in% c("MX-Tes", "US-KS3"))) |>  # failed sites
  left_join(
    FluxDataKit::fdk_site_fullyearsequence |>
      distinct(sitename, .keep_all = TRUE),
    # readr::read_csv("~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv"),
    by = join_by(sitename)
  ) |>
  filter(!drop_le) |>  # where no full year sequence was found
  filter(nyears_le >= 3)

# Load data -------------
path <- "~/data_2/FluxDataKit/v3.4/zenodo_upload/fluxnet/"  # adjust for your own local use
read_onesite <- function(site, path){
  filename <- list.files(path = path,
                         pattern = paste0("FLX_", site, "_FLUXDATAKIT_FULLSET_DD"),
                         full.names = TRUE
  )
  out <- read_csv(filename) |>
    mutate(sitename = site)
  return(out)
}

# read all daily data for the selected sites
ddf <- purrr::map_dfr(
  sites$sitename,
  ~read_onesite(., path)
)

# Select data sequences ----------------
tmp <- ddf |>
  left_join(
    sites |>
      select(
        sitename,
        year_start = year_start_le,
        year_end = year_end_le),
    by = join_by(sitename)
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  filter(year >= year_start & year <= year_end) |>
  select(-year_start, -year_end, -year)

le_to_et <- function(le, tc, patm){
  1000 * 60 * 60 * 24 * le / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
}

# Get mean annual X
adf <- ddf |>
  mutate(
    # convert latent heat flux into mass flux in mm day-1
    le_mm = le_to_et(LE_F_MDS, TA_F_MDS, PA_F),   # LE_CORR
    pet = 60 * 60 * 24 * cwd::pet(NETRAD, TA_F_MDS, PA_F)
  ) |>
  mutate(year = year(TIMESTAMP)) |>

  # get annual sum per site and year
  group_by(sitename, year) |>
  summarise(
    prec = sum(P_F),
    aet = sum(le_mm),
    pet = sum(pet)
  ) |>
  ungroup() |>

  # get mean annual values per site
  group_by(sitename) |>
  summarise(
    prec = mean(prec, na.rm = TRUE),
    aet = mean(aet, na.rm = TRUE),
    pet = mean(pet, na.rm = TRUE)
  ) |>
  filter(!is.nan(pet))

# Add mean annual condensation from rsofun model outputs
df_pmodel <- readr::read_csv(here::here("data/df_cond_mean_ann_2.csv"))

adf <- adf |>
  left_join(
    df_pmodel,
    by = join_by(sitename)
  )

adf <- adf |>
  mutate(prec_cond = prec + cond_mean_ann)

# Add CTI to data
df_sites_cti <- readr::read_csv(here::here("data/df_sites.csv")) |>
  select(sitename, cti)

adf <- adf |>
  left_join(
    df_sites_cti,
    by = join_by(sitename)
  )

# remove
adf <- adf |>
  tidyr::drop_na(prec_cond, cti)

# Fit Fu equation ----------------
fu_equation <- function(pet_p, omega){
  1 + pet_p - (1 + pet_p^omega)^(1 / omega)
}

## No condensation -----------
adf_nocond <- adf |>
  mutate(pet_p = pet/prec, aet_p = aet/prec)

out_nocond <- nls(
  aet_p ~  1 + pet_p - (1 + pet_p^omega)^(1 / omega),
  data = adf_nocond,
  start = list(omega = 2)
)

# add residuals to fit
adf_nocond <- adf_nocond |>
  mutate(res = residuals(out_nocond))

write_rds(adf_nocond, here::here("data/adf_nocond.rds"))

## With condensation, ... -----------
adf_cond <- adf |>
  mutate(pet_p = pet/prec_cond, aet_p = aet/prec_cond) |>
  tidyr::drop_na(aet_p, pet_p)

out_cond <- nls(
  aet_p ~  1 + pet_p - (1 + pet_p^omega)^(1 / omega),
  data = adf_cond,
  start = list(omega = 2)
)

# add residuals to fit
adf_cond <- adf_cond |>
  mutate(res = residuals(out_cond))

write_rds(adf_cond, here::here("data/adf_cond.rds"))

# Plot --------------
## Budyko ---------
gg2 <- adf_cond |>
  ggplot(
    aes(
      x = pet_p,
      y = aet_p,
      color = cti
    )
  ) +
  geom_function(fun = fu_equation, args = list(omega = coef(out_cond)), color = "tomato") +
  geom_point() +
  scale_color_viridis_c(option = "B", direction = -1) +
  # khroma::scale_color_romaO() +
  # gghighlight(
  #   sitename %in% c("DE-Hai", "US-Ton", "FI-Hyy", "US-ICh", "AU-How"),
  #   label_key = sitename,
  #   use_direct_label = FALSE,
  #   unhighlighted_params = list(color = "grey40")
  # ) +
  # geom_label(aes(label = sitename),
  #            hjust = 1, vjust = 1, fill = "white", colour = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  ylim(0, 1.8) +
  labs(
    x = expression(paste("PET/", italic(P))),
    y = expression(paste("AET/", italic(P)))
  )

gg2

ggsave(here::here("fig/budyko_fluxnet.png"), width = 8, height = 3.5)

## Residuals
adf_cond |>
  ggplot() +
  geom_histogram(aes(res, ..count..), color = "black", fill = "grey70") +
  theme_classic()

# log-transformed
adf_cond |>
  ggplot() +
  geom_histogram(aes(log(res), ..count..), color = "black", fill = "grey70") +
  theme_classic()

# Analysis of rediduals --------------------------------------------------------
adf_cond <- adf_cond |>
  left_join(
    sites |>
      select(sitename, vegtype = igbp_land_use),
    by = join_by(sitename)
  )

# How can we explain variation in the residual (res). Related to CTI and pet_p?

## Linear model ----------------------------------------------------------------
# No strongly significant effect of CTI
linmod <- lm(res ~ pet_p + cti, data = adf_cond)
summary(linmod)

## Non-linear with interactions ------------------------------------------------
library(splines)
lm(res ~ bs(pet_p, df = 3) * bs(cti, df = 2) + factor(vegtype), data = adf_cond)
summary(linmod)

## GAM --------------
library(mgcv)
gammod <- gam(res ~ s(pet_p) + s(cti), data = adf_cond)
summary(gammod)

## Random forest --------------
# Gets an R-squared of 0.3733181 - may include Pelletier sediment thickness
library(caret)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
  )

#### With veg type and CTI -----------
set.seed(123)
rf_model <- train(
  res ~ pet_p + cti + vegtype,
  data = adf_cond,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = 3, #c(2, 3),
    .min.node.size = 3, #c(3, 5, 7, 9),
    .splitrule = "extratrees" # c("extratrees", "variance")
    ),
  metric = "RMSE",
)

# The final values used for the model were mtry = 3, splitrule = extratrees and min.node.size = 7.
print(rf_model)

#### Without CTI -----------
# gets R-squared of 0.2666482 - much lower than with CTI
set.seed(123)
rf_model <- train(
  res ~ pet_p + vegtype,
  data = adf_cond,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = 2, #c(1,2),
    .min.node.size = 5, #c(3, 5, 7, 9),
    .splitrule = "extratrees" #c("extratrees", "variance")
  ),
  metric = "RMSE",         # optimize for AUC
  tuneLength = 5          # try 5 different mtry values
)

# The final values used for the model were mtry = 3, splitrule = extratrees and min.node.size = 7.
print(rf_model)


#### Without veg type -----------
# pretty weak
set.seed(123)
rf_model <- train(
  res ~ pet_p + cti,
  data = adf_cond,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = 1, #c(1,2),
    .min.node.size = 9, # c(3, 5, 7, 9),
    .splitrule = "extratrees"  #c("extratrees", "variance")
  ),
  metric = "RMSE",         # optimize for AUC
  tuneLength = 5          # try 5 different mtry values
)

# The final values used for the model were mtry = 3, splitrule = extratrees and min.node.size = 7.
print(rf_model)

## Visual ----------
### Heatmaps --------
# Very clear pattern!
adf_cond_binned <- adf_cond |>
  mutate(
    cti_bin = cut(cti, breaks = quantile(cti, probs = seq(0, 1, 1/5)), include.lowest = TRUE),
    pet_p_bin = cut(pet_p, breaks = quantile(pet_p, probs = seq(0, 1, 1/5)), include.lowest = TRUE)
  ) |>
  group_by(cti_bin, pet_p_bin) |>
  summarise(
    res = mean(res),
    count = n(),
    .groups = "drop"
  )

common_heatmap_scale <- scale_fill_gradient2(
  low = "darkslategrey",
  mid = "beige",
  high = "hotpink4",
  midpoint = 0,
  name = expression(epsilon*"′ (Mean Deviation)"),
  limits = c(-0.2, 0.2),
  labels = scales::label_number(accuracy = 0.1),
  breaks = seq(-0.2, 0.2, by = 0.1)
)

gg_cti_heat_nocond <- adf_cond_binned |>
  ggplot(
    aes (
      x = pet_p_bin,
      y = cti_bin,
      fill = res)) +
  geom_tile(color = "white") +
  common_heatmap_scale +
  labs(
    x = "Aridity Classes (PET/P)",
    y = "CTI-Classes",
    title = "Residuals versus aridity and topography"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0)
  )

gg_cti_heat_nocond

gg_counts <- adf_cond_binned |>
  ggplot(
    aes (
      x = pet_p_bin,
      y = cti_bin,
      fill = count)) +
  scale_fill_viridis_c(breaks = seq(0, 13, by = 1)) +
  geom_tile(color = "white") +
  labs(
    x = "Aridity Classes (PET/P)",
    y = "CTI-Classes",
    title = "Residuals versus aridity and topography"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0)
  )

gg_counts

