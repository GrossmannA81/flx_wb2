
install.packages("rsofun")
library(rsofun)
library(purrr)
library(readr)



driver <- readr::read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds") |>
  dplyr::mutate(fapar_missing = purrr::map_lgl(forcing, ~ all(is.na(.x$fapar)))) |>
  dplyr::filter(!fapar_missing) |>
  dplyr::select(-fapar_missing)


params_modl <- list(
  kphio              = 5.000000e-02, # chosen to be too high for demonstration
  kphio_par_a        =-2.289344e-03,
  kphio_par_b        = 1.525094e+01,
  soilm_thetastar    = 1.577507e+02,
  soilm_betao        = 1.169702e-04,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 20.0,
  kc_jmax            = 0.41
)

# Run the model for these parameters.
output <- rsofun::runread_pmodel_f(
  driver,
  par = params_modl
)



library(tidyverse)

get_annual_cond <- function(df){
  adf <- df |>
    mutate(year = lubridate::year(date)) |>
    group_by(year) |>
    summarise(cond_ann = sum(cond))

  adf |>
    ungroup() |>
    summarise(cond_mean_ann = mean(cond_ann))
}

df_cond_mean_ann <- output |>
  select(sitename, data) |>
  mutate(data = purrr::map(data, ~get_annual_cond(.))) |>
  unnest(data)

readr::write_csv(df_cond_mean_ann, file = here::here("flx_wb2/flx_wb2/data/df_cond_mean_ann_2.csv"))

cond_pmodel <- readr::read_csv(here::here("flx_wb2/flx_wb2/data/df_cond_mean_ann_2.csv"))


df_sites <-
  left_join(
    df_sites,
    cond_pmodel,
    by="sitename"
  )

