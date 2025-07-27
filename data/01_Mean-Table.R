
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
install.packages("remotes")
remotes::install_github("geco-bern/cwd")
#install.packages('cwd')
#library(cwd)
library(rsofun)
remotes::install_github("stineb/rsofun")

install.packages('gghighlight')
library(gghighlight)
install.packages('cowplot')
library(cowplot)





path <- "/data_2/FluxDataKit/v3.4/zenodo_upload/fluxnet/"
fun_read_onesite <- function(site, path){
  filename <- list.files(path = path,
                         pattern = paste0("FLX_", site, "_FLUXDATAKIT_FULLSET_DD"),
                         full.names = TRUE
  )
  out <- read_csv(filename) |>
    mutate(sitename = site)
  return(out)
}



# read all daily data for the selected sites
daily_data <- purrr::map_dfr(
  df_sites$sitename,
  ~fun_read_onesite(., path = path)
)




#function for converting latent energy to et

# because cwd is not supported in new version anymore and rsofun does not contain enthalpy_vap and density_h2o:

#enthalpy_vap λ=(2.501−0.002361⋅T)×10^6(in J/kg): according to Henderson-Sellers (1984):
fun_calc_enthalpy_vap <- function(temp_C) {
  lambda <- (2.501 - 0.002361 * temp_C) * 1e6  # J/kg
  return(lambda)
}

fun_calc_density_h2o <- function(temp_C, patm_Pa) {
  patm_kPa <- patm_Pa / 1000
  rho <- 1000 * (1 - ((temp_C + 288.9414) / (508929.2 * (temp_C + 68.12963))) * (temp_C - 3.9863)^2)
  return(rho)  # in kg/m³
}


fun_l_energy_to_et <- function(le_W_m2, temp_C, patm_Pa) {
  lambda <- fun_calc_enthalpy_vap(temp_C)         # J/kg
  et_kg_m2_s <- le_W_m2 / lambda                  # kg m-2 s-1
  et_mm_day <- et_kg_m2_s * 86400                 # mm/day
  return(et_mm_day)
}

fun_calc_pet <- function(netrad, temp_C, patm_Pa, alpha = 1.26) {
  lambda <- fun_calc_enthalpy_vap(temp_C)  # J/kg
  pet_kg_m2_s <- alpha * netrad / lambda
  pet_mm_day <- pet_kg_m2_s * 86400  # s/day -> mm/day (weil 1kg/m2 = 1mm)
  return(pet_mm_day)
}

# fun_l_energy_to_et <- function(le, tc, patm){
#   1000 * 60 * 60 * 24 * le / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
# }





##Annual Means:

annual_means_data <- daily_data |>
  mutate(
    # convert latent heat flux into mass flux in mm day-1
    # LE_F_MDS = latent energy flux
    #TA_F_MDS = Temperature to determine the latent heat of vaporization
    #PA_F = Air pressure for conversion from energy flux to water flux
#    LE_CORR_filled = ifelse(is.na(LE_CORR), LE_F_MDS, LE_CORR),
    le_mm = fun_l_energy_to_et(LE_F_MDS, TA_F_MDS, PA_F),    #Umwandlung von Latentwärmefluss in ET in mm pro Tag
    le_mm_corr = fun_l_energy_to_et(LE_CORR, TA_F_MDS, PA_F), # Umwandlung mit korrigierten latenten Wärmeflüssen in mm pro Tag
    pet = fun_calc_pet(NETRAD, TA_F_MDS, PA_F)  # Umrechnung von W/m² zu mm
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  group_by(sitename, year) |>
  summarise(
    prec = sum(P_F, na.rm = TRUE),
    aet = sum(le_mm, na.rm = TRUE),
    aet_corr = sum(le_mm_corr, na.rm = TRUE),
    pet = sum(pet, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(sitename) |>
  summarise(
    prec = mean(prec, na.rm = TRUE),
    aet = mean(aet, na.rm = TRUE),
    aet_corr = mean(aet_corr, na.rm = TRUE),
    pet = mean(pet, na.rm = TRUE),
    .groups = "drop"

  )

df_sites <- df_sites |>
#  select(-aet, -aet_corr, -pet, -prec) |>  # entfernt alte Werte
 # left_join(annual_means_data, by = "sitename") |>
  mutate(
    prec_cond = prec + cond_mean_ann,
    pet_over_prec = pet / prec,
    aet_over_prec = aet / prec,
    aet_corr_over_prec = aet_corr / prec,
    aet_corr_over_prec_cond = aet_corr / prec_cond,
    aet_over_prec_cond = aet / prec_cond,
    pet_over_prec_cond = pet / prec_cond
  )

#pet will not be used with corrected le_corr because is only modelled with radiation, temp and pressure



# df_sites is main-table for further use with necessary variables::
df_sites <- df_sites |>
  select(
    sitename,
    canopy_height,
    igbp_land_use,
    whc,
    koeppen_code,
    product,
    mat,
    p_over_pet,
    cti,
    cond_mean_ann,
    aet,
    aet_corr,
    prec,
    prec_cond,
    pet,
    pet_over_prec,
    pet_over_prec_cond,
    aet_over_prec,
    aet_over_prec_cond,
    aet_corr_over_prec,
    aet_corr_over_prec_cond
  )

readr::write_csv(df_sites, file = here::here("flx_wb2/flx_wb2/data/df_sites.csv"))
