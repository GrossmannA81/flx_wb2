
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
install.packages('cwd')
remotes::install_github("geco-bern/cwd")
library(cwd)
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

# Select data sequences ----------------
# daily_data <- daily_data |> # former ddf
#   left_join(
#     df_sites |>
#       select(
#         sitename,
#         year_start = year_start_lecorr,
#         year_end = year_end_lecorr),
#     by = join_by(sitename)
#   ) |>
#   mutate(year = year(TIMESTAMP)) |>
#   filter(year >= year_start & year <= year_end) |>
#   select(-year_start, -year_end, -year)


#function for converting latent energy to et
fun_l_energy_to_et <- function(le, tc, patm){
  1000 * 60 * 60 * 24 * le / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
}





##Annual Means with latent energy corrected:

annual_means_data <- daily_data |>
  mutate(
    # convert latent heat flux into mass flux in mm day-1
    # LE_F_MDS = latent energy flux
    #TA_F_MDS = Temperature to determine the latent heat of vaporization
    #PA_F = Air pressure for conversion from energy flux to water flux
    le_mm = fun_l_energy_to_et(LE_F_MDS, TA_F_MDS, PA_F),    #Umwandlung von Latentwärmefluss in ET in mm pro Tag
    le_mm_corr = fun_l_energy_to_et(LE_CORR, TA_F_MDS, PA_F), # Umwandlung mit korrigierten latenten Wärmeflüssen in mm pro Tag
    pet = 60 * 60 * 24 * cwd::pet(NETRAD, TA_F_MDS, PA_F)  # Umrechnung von W/m² zu mm
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  group_by(sitename, year) |>
  summarise(
    prec = sum(P_F),
    aet = sum(le_mm),
    aet_corr = sum(le_mm_corr),
    pet = sum(pet)
  ) |>
  ungroup() |>                #Mittelwerte pro site
  group_by(sitename) |>
  summarise(
    prec = mean(prec, na.rm = TRUE),
    #aet = mean(aet, na.rm = TRUE),
    aet_corr = mean(aet_corr, na.rm = TRUE),
    pet = mean(pet, na.rm = TRUE),
    pet_over_prec = pet/prec,
    aet_over_prec = aet_corr/prec
  )

df_sites <- df_sites |>
  left_join(
    annual_means_data,
    by="sitename"
  )



df_sites <- df_sites |>       #add condensation
  mutate(
    aet_cond = aet_corr + cond_mean_ann,
    prec_cond = prec + cond_mean_ann,
    pet_over_prec_cond = pet/prec_cond,
    aet_over_prec_cond = aet_corr/prec_cond
    #pet will not be used with corrected le_corr because is only modelled with radiation, temp and pressure
  )


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
    aet_cond,
    prec,
    prec_cond,
    pet_over_prec,
    pet_over_prec_cond,
    aet_corr,
    aet_cond,
    aet_over_prec,
    aet_over_prec_cond
  )


readr::write_csv(df_sites, file = here::here("data/df_sites.csv"))
