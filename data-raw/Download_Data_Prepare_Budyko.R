




install.packages("dplyr")
library(dplyr)
install.packages("terra")
library(terra)
install.packages("remotes")
library(remotes)
remotes::install_github("https://github.com/geco-bern/FluxDataKit")
library(FluxDataKit)

#install.packages("rsofun")
remotes::install_github("geco-bern/rsofun")
library(rsofun)

library(tidyverse)
library(readr)




## Get site infos and fullyearsequences from FluxDataKit v.3.4

df_sites <- readr::read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv") |>
  filter(!(sitename %in% c("MX-Tes", "US-KS3"))) |>  # failed sites
  left_join(
    readr::read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv"),
    by = "sitename"
  )


df_fullyear <- readr::read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv")


##ADD CTI Data
cti_all_data <- rast("/data/archive/gti_marthews_2015/data/ga2.nc")

# extract needed Value from cti_all_data
extracted_cti <- terra::extract(
  cti_all_data,
  df_sites |> dplyr::select(lon, lat)
)

df_sites <- df_sites |>
  dplyr::mutate(cti = extracted_cti[[2]])



