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

readr::write_csv(df_cond_mean_ann, file = here::here("data/df_cond_mean_ann.csv"))
