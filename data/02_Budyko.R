
library (broom)
library(dplyr)



##------------------------Without Condensation::----------------------

## Fit Fu Equation------
fun_fu_equation <- function(pet_over_prec, omega) {
  1 + pet_over_prec -(1 + pet_over_prec ^omega)^(1/omega)
}

out_nocond <- nls(
  aet_over_prec ~ 1 + pet_over_prec -(1 + pet_over_prec ^omega)^(1/omega),
  data = df_sites,
  start = list(omega = 2)
)

#add residuals to fit---------
df_nocond_augment <- augment(out_nocond, data = df_sites)

df_sites <- left_join(
  df_sites,
  df_nocond_augment |>
    select(
      sitename,
      resid = .resid
    ),
  by = "sitename"
)



##-----------------------With Condensation::------------------------

## Fit Fu Equation------
fun_fu_equation_cond <- function(pet_over_prec_cond, omega) {
  1 + pet_over_prec_cond -(1 + pet_over_prec_cond ^omega)^(1/omega)
}

out_cond <- nls(
  aet_over_prec_cond ~ 1 + pet_over_prec_cond -(1 + pet_over_prec_cond ^omega)^(1/omega),
  data = df_sites,
  start = list(omega = 2)
)

df_cond_augment <- augment(out_cond, data = df_sites)

df_sites <- left_join(
  df_sites,
  df_cond_augment |>
    select(
      sitename,
      resid_cond = .resid
    ),
  by = "sitename"
)


readr::write_csv(df_sites, file = here::here("data/df_sites.csv"))
