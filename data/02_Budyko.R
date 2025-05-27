

library(dplyr)






##------------------------Without Condensation::----------------------

## Fu Equation------
fun_fu_equation <- function(pet_p, omega) {
  1 + pet_p -(1 + pet_p ^omega)^(1/omega)
}

df_budyko <- df_sites |>
  transmute(
    sitename,
    pet_p = pet_over_prec,
    aet_p = aet_over_prec,
    pet_p_cond = pet_over_prec_cond,
    aet_p_cond = aet_over_prec_cond
  ) |>
  filter(
    complete.cases(
      pet_p,
      aet_p)
    )


#fit of non-linear equation
out_nocond <- nls(
  aet_p ~ 1 + pet_p -(1 + pet_p ^omega)^(1/omega),
  data = df_budyko,
  start = list(omega = 2)     #non-linear least squares: 2 is start value: essential for convergence
)

#add residuals into model df_budyko---------

df_budyko <- df_budyko |>
  mutate(
    res = residuals(out_nocond)
  )




##-----------------------With Condensation::------------------------

## Fit Fu Equation------
fun_fu_equation_cond <- function(pet_p_cond, omega) {
  1 + pet_p_cond -(1 + pet_p_cond ^omega)^(1/omega)
}

out_cond <- nls(
  aet_p_cond ~ 1 + pet_p_cond -(1 + pet_p_cond ^omega)^(1/omega),
  data = df_budyko,
  start = list(omega = 2)
)

#add residuals into model df_budyko---------

df_budyko <- df_budyko |>
  mutate(
    res_cond = residuals(out_nocond)
  )

readr::write_csv(df_budyko, file = here::here("data/df_budyko.csv"))

# further analysis with df_budyko in ANALYSIS folder



