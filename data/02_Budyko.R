

library(dplyr)
library(readr)


df_budyko <- df_sites |>
  transmute(
    sitename,
    pet_p = pet_over_prec,
    aet_p = aet_over_prec,
    aet_corr_p_cond  = aet_corr_over_prec_cond,
    aet_corr_p = aet_corr_over_prec,
    aet_p_cond = aet_over_prec_cond,
    pet_p_cond = pet_over_prec_cond

  ) |>
  filter(
    complete.cases(across(everything())),
    across(-sitename, ~ . > 0.01) |> reduce(`&`)
  )







## Fu Equation------
fun_fu_equation <- function(pet_p, omega) {
  1 + pet_p -(1 + pet_p ^omega)^(1/omega)
}




##------------------------Budyko WITHOUT Condensation::----------------------



### LE_F_MDS:

#fit of non-linear equation
out_nocond <- nls(
  aet_p ~ fun_fu_equation(pet_p, omega),
  data = df_budyko,
  start = list(omega = 2)     #non-linear least squares: 2 is start value: essential for convergence
)

#calculate residuals (only model oriented)---------

df_budyko <- df_budyko |>
  mutate(
    res = residuals(out_nocond)
  )




### LE_CORR:

#fit of non-linear equation
out_nocond_corr <- nls(
  aet_corr_p ~ fun_fu_equation(pet_p, omega),
  data = df_budyko,
  start = list(omega = 2)     #non-linear least squares: 2 is start value: essential for convergence
)

#calculate residuals (only model oriented)---------

df_budyko <- df_budyko |>
  mutate(
    res_corr = residuals(out_nocond_corr)
  )

# > summary(out_nocond_corr)
# > summary(out_nocond)
#
# Formula: aet_p ~ 1 + pet_p - (1 + pet_p^omega)^(1/omega)
#
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)
# omega   3.4315     0.8367   4.101 5.76e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.6722 on 224 degrees of freedom
#
# Number of iterations to convergence: 7
# Achieved convergence tolerance: 2.261e-06





##-----------------------Budyko WITH Condensation::------------------------


### LE_F_MDS:
#Nonlinear modelling (non log which would be lm(log(aet_p_cond) ~log(fun_fu_equation))
out_cond <- nls(
  aet_p_cond ~ fun_fu_equation(pet_p_cond, omega),
  data = df_budyko,
  start = list(omega = 2) #start value of parameter
)


#> coef(out_cond)
#omega
#2.155718

#calculate residuals (only model oriented)---------

df_budyko <- df_budyko |>
  mutate(
    res_cond = residuals(out_cond)
  )

### LE_CORR:
out_cond_corr <- nls(
  aet_corr_p_cond ~ fun_fu_equation(pet_p_cond, omega),
  data = df_budyko,
  start = list(omega = 2) #start value of parameter
)

df_budyko <- df_budyko |>
  mutate(
    res_corr_cond = residuals(out_cond_corr)
  )

# further analysis with df_budyko in ANALYSIS folder



##---------------Calculate Delta (Epsilon Deviation)::------------


df_budyko <- df_budyko |>
  mutate(
    delta_cond   = aet_p_cond - fun_fu_equation_cond(pet_p_cond, omega = coef(out_cond)),
    delta_cond_corr = aet_corr_p_cond - fun_fu_equation_cond(pet_p_cond, omega = coef(out_cond_corr)),
    delta_nocond = aet_p - fun_fu_equation(pet_p, omega = coef(out_nocond)),
    delta_nocond_corr = aet_corr_p - fun_fu_equation(pet_p, omega = coef(out_nocond_corr))

  )


readr::write_csv(df_budyko, file = here::here("flx_wb2/flx_wb2/data/df_budyko.csv"))






