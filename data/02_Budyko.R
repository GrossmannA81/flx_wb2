

library(dplyr)
library(readr)


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
      aet_p,
      pet_p_cond,
      aet_p_cond
    )
  )





##------------------------Budyko WITHOUT Condensation::----------------------

## Fu Equation------
fun_fu_equation <- function(pet_p, omega) {
  1 + pet_p -(1 + pet_p ^omega)^(1/omega)
}


#fit of non-linear equation
out_nocond <- nls(
  aet_p ~ 1 + pet_p -(1 + pet_p ^omega)^(1/omega),
  data = df_budyko,
  start = list(omega = 2)     #non-linear least squares: 2 is start value: essential for convergence
)

#calculate residuals (only model oriented)---------

df_budyko <- df_budyko |>
  mutate(
    res = residuals(out_nocond)
  )



##-----------------------Budyko WITH Condensation::------------------------

## Fit Fu Equation------
fun_fu_equation_cond <- function(pet_p_cond, omega) {
  1 + pet_p_cond -(1 + pet_p_cond ^omega)^(1/omega)
}

#Nonlinear modelling (non log which would be lm(log(aet_p_cond) ~log(fun_fu_equation))
out_cond <- nls(
  aet_p_cond ~ 1 + pet_p_cond -(1 + pet_p_cond ^omega)^(1/omega),
  data = df_budyko,
  start = list(omega = 2) #start value of parameter
)

# > summary(out_cond)
#
# Formula: aet_p_cond ~ 1 + pet_p_cond - (1 + pet_p_cond^omega)^(1/omega)
#
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)
# omega  2.15572    0.08749   24.64   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2705 on 224 degrees of freedom
# 0.2705 = sum of squared residuals
# Number of iterations to convergence: 4
# Achieved convergence tolerance: 9.234e-07
#https://www.youtube.com/watch?v=ViWhcq72laU

#> coef(out_cond)
#omega
#2.155718

#calculate residuals (only model oriented)---------

df_budyko <- df_budyko |>
  mutate(
    res_cond = residuals(out_cond)
  )



# further analysis with df_budyko in ANALYSIS folder



##---------------Calculate Delta (Epsilon Deviation)::------------


df_budyko <- df_budyko |>
  mutate(
    delta_cond   = aet_p_cond - fun_fu_equation_cond(pet_p_cond, omega = coef(out_cond)),
    delta_nocond = aet_p - fun_fu_equation(pet_p, omega = coef(out_nocond))

  )


readr::write_csv(df_budyko, file = here::here("data/df_budyko.csv"))






