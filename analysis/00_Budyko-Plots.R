

library(ggplot2)
library(cowplot)
library(gghighlight)
library(here)


#-------Plot Budyko WITHOUT condensation::--------

gg_bud_nocond <- df_budyko |>
  ggplot(
    aes(
      x = pet_p,
      y = aet_p
    )
  ) +
  geom_function(fun = fun_fu_equation, args = list(omega = coef(out_nocond)), color = "firebrick") +
  geom_point(color = "blue", size= 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 5)+
  ylim(0, 5) +
  labs(
    x = expression(paste("PET/P")),
    y = expression (paste("AET/P")),
    title = "Budyko without Condensation"
  )

plot(gg_bud_nocond)

ggsave(here::here("analysis/pics/Budyko_NoCondensation.png"))






#-------Plot Budyko WITH condensation::--------

gg_bud_cond <- df_budyko |>
  ggplot(
    aes(
      x = pet_p_cond,
      y = aet_p_cond
    )
  ) +
  geom_function(fun = fun_fu_equation_cond, args = list(omega = coef(out_cond)), color = "firebrick") +
  geom_point(color = "blue", size= 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 5)+
  ylim(0, 5) +
  labs(
    x = expression(paste("PET/P")),
    y = expression (paste("AET/P")),
    title = "Budyko with Condensation"
  )

plot(gg_bud_cond)
ggsave(here::here("analysis/pics/Budyko_Condensation.png"))


cowplot::plot_grid(gg_bud_nocond, gg_bud_cond)
ggsave(here::here("analysis/pics/Budyko_Cond_Comparison.png"))

