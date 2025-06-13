

library(ggplot2)
library(cowplot)
library(gghighlight)
library(here)


#-------Plot Budyko WITHOUT condensation::--------

# gg_bud_nocond <- df_budyko |>
#   ggplot(
#     aes(
#       x = pet_p,
#       y = aet_p
#     )
#   ) +
#   geom_function(fun = fun_fu_equation, args = list(omega = coef(out_nocond)), color = "firebrick") +
#   geom_point(color = "blue", size= 1) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
#   geom_hline(yintercept = 1, linetype = "dotted") +
#   theme_classic() +
#   xlim(0, 5)+
#   ylim(0, 5) +
#   labs(
#     x = expression(paste("PET/P")),
#     y = expression (paste("AET/P")),
#     title = "Budyko without Condensation"
#   )
#
# plot(gg_bud_nocond)
#
# ggsave(here::here("analysis/pics/Budyko_NoCondensation.png"))




bud_nocond_total <- nrow(df_budyko)
bud_nocond_red <- sum(df_budyko$aet_p > 1)
share_nocond_red <- round(bud_nocond_red / bud_nocond_total * 100, 1)



gg_bud_nocond <- df_budyko |>
  ggplot(
    aes(
      x = pet_p,
      y = aet_p,
      color = aet_p > 1
    )
  ) +
  geom_function(fun = fun_fu_equation, args = list(omega = coef(out_nocond)), color = "black") +
  geom_point(size = 1) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "firebrick")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 5) +
  ylim(0, 5) +
  labs(
    x = expression(paste("PET/P")),
    y = expression(paste("AET/P")),
    color = "AET/P > 1",
    title = paste0("Budyko without Condensation (",
                   bud_nocond_red, " sites (", share_nocond_red, "%) with AET/P > 1)")
  )


plot(gg_bud_nocond)

ggsave(here::here("analysis/pics/Budyko_NoCondensation.png"))




#-------Plot Budyko WITH condensation::--------

# gg_bud_cond <- df_budyko |>
#   ggplot(
#     aes(
#       x = pet_p_cond,
#       y = aet_p_cond
#     )
#   ) +
#   geom_function(fun = fun_fu_equation_cond, args = list(omega = coef(out_cond)), color = "firebrick") +
#   geom_point(color = "blue", size= 1) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
#   geom_hline(yintercept = 1, linetype = "dotted") +
#   theme_classic() +
#   xlim(0, 5)+
#   ylim(0, 5) +
#   labs(
#     x = expression(paste("PET/P")),
#     y = expression (paste("AET/P")),
#     title = "Budyko with Condensation"
#   )

#-----------------------------------------------------------------

bud_total <- nrow(df_budyko)
bud_red <- sum(df_budyko$aet_p_cond > 1)
share_red <- round(bud_red / bud_total * 100, 1)



gg_bud_cond <- df_budyko |>
  ggplot(
    aes(
      x = pet_p_cond,
      y = aet_p_cond,
      color = aet_p_cond > 1
    )
  ) +
  geom_function(fun = fun_fu_equation_cond, args = list(omega = coef(out_cond)), color = "black") +
  geom_point(size = 1) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "firebrick")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 5) +
  ylim(0, 5) +
  labs(
    x = expression(paste("PET/P")),
    y = expression(paste("AET/P")),
    color = "AET/P > 1",
    title = "Budyko with Condensation",
    subtitile = past0(bud_red, " sites (", share_red, "%) with AET/P > 1)")
  )




plot(gg_bud_cond)
ggsave(here::here("analysis/pics/Budyko_Condensation.png"))


cowplot::plot_grid(gg_bud_nocond, gg_bud_cond)
ggsave(here::here("analysis/pics/Budyko_Cond_Comparison.png"),
       width = 10,
       height = 5,
       dpi= 300,
       limitsize = FALSE
)






#-----------HISTOGRAMS RESIDUALS::---------------------------



# with Condensation:
gg_bud_hist_cond <- df_budyko |>
  ggplot() +
  geom_histogram(aes(res_cond, ..count..), color = "black", fill = "grey70") +
  labs(title = "Residuals With Condensation") +
theme_classic()


# whitout Condensation:
gg_bud_hist_nocond <- df_budyko |>
  ggplot() +
  geom_histogram(aes(res, ..count..), color = "black", fill = "grey70") +
  labs(title = "Residuals No Condensation") +
theme_classic()

cowplot::plot_grid(gg_bud_hist_nocond, gg_bud_hist_cond)

ggsave(here::here("analysis/pics/Budyko_Residuals_Histograms.png"))

