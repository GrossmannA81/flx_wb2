

##LM:----------------
#linear model
stat_lm <- lm(res_cond ~ pet_p_cond + cti + whc + canopy_height + igbp_land_use, data = df_budyko)
summary(stat_lm)

par(mfrow = c(2, 2))
plot(stat_lm)
# stat_lm_nocond <- lm(res ~ pet_p + cti  + whc +canopy_height + igbp_land_use, data = df_budyko)
# summary(stat_lm_nocond)


library(broom)
library(ggplot2)
library(dplyr)
library(forcats)

# Extract tidy model results
df_coef_lm<- tidy(stat_lm, conf.int = TRUE)

# Reorder for plotting and filter out intercept
coef_lm_plot <- df_coef_lm |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, estimate))

# Create the plot
ggplot(coef_lm_plot, aes(x = estimate, y = term)) +
  geom_point(color = "firebrick4", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = " Linear Model: Estimates of Coefficients",
    x = "Estimated Effect on Residuals",
    y = NULL
    ) +
  theme_minimal(base_size = 14)


##BS:-----------------
library(splines)
#non-linear model bsplines
stat_bs <- lm(
  res_cond ~ bs(pet_p_cond, df = 3) * bs(cti, df = 3)* bs(whc, df = 3) * bs(canopy_height, df=3) + factor(igbp_land_use), data = df_budyko)
summary(stat_bs)

# stat_bs_nocond <- lm(res ~ bs(pet_p, df = 3) * bs(cti, df = 2) * bs(whc, df = 2) + factor(igbp_land_use), data = df_budyko)
# summary(stat_bs_nocond)



# library(mgcv)
# stat_gam <- gam(res_cond ~ te(pet_p_cond, cti, k = c(3, 2)) + igbp_land_use, data = df_budyko)
# summary (stat_gam)


##GAM:-----------------
library(mgcv)
# df_budyko$igbp_land_use <- factor(df_budyko$igbp_land_use)
# df_budyko$whc <- as.numeric(as.character(df_budyko$whc))
stat_gam <- gam(res_cond ~ s(pet_p_cond) + s(cti) + s(whc) + s(canopy_height) + igbp_land_use , data = df_budyko)
summary(stat_gam)

#Plot GAM Smooth Effects
par(mfrow = c(2, 2))
plot(stat_gam, se = TRUE, shade = TRUE, pages = 1)
title("GAM - Smooth Effects of  Predictors on Residual", outer = TRUE, line = -1)



library(broom)
library(ggplot2)
library(dplyr)
library(forcats)
# Extract tidy model results
df_coef_gam<- tidy(stat_gam, parametric = TRUE, conf.int = TRUE)

# Reorder for plotting and filter out intercept
coef_gam_plot <- df_coef_gam |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, estimate))



# Create the plot with  reference levels:
ggplot(coef_gam_plot, aes(x = estimate, y = term)) +
  geom_point(color = "firebrick4", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = " WSA (reference level) Estimates of Coefficients", # df_budyko$igbp_land_use <- relevel(df_budyko$igbp_land_use, ref = "MF")
    x = "Estimated Effect on Residuals",
    y = NULL
  ) +
  theme_minimal(base_size = 14)


install.packages("gratia")
library(gratia)

#show smoothness of predictors
draw(stat_gam)
#or
plot(stat_gam)



#GAM---# find extreme effects with reference levelled LCT----------------------------------------------------

library(mgcv)

# Store results
ref_effects <- data.frame()

# Loop through all land cover types
for (ref in levels(df_budyko$igbp_land_use)) {
  df_budyko$igbp_land_use <- relevel(df_budyko$igbp_land_use, ref = ref)

  # Fit GAM
  model <- gam(res_cond ~ s(pet_p_cond) + s(cti) + s(whc) + s(canopy_height) + igbp_land_use, data = df_budyko)
  coefs <- summary(model)$p.table

  # Extract land use coefficients (skip intercept and smooth terms)
  coef_lct <- coefs[grepl("igbp_land_use", rownames(coefs)), , drop = FALSE]

  if (nrow(coef_lct) > 0) {
    min_effect <- min(coef_lct[, "Estimate"])
    max_effect <- max(coef_lct[, "Estimate"])

    ref_effects <- rbind(ref_effects, data.frame(
      Reference = ref,
      MinEffect = min_effect,
      MaxEffect = max_effect,
      Range = max_effect - min_effect
    ))
  }
}


# Most extreme ranges
ref_effects[which.max(ref_effects$Range), ]   # Largest difference
ref_effects[which.min(ref_effects$MinEffect), ] # Most negative estimate
ref_effects[which.max(ref_effects$MaxEffect), ] # Most positive estimate


library(ggplot2)

ggplot(ref_effects, aes(x = Reference)) +
  geom_point(aes(y = MaxEffect, color = "Max Coefficient"), size = 3) +
  geom_point(aes(y = MinEffect, color = "Min Coefficient"), size = 3) +
 # geom_line(aes(y = Range, group = 1), linetype = "dotted", color = "black") +
  labs(title = "Effect Range by Reference Level of LCT",
       y = "Estimated Coefficient",
       color = "Legend") +
  scale_color_manual(values = c("Max Coefficient" = "forestgreen",
                                "Min Coefficient" = "red3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





















# ## GAM:
#
# grid_data_all$predicted_res <- predict(stat_gam, newdata = grid_data_all)
# grid_data_all$igbp_land_use <- factor(grid_data_all$igbp_land_use, levels = land_types)
#
#
#
# ggplot(grid_data_all, aes(x = cti, y = pet_p_cond, fill = predicted_res)) +
#   geom_raster() +
#   facet_wrap(~ igbp_land_use) +
#   scale_fill_viridis_c(option = "B", name = expression(hat(epsilon)*"′")) +
#   labs(
#     title = "Predicted Budyko Deviation (ε′) by Land Use Type",
#     x = "CTI (Compound Topographic Index)",
#     y = "Aridity (PET/P)"
#   ) +
#   theme_minimal()
#
#
#
#
# ## BS
# # Choose representative land use types
# land_types <- levels(df_budyko$igbp_land_use)
#
# # Create grid
# grid_data_all <- expand.grid(
#   pet_p_cond = seq(min(df_budyko$pet_p_cond, na.rm = TRUE), max(df_budyko$pet_p_cond, na.rm = TRUE), length.out = 100),
#   cti = seq(min(df_budyko$cti, na.rm = TRUE), max(df_budyko$cti, na.rm = TRUE), length.out = 100),
#   igbp_land_use = land_types
# )
#
# # Predict for all
# grid_data_all$predicted_res <- predict(stat_bs, newdata = grid_data_all)
# grid_data_all$igbp_land_use <- factor(grid_data_all$igbp_land_use, levels = land_types)
# # Plot
# ggplot(grid_data_all, aes(x = cti, y = pet_p_cond, fill = predicted_res)) +
#   geom_raster() +
#   facet_wrap(~ igbp_land_use) +
#   scale_fill_viridis_c(option = "B", name = expression(hat(epsilon)*"′")) +
#   labs(
#     title = "Predicted Budyko Deviation (ε′) by Land Use Type",
#     x = "CTI",
#     y = "Aridity (PET/P)"
#   ) +
#   theme_minimal()
