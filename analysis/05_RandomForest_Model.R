

install.packages("caret")
install.packages("ranger")
library(caret)
library(ranger)



ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

#with all predictors::-----
set.seed(123)
stat_model_mtry <- train(
  res_cond ~ pet_p_cond * (cti + whc + igbp_land_use + canopy_height),
  data = df_budyko,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(2,3,4,5,6),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 5*4*2 =40 Kombinationen, welche 5 fach validiertwerden -> 200 Modelltrainings
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_model_mtry)
plot(stat_model_mtry, main = "Random Forest Model (All Predictors for Residuals)")


summary(stat_model_mtry)


varImp(stat_model_mtry)









#---------------------------------------------------------

#with IGBP and WHC::-----
set.seed(123)
stat_model_igbp_whc <- train(
  res_cond ~ pet_p_cond * (whc + igbp_land_use),
  data = df_budyko,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance")
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_model_igbp_whc)
plot(stat_model_igbp_whc, main = "Random Forest Model (IGBP + WHC)")
varImp(stat_model_igbp_whc)

#with IGBP and CTI::-----
set.seed(123)
stat_model_igbp_cti <- train(
  res_cond ~ pet_p_cond * (igbp_land_use + cti),
  data = df_budyko,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 5*4*2 =40 Kombinationen, welche 5 fach validiertwerden -> 200 Modelltrainings
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_model_igbp_cti)
plot(stat_model_igbp_cti, main = "Random Forest Model (LCT + CTI)")

#with WHC and CTI::-----
set.seed(123)
stat_model_whc_cti <- train(
  res_cond ~ pet_p_cond *(whc + cti),
  data = df_budyko,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 5*4*2 =40 Kombinationen, welche 5 fach validiertwerden -> 200 Modelltrainings
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_model_whc_cti)
plot(stat_model_whc_cti, main = "Random Forest Model (WHC + CTI)")


#with IGBP and Canopy Height::-----
set.seed(123)
stat_model_igbp_ch <- train(
  res_cond ~ pet_p_cond *(igbp_land_use + canopy_height),
  data = df_budyko,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 5*4*2 =40 Kombinationen, welche 5 fach validiertwerden -> 200 Modelltrainings
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_model_igbp_ch)
plot(stat_model_igbp_ch, main = "Random Forest Model (LCT + CH)")
varImp(stat_model_igbp_ch)

#with WHC and Canopy Height::-----
set.seed(123)
stat_model_whc_ch <- train(
  res_cond ~ pet_p_cond *(whc + canopy_height),
  data = df_budyko,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 5*4*2 =40 Kombinationen, welche 5 fach validiertwerden -> 200 Modelltrainings
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_model_whc_ch)
plot(stat_model_whc_ch, main = "Random Forest Model (WHC + CH)")
varImp(stat_model_whc_ch)

#with CTI and Canopy Height::-----
set.seed(123)
stat_model_cti_ch <- train(
  res_cond ~ pet_p_cond *(cti + canopy_height),
  data = df_budyko,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = expand.grid(
    .mtry = c(1,2,3),
    .min.node.size = c(3,5,7,9),
    .splitrule = c("extratrees", "variance") # gesamt 5*4*2 =40 Kombinationen, welche 5 fach validiertwerden -> 200 Modelltrainings
  ),
  importance = 'impurity',
  metric = "RMSE",         # optimize for AUC
  # tuneLength = 5          # try 5 different mtry values

)

print(stat_model_cti_ch)
plot(stat_model_cti_ch, main = "Random Forest Model (CTI + CH)")
varImp(stat_model_cti_ch)


#Extract RMSE values:

stat_model_results <- data.frame(
  Model = c("LCT + WHC", "LCT + CTI", "WHC + CTI", "CH + LCT", "CH + WHC", "CH + CTI"),
  RMSE = c(
    min(stat_model_igbp_whc$results$RMSE),
    min(stat_model_igbp_cti$results$RMSE),
    min(stat_model_whc_cti$results$RMSE),
    min(stat_model_igbp_ch$results$RMSE),
    min(stat_model_whc_ch$results$RMSE),
    min(stat_model_cti_ch$results$RMSE)

  ),
  R2 = c(
    max(stat_model_igbp_whc$results$Rsquared),
    max(stat_model_igbp_cti$results$Rsquared),
    max(stat_model_whc_cti$results$Rsquared),
    max(stat_model_igbp_ch$results$Rsquared),
    max(stat_model_whc_ch$results$Rsquared),
    max(stat_model_cti_ch$results$Rsquared)
  )
)

#
# print(stat_model_results)
#
# stat_model_results_sorted <- stat_model_results|>
#   arrange(RMSE, desc(R2))
#
#
# print(stat_model_results_sorted)
#
# stat_model_results_long <- pivot_longer(
#   stat_model_results_sorted,
#   cols = c("RMSE", "R2"),
#   names_to = "Metric",
#   values_to = "Value"
# )
# stat_model_results_long <- stat_model_results_long |>
#   group_by(Metric) |>
#   mutate(Value_scaled = (Value - min(Value)) / (max(Value) - min(Value))) |>
#   mutate(Value_scaled = ifelse(Value_scaled == 0, 0.01, Value_scaled)) |>
#   ungroup()
#
# # 3. Plot mit Balken nebeneinander (dodge)
# ggplot(stat_model_results_long, aes(x = Model, y = Value_scaled, fill = Metric)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_text(
#     aes(label = round(Value, 3)),
#     position = position_dodge(width = 0.7),
#     vjust = -0.5,
#     size = 3.5,
#     check_overlap = TRUE
#   ) +
#   labs(
#     title = "Model Performance: RMSE vs. R²",
#     x = "Model",
#     y = "Value",
#     fill = "Metric"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
#


#mit 2 achsen:

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Daten sortieren wie gewünscht:
# stat_model_results_sorted <- stat_model_results |>
#   arrange(RMSE, desc(R2))
#
# print(stat_model_results_sorted)
#
# # 2. Long-Format erzeugen:
# stat_model_results_long <- pivot_longer(
#   stat_model_results_sorted,
#   cols = c("RMSE", "R2"),
#   names_to = "Metric",
#   values_to = "Value"
# )
#
# # 3. Separate Sortierung nach Metrik:
# rmse_models <- stat_model_results_sorted |>
#   arrange(RMSE) |>
#   pull(Model)
#
# r2_models <- stat_model_results_sorted |>
#   arrange(desc(R2)) |>
#   pull(Model)


# 1. RMSE sortiert
# Beispiel-R²-Werte (ersetzen durch deine echten Werte!)
r2_values <- c(0.3623863, 0.3189572, 0.3486199, 0.3535943, 0.3590660, 0.2609355)

# R² zur Tabelle hinzufügen
stat_model_results <- stat_model_results |>
  mutate(R2 = r2_values)

rmse_df <- stat_model_results |>
  arrange(RMSE) |>
  mutate(
    Metric = "RMSE",
    Value = RMSE,
    Model = factor(Model, levels = Model)  # korrekt geordnete Levels
  ) |>
  select(Model, Metric, Value)

# 2. R² sortiert
r2_df <- stat_model_results |>
  arrange(desc(R2)) |>
  mutate(
    Metric = "R2",
    Value = R2,
    Model = factor(Model, levels = Model)  # korrekt geordnete Levels
  ) |>
  select(Model, Metric, Value)

# 3. Zusammenführen
stat_model_results_long <- bind_rows(rmse_df, r2_df)

# 4. Einheitliche Model-Level nach RMSE
model_levels <- as.character(rmse_df$Model)  # Reihenfolge nach RMSE
stat_model_results_long <- stat_model_results_long |>
  mutate(Model = factor(Model, levels = model_levels))

stat_model_results_long <- stat_model_results_long |>
  mutate(
    Model_facet = case_when(
      Metric == "RMSE" ~ factor(Model, levels = as.character(rmse_df$Model)),
      Metric == "R2"   ~ factor(Model, levels = as.character(r2_df$Model))
    )
  )

ggplot(stat_model_results_long, aes(x = Model_facet, y = Value, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = round(Value, 3)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~Metric, scales = "free_y") +  # ← zwei getrennte Y-Achsen
  labs(
    title = "Model Performance: RMSE und R²",
    x = "Model",
    y = "Value",
    fill = "Metrik"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#Extract RSquared Values:

# stat_model_results_R2 <- data.frame(
#   Model = c("LCT + WHC", "LCT + CTI", "WHC + CTI", "CH + LCT", "CH + WHC", "CH + CTI"),
#   RSquared = c(
#     min(stat_model_igbp_whc$results$Rsquared),
#     min(stat_model_igbp_cti$results$Rsquared),
#     min(stat_model_whc_cti$results$Rsquared),
#     min(stat_model_igbp_ch$results$Rsquared),
#     min(stat_model_whc_ch$results$Rsquared),
#     min(stat_model_cti_ch$results$Rsquared)
#
#   )
# )

# stat_model_results_R2_sorted <- stat_model_results_R2 |>
#   arrange(desc(RSquared))
#
# print(stat_model_results_R2_sorted)


#-----------PLOTS----------------

#all predictors:
library(ggplot2)
ggplot(stat_model_mtry$results, aes(x = mtry, y = RMSE, color = splitrule)) +
  geom_point(aes(shape = factor(min.node.size)), size = 3) +
  geom_line(aes(group = interaction(splitrule, min.node.size)), linetype = "dashed") +
  facet_wrap(~splitrule) +
  theme_minimal() +
  labs(
    title = "Cross-validated RMSE across tuning parameters",
    x = "mtry (number of variables tried at each split)",
    y = "RMSE",
    shape = "min.node.size"
  )



#Comparison Variable importance plots
library(ggplot2)
library(caret)

imp1 <- varImp(stat_model_igbp_whc)$importance
imp2 <- varImp(stat_model_igbp_cti)$importance
imp3 <- varImp(stat_model_whc_cti)$importance
imp4 <- varImp(stat_model_cti_ch)$importance
imp5 <- varImp(stat_model_igbp_ch)$importance
imp6 <- varImp(stat_model_whc_ch)$importance


imp1$Model <- "LCT + WHC"
imp2$Model <- "LCT + CTI"
imp3$Model <- "WHC + CTI"
imp4$Model <- "CH + CTI"
imp5$Model <- "CH + LCT"
imp6$Model <- "CH + WHC"

imp1$Variable <- rownames(imp1)
imp2$Variable <- rownames(imp2)
imp3$Variable <- rownames(imp3)
imp4$Variable <- rownames(imp4)
imp5$Variable <- rownames(imp5)
imp6$Variable <- rownames(imp6)

imp_all <- rbind(imp1, imp2, imp3, imp4, imp5, imp6)

ggplot(imp_all, aes(x = Variable, y = Overall, fill = Model)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance accross 6 Models", y = "Importance")



#Comparison Balkendiagram RMSE
# RMSE-Werte manuell eingeben

# stat_model_results_rmse <- data.frame(
#   Model = c("LCT + WHC", "LCT + CTI", "WHC + CTI","CH + CTI","CH + LCT", "CH + WHC")
#   #RMSE = c(0.4138488, 0.4598321, 0.3886788, 0.4253939, 0.4400672, 0.3791900 )
# )

# Balkendiagramm
ggplot(stat_model_results_rmse, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_col(fill = "grey", width = 0.6) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5, size = 4) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Model Performance Comparison (RMSE)",
    x = "Model",
    y = "Root Mean Square Error (RMSE)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

stat_model_results_rmse_sorted <- stat_model_results_rmse|>
  arrange(RMSE)

print(stat_model_results_rmse_sorted)

#------merge RMSE and R2 für Balkensiadram---------------------------------

# stat_model_results <- stat_model_results_rmse |>
#   left_join(stat_model_results_R2, by = "Model")




#-----------------------------R^2----------------------------------






