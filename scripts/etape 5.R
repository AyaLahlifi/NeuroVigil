########################################################################
# SCRIPT 05 : DCA + Fairness 
########################################################################

library(tidyverse)
library(pROC)

project_root <- "C:/Users/ayala/OneDrive/Attachments/Documents/TEST_FUNCML/data-ADNI-stage02"
path_funcml_ready <- file.path(project_root, "data/processed/Tabular data/funcml_ready")

fit_final <- readRDS(file.path(path_funcml_ready, "modele_final_xgboost_2F.rds"))
metriques_modele <- readRDS(file.path(path_funcml_ready, "metriques_modele_2F.rds"))
seuil_decision <- metriques_modele$seuil_optimal
cat("Decision threshold used (Youden, calculated on validation set):",
    round(seuil_decision, 3), "\n\n")

data_test <- read_csv(file.path(path_funcml_ready, "adni_test_imp_2.csv"),
                      show_col_types = FALSE)
data_test$target <- factor(data_test$target, levels = c("No", "Yes"))

probas_test <- predict(fit_final, data_test, type = "prob")[, "Yes"]
df_eval <- data_test %>% mutate(proba_pred = probas_test)

## DCA
N <- nrow(df_eval)
TP_all <- sum(df_eval$target == "Yes")
FP_all <- sum(df_eval$target == "No")
thresholds <- seq(0.01, 0.99, by = 0.01)

nb_model <- sapply(thresholds, function(pt) {
  pred <- ifelse(df_eval$proba_pred >= pt, "Yes", "No")
  tp <- sum(pred == "Yes" & df_eval$target == "Yes")
  fp <- sum(pred == "Yes" & df_eval$target == "No")
  (tp / N) - (fp / N) * (pt / (1 - pt))
})
nb_all <- sapply(thresholds, function(pt) (TP_all / N) - (FP_all / N) * (pt / (1 - pt)))

dca_plot_data <- data.frame(
  threshold = thresholds, net_benefit_model = nb_model,
  net_benefit_all = nb_all, net_benefit_none = 0
) %>%
  pivot_longer(cols = starts_with("net_benefit"), names_to = "Strategy", values_to = "NetBenefit") %>%
  mutate(Strategy = recode(Strategy,
                           "net_benefit_model" = "XGBoost Model",
                           "net_benefit_all" = "Treat all",
                           "net_benefit_none" = "Treat none"))

p_dca <- ggplot(dca_plot_data, aes(x = threshold, y = NetBenefit, color = Strategy)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = seuil_decision, linetype = "dashed", color = "grey40") +
  annotate("text", x = seuil_decision, y = max(dca_plot_data$NetBenefit, na.rm = TRUE),
           label = paste0("Youden threshold = ", round(seuil_decision, 3)), 
           hjust = -0.05, size = 3) +
  scale_color_manual(values = c("XGBoost Model" = "#2980b9", "Treat all" = "#e74c3c", "Treat none" = "#27ae60")) +
  theme_minimal() +
  labs(title = "Decision Curve Analysis - Clinical Utility (Binary Classification)",
       subtitle = paste0("Youden threshold = ", round(seuil_decision, 3), 
                         " | Clinical high-risk threshold = 0.369"),
       x = "Decision probability threshold", 
       y = "Standardized Net Benefit", 
       color = "Strategy") +
  theme(plot.title = element_text(face = "bold", size = 14), 
        plot.subtitle = element_text(size = 11, color = "grey40"),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(-0.05, max(dca_plot_data$NetBenefit, na.rm = TRUE) + 0.05))

print(p_dca)
ggsave(file.path(path_funcml_ready, "02_DCA_Clinique_EN_2F.png"), p_dca, width = 8, height = 5, dpi = 300)

## Fairness
calculer_metriques <- function(truth, prob, seuil) {
  pred <- ifelse(prob >= seuil, "Yes", "No")
  tp <- sum(pred == "Yes" & truth == "Yes"); fp <- sum(pred == "Yes" & truth == "No")
  tn <- sum(pred == "No" & truth == "No");   fn <- sum(pred == "No" & truth == "Yes")
  data.frame(
    AUC = as.numeric(auc(truth, prob, levels = c("No", "Yes"))),
    Sensitivity = ifelse((tp + fn) > 0, tp / (tp + fn), NA),
    Specificity = ifelse((tn + fp) > 0, tn / (tn + fp), NA),
    PPV = ifelse((tp + fp) > 0, tp / (tp + fp), NA)
  )
}

df_fairness <- df_eval %>%
  mutate(
    sexe = ifelse(PTGENDER == 1 | as.character(PTGENDER) == "Homme", "Male", "Female"),
    apoe_risque = ifelse(n_e4 >= 1, "APOE-e4 Carrier", "Non-carrier"),
    education = ifelse(PTEDUCAT >= median(PTEDUCAT, na.rm = TRUE), "High Education", "Low Education")
  )

res_sexe <- df_fairness %>% group_by(sexe) %>%
  summarise(calculer_metriques(target, proba_pred, seuil_decision), .groups = "drop") %>%
  mutate(Variable = "Sex", Groupe = sexe) %>% select(Variable, Groupe, everything())
res_apoe <- df_fairness %>% group_by(apoe_risque) %>%
  summarise(calculer_metriques(target, proba_pred, seuil_decision), .groups = "drop") %>%
  mutate(Variable = "APOE Status", Groupe = apoe_risque) %>% select(Variable, Groupe, everything())
res_edu <- df_fairness %>% group_by(education) %>%
  summarise(calculer_metriques(target, proba_pred, seuil_decision), .groups = "drop") %>%
  mutate(Variable = "Education", Groupe = education) %>% select(Variable, Groupe, everything())

table_fairness <- bind_rows(res_sexe, res_apoe, res_edu)
print(table_fairness)
write_csv(table_fairness, file.path(path_funcml_ready, "03_Metriques_Fairness_EN_2F.csv"))

p_fairness <- table_fairness %>%
  pivot_longer(cols = c(AUC, Sensitivity, Specificity, PPV), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = Metric, y = Value, fill = Groupe)) +
  geom_col(position = "dodge", alpha = 0.85) +
  facet_wrap(~Variable, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_minimal() +
  labs(title = paste0("Performance across subgroups (Youden threshold = ", round(seuil_decision, 3), ")"),
       subtitle = "Binary classification: Yes (≥0.124) vs No (<0.124)",
       y = "Score (0 to 1)", x = "Metric", fill = "Group") +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_fairness)
ggsave(file.path(path_funcml_ready, "03_Fairness_Comparison_EN_2F.png"), p_fairness, width = 12, height = 6, dpi = 300)

## Fairness Analysis - 3 Risk Levels

# Classer chaque patIENT SUR 3 N
df_eval_3niveaux <- df_eval %>%
  mutate(
    niveau_risque = case_when(
      proba_pred >= 0.369 ~ "Élevé",
      proba_pred >= seuil_decision ~ "Modéré",
      TRUE ~ "Faible"
    ),
    sexe = ifelse(PTGENDER == 1 | as.character(PTGENDER) == "Homme", "Male", "Female"),
    apoe_risque = ifelse(n_e4 >= 1, "APOE-e4 Carrier", "Non-carrier"),
    education = ifelse(PTEDUCAT >= median(PTEDUCAT, na.rm = TRUE), "High Education", "Low Education")
  )

# SEX
dist_sexe <- df_eval_3niveaux %>%
  group_by(sexe, niveau_risque) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexe) %>%
  mutate(pct = 100 * n / sum(n))

# APOE
dist_apoe <- df_eval_3niveaux %>%
  group_by(apoe_risque, niveau_risque) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(apoe_risque) %>%
  mutate(pct = 100 * n / sum(n))

# Education
dist_edu <- df_eval_3niveaux %>%
  group_by(education, niveau_risque) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(education) %>%
  mutate(pct = 100 * n / sum(n))

# Affichage
cat("\n Distribution des 3 N\n")
cat("\nPar Sexe :\n")
print(dist_sexe)
cat("\nPar APOE :\n")
print(dist_apoe)
cat("\nPar Education :\n")
print(dist_edu)

# Graphique de distribution
p_dist_3niveaux <- bind_rows(
  dist_sexe %>% mutate(Variable = "Sex", Groupe = sexe),
  dist_apoe %>% mutate(Variable = "APOE Status", Groupe = apoe_risque),
  dist_edu %>% mutate(Variable = "Education", Groupe = education)
) %>%
  ggplot(aes(x = Variable, y = pct, fill = niveau_risque)) +
  geom_col(position = "dodge", alpha = 0.85) +
  facet_wrap(~Groupe, scales = "free_x") +
  
  scale_fill_manual(
    values = c("Faible" = "#00B894", "Modéré" = "#F39C12", "Élevé" = "#D63031"),
    labels = c("Faible" = "Low", "Modéré" = "Moderate", "Élevé" = "High") 
  ) +

scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  theme_minimal() +
  labs(
    title = "Distribution of 3 Risk Levels across Subgroups",
    subtitle = paste0("Thresholds: Low < ", round(seuil_decision, 3), 
                      " ≤ Moderate < 0.369 ≤ High"),
    y = "Percentage (%)", 
    x = "Subgroup", 
    fill = "Risk Level"
  ) +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_dist_3niveaux)
ggsave(file.path(path_funcml_ready, "04_Distribution_3Levels_Fairness.png"), 
       p_dist_3niveaux, width = 12, height = 6, dpi = 300)

cal_final     <- readRDS(file.path(path_funcml_ready, "cal_final_2F.rds"))
shap_patient  <- readRDS(file.path(path_funcml_ready, "shap_patient_2F.rds"))
resultats_table <- readRDS(file.path(path_funcml_ready, "resultats_table_2F.rds"))

# calibration
plot(cal_final, style = "curve")
ggsave(file.path(path_funcml_ready, "calibration_plot_2F.png"), width = 8, height = 6)

# SHAP
plot(shap_patient, kind = "waterfall")
ggsave(file.path(path_funcml_ready, "shap_waterfall_2F.png"), width = 8, height = 6)

#robustesse
print(resultats_table)
