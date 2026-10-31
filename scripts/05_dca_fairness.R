########################################################################
# SCRIPT 05 : decision curve analysis and subgroup fairness audit (TEST set)
########################################################################
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(funcml); library(pROC) })   # pROC after funcml: use pROC::auc explicitly

for (f in c(FILES$model, FILES$metrics, FILES$test)) require_file(proc_path(f), "Run scripts 01 and 02 first.")
fit_final        <- readRDS(proc_path(FILES$model))
metriques_modele <- readRDS(proc_path(FILES$metrics))
seuil_bas  <- metriques_modele$seuil_bas
seuil_haut <- metriques_modele$seuil_haut           # read from the metrics file, not hard-coded
cat("Thresholds (validation set): low", round(seuil_bas, 3), "| high", round(seuil_haut, 3), "\n")

data_test <- read_csv(proc_path(FILES$test), show_col_types = FALSE)
data_test$target <- factor(data_test$target, levels = c("No", "Yes"))
df_eval <- data_test %>% mutate(proba_pred = predict(fit_final, data_test, type = "prob")[, "Yes"])

## Decision curve analysis ------------------------------------------------------------
N <- nrow(df_eval); TP_all <- sum(df_eval$target == "Yes"); FP_all <- sum(df_eval$target == "No")
thresholds <- seq(0.01, 0.99, by = 0.01)
nb_model <- sapply(thresholds, function(pt) {
  pred <- df_eval$proba_pred >= pt
  (sum(pred & df_eval$target == "Yes") / N) - (sum(pred & df_eval$target == "No") / N) * (pt / (1 - pt))
})
nb_all <- (TP_all / N) - (FP_all / N) * (thresholds / (1 - thresholds))

dca_data <- tibble(threshold = thresholds, `XGBoost Model` = nb_model, `Treat all` = nb_all, `Treat none` = 0) %>%
  pivot_longer(-threshold, names_to = "Strategy", values_to = "NetBenefit")

p_dca <- ggplot(dca_data, aes(threshold, NetBenefit, color = Strategy)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = c(seuil_bas, seuil_haut), linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("XGBoost Model" = "#2980b9", "Treat all" = "#e74c3c", "Treat none" = "#27ae60")) +
  coord_cartesian(ylim = c(-0.05, max(dca_data$NetBenefit, na.rm = TRUE) + 0.05)) +
  theme_minimal() + theme(legend.position = "bottom") +
  labs(title = "Decision Curve Analysis - Clinical Utility",
       subtitle = sprintf("Dashed lines: low threshold %.3f and high threshold %.3f", seuil_bas, seuil_haut),
       x = "Decision probability threshold", y = "Standardized Net Benefit")
print(p_dca); ggsave(out_path("dca.png"), p_dca, width = 8, height = 5, dpi = 300)

## Fairness audit at the screening threshold -------------------------------------------
calculer_metriques <- function(truth, prob, seuil) {
  pred <- prob >= seuil
  tp <- sum(pred & truth == "Yes"); fp <- sum(pred & truth == "No")
  tn <- sum(!pred & truth == "No"); fn <- sum(!pred & truth == "Yes")
  data.frame(AUC = as.numeric(pROC::auc(truth, prob, levels = c("No", "Yes"), direction = "<")),
             Sensitivity = ifelse(tp + fn > 0, tp / (tp + fn), NA),
             Specificity = ifelse(tn + fp > 0, tn / (tn + fp), NA),
             PPV         = ifelse(tp + fp > 0, tp / (tp + fp), NA))
}
cat("Education median (test set):", median(df_eval$PTEDUCAT, na.rm = TRUE), "years\n")
df_fairness <- df_eval %>% mutate(
  sexe        = ifelse(PTGENDER == "Homme", "Male", "Female"),
  apoe_risque = ifelse(n_e4 >= 1, "APOE-e4 Carrier", "Non-carrier"),
  education   = ifelse(PTEDUCAT >= median(PTEDUCAT, na.rm = TRUE), "High Education", "Low Education"))

audit <- function(var, label) df_fairness %>% group_by(Groupe = .data[[var]]) %>%
  summarise(n = n(), converters = sum(target == "Yes"),
            calculer_metriques(target, proba_pred, seuil_bas), .groups = "drop") %>%
  mutate(Variable = label, .before = 1)
table_fairness <- bind_rows(audit("sexe", "Sex"), audit("apoe_risque", "APOE Status"), audit("education", "Education"))
print(table_fairness)
write_csv(table_fairness, out_path("fairness_metrics.csv"))
table_fairness %>% group_by(Variable) %>% summarise(Delta_AUC = max(AUC) - min(AUC)) %>% print()

p_fair <- table_fairness %>%
  pivot_longer(c(AUC, Sensitivity, Specificity, PPV), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(Metric, Value, fill = Groupe)) + geom_col(position = "dodge", alpha = 0.85) +
  facet_wrap(~Variable, scales = "free_x") + scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = sprintf("Performance across subgroups (Youden threshold = %.3f)", seuil_bas), y = "Score", fill = "Group")
print(p_fair); ggsave(out_path("fairness_comparison.png"), p_fair, width = 12, height = 6, dpi = 300)

## Distribution of the three risk levels by subgroup ------------------------------------
df_3 <- df_fairness %>% mutate(niveau = case_when(proba_pred >= seuil_haut ~ "High",
                                                  proba_pred >= seuil_bas  ~ "Moderate", TRUE ~ "Low"))
dist <- bind_rows(lapply(c(Sex = "sexe", APOE = "apoe_risque", Education = "education"), function(v)
  df_3 %>% count(Groupe = .data[[v]], niveau) %>% group_by(Groupe) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()),
  .id = "Variable")
print(dist, n = Inf)
p_dist <- ggplot(dist, aes(Variable, pct, fill = factor(niveau, c("Low", "Moderate", "High")))) +
  geom_col(position = "dodge", alpha = 0.85) + facet_wrap(~Groupe) +
  scale_fill_manual(values = c(Low = "#00B894", Moderate = "#F39C12", High = "#D63031")) +
  theme_minimal() + theme(legend.position = "bottom") + labs(y = "Percentage (%)", fill = "Risk level")
print(p_dist); ggsave(out_path("risk_level_distribution.png"), p_dist, width = 12, height = 6, dpi = 300)

## Saved figures and temporal table from earlier scripts --------------------------------
if (file.exists(proc_path(FILES$cal)))  { plot(readRDS(proc_path(FILES$cal)), style = "curve") }
if (file.exists(proc_path(FILES$shap))) { plot(readRDS(proc_path(FILES$shap)), kind = "waterfall") }
if (file.exists(proc_path(FILES$temporal))) print(readRDS(proc_path(FILES$temporal)), width = Inf)
message("Script 05 done.")
