########################################################################
# SCRIPT 02 : TRAIN XGBoost
########################################################################

library(funcml)
library(tidyverse)

project_root <- "C:/Users/ayala/OneDrive/Attachments/Documents/TEST_FUNCML/data-ADNI-stage02"
path_funcml_ready <- file.path(project_root, "data/processed/Tabular data/funcml_ready")


data_train_full <- read_csv(
  file.path(path_funcml_ready, "adni_train_imputed.csv"),
  show_col_types = FALSE
)

data_test <- read_csv(
  file.path(path_funcml_ready, "adni_test_imp_2.csv"),
  show_col_types = FALSE
)

data_train_full$target <- factor(data_train_full$target, levels = c("No", "Yes"))
data_test$target       <- factor(data_test$target, levels = c("No", "Yes"))

cat("Répartition TRAIN :\n"); print(table(data_train_full$target))
cat("\nRépartition TEST  :\n"); print(table(data_test$target))

vars_predicteurs <- data_train_full %>%
  select(-any_of(c("RID", "event", "time_to_event", "target", "GENOTYPE", "a_une_irm"))) %>%
  names()

formula_cls <- as.formula(paste("target ~", paste(vars_predicteurs, collapse = " + ")))


set.seed(42)
idx_val   <- sample(nrow(data_train_full), size = round(0.2 * nrow(data_train_full)))
data_val  <- data_train_full[idx_val, ]
data_fit  <- data_train_full[-idx_val, ]

cat("\nFit (entraînement) :", nrow(data_fit),
    "| Validation (seuils) :", nrow(data_val),
    "| Test (rapport final) :", nrow(data_test), "\n")

resampling_cv <- cv(v = 5, seed = 42)


## Step 1 : Baseline Benchmark 
cmp_cls <- compare_learners(
  data       = data_fit,
  formula    = formula_cls,
  models     = c("glm", "rpart", "ranger", "gbm", "xgboost"),
  resampling = resampling_cv,
  metrics    = c("accuracy", "auc", "logloss"),
  type       = "prob",
  seed       = 42
)
cmp_cls
plot(cmp_cls)

## Step 2 : Tune the Best Learner 
meilleur_modele <- "xgboost"

grid_xgb <- expand.grid(
  nrounds   = c(100, 200),
  max_depth = c(3, 4, 6),
  eta       = c(0.05, 0.1)
)

tuned <- tune(
  data       = data_fit,
  formula    = formula_cls,
  model      = meilleur_modele,
  grid       = grid_xgb,
  resampling = resampling_cv,
  metric     = "auc",
  search     = "random",
  n_evals    = 8,
  type       = "prob",
  seed       = 42
)
print(tuned$best)

## Step 3 : Fit the Final Model 
fit_final <- fit(
  formula = formula_cls,
  data    = data_fit,
  model   = meilleur_modele,
  spec    = as.list(tuned$best[1, ])
)
fit_final

## Step 4 : Global Interpretation 
perm_cls <- interpret(fit_final, data_test, method = "permute",
                      metric = "auc", type = "prob", nsim = 50, seed = 42)

perm_df_cls <- tryCatch(
  as.data.frame(perm_cls$result$scores),
  error = function(e) NULL
)

if (is.null(perm_df_cls) || !any(c("feature", "Variable", "variable", "term") %in% names(perm_df_cls))) {
  perm_df_cls <- perm_cls$result$scores
}

col_var_cls <- intersect(c("feature", "Variable", "variable", "term"), names(perm_df_cls))[1]
col_imp_cls <- intersect(c("importance", "Importance", "mean_importance", "score"), names(perm_df_cls))[1]

top_predictor <- perm_df_cls %>%
  arrange(desc(.data[[col_imp_cls]])) %>%
  slice_head(n = 1) %>%
  pull(.data[[col_var_cls]])
plot(perm_cls)

cat("\nTop predictor (permutation importance) :", top_predictor, "\n")

pdp_var <- interpret(fit_final, data_test, method = "pdp",
                     features = top_predictor, type = "prob", pos_level = "Yes")
plot(pdp_var)

ale_var <- interpret(fit_final, data_test, method = "ale",
                     features = top_predictor, type = "prob", pos_level = "Yes")
plot(ale_var)

## Step 5 : Local Explanation for a High-Risk Patient 
patient_haut_risque <- data_test[which.max(
  predict(fit_final, data_test, type = "prob")[, "Yes"]
), , drop = FALSE]

shap_patient <- interpret(fit_final, data_test, method = "shap",
                          newdata = patient_haut_risque, type = "prob",
                          nsim = 50, seed = 42)
plot(shap_patient, kind = "waterfall")

## Step 6 : Calibration Check 
cal_final <- interpret(fit_final, data_test, method = "calibration",
                       type = "prob", pos_level = "Yes", bins = 10)
plot(cal_final, style = "curve")

## Step 7 : CALCUL DES DEUX SEUILS (Rule-In )

probs_val  <- predict(fit_final, data_val, type = "prob")[, "Yes"]
labels_val <- data_val$target

# ============================================================================
# 1. Calculer Sensibilité, Spécificité et Youden pour chaque seuil candidat
# ============================================================================
seuils_test <- seq(0.01, 0.99, by = 0.001)

metrics_df <- data.frame(
  seuil = seuils_test,
  sens = sapply(seuils_test, function(s) {
    pred <- ifelse(probs_val >= s, "Yes", "No")
    tp <- sum(pred == "Yes" & labels_val == "Yes")
    fn <- sum(pred == "No"  & labels_val == "Yes")
    ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  }),
  spec = sapply(seuils_test, function(s) {
    pred <- ifelse(probs_val >= s, "Yes", "No")
    tn <- sum(pred == "No"  & labels_val == "No")
    fp <- sum(pred == "Yes" & labels_val == "No")
    ifelse((tn + fp) > 0, tn / (tn + fp), 0)
  })
)

metrics_df$youden <- metrics_df$sens + metrics_df$spec - 1

# ============================================================================
# 2. Seuil BAS : Youden
# ============================================================================
seuil_bas <- metrics_df$seuil[which.max(metrics_df$youden)]

# ============================================================================
# 3. Seuil HAUT : Rule-In 
# ============================================================================
spec_cible <- 0.90
candidats <- metrics_df %>% filter(spec >= spec_cible)

if (nrow(candidats) > 0) {
  seuil_haut <- candidats$seuil[which.max(candidats$youden)]
  methode_haut <- paste0("Rule-In (Spécificité ≥ ", spec_cible*100, "%)")
} else {
  # Fallback à 85%
  spec_cible <- 0.85
  candidats <- metrics_df %>% filter(spec >= spec_cible)
  if (nrow(candidats) > 0) {
    seuil_haut <- candidats$seuil[which.max(candidats$youden)]
    methode_haut <- paste0("Rule-In (Spécificité ≥ ", spec_cible*100, "%, fallback)")
  } else {
    # Dernier fallback : P75 des converters
    probs_converters <- probs_val[labels_val == "Yes"]
    seuil_haut <- as.numeric(quantile(probs_converters, probs = 0.75, na.rm = TRUE))
    methode_haut <- "P75 des converters (fallback ultime)"
  }
}

# Securite : garantir seuil_haut > seuil_bas
if (seuil_haut <= seuil_bas) {
  seuil_haut <- seuil_bas + 0.15
  if (seuil_haut > 0.95) seuil_haut <- 0.95
}

# 4. Calcul des métriques à chaque seuil 
sens_bas <- metrics_df$sens[metrics_df$seuil == seuil_bas]
spec_bas <- metrics_df$spec[metrics_df$seuil == seuil_bas]
sens_haut <- metrics_df$sens[metrics_df$seuil == seuil_haut]
spec_haut <- metrics_df$spec[metrics_df$seuil == seuil_haut]

# 5. Affichage 

n_faible <- sum(probs_val < seuil_bas)
n_modere <- sum(probs_val >= seuil_bas & probs_val < seuil_haut)
n_eleve  <- sum(probs_val >= seuil_haut)
n_total  <- length(probs_val)

cat("SUILS CALCULES \n")
cat(sprintf("Seuil bas  (Youden)                : %.3f\n", seuil_bas))
cat(sprintf("  → Sensibilité: %.3f | Spécificité: %.3f\n", sens_bas, spec_bas))
cat(sprintf("Seuil haut (%s) : %.3f\n", methode_haut, seuil_haut))
cat(sprintf("  → Sensibilité: %.3f | Spécificité: %.3f\n", sens_haut, spec_haut))
cat(sprintf("Risque FAIBLE  (< %.3f)   : %3d patients (%.1f%%)\n", seuil_bas, n_faible, 100*n_faible/n_total))
cat(sprintf("Risque MODERE  (%.3f - %.3f) : %3d patients (%.1f%%)\n", seuil_bas, seuil_haut, n_modere, 100*n_modere/n_total))
cat(sprintf("Risque ELEVE   (>= %.3f)  : %3d patients (%.1f%%)\n", seuil_haut, n_eleve, 100*n_eleve/n_total))
cat("--------------------------------------------------\n")
cat(sprintf("Nombre de converters en validation : %d / %d (%.1f%%)\n", 
            sum(labels_val == "Yes"), n_total, 100*sum(labels_val == "Yes")/n_total))

## Step 8 : Metriques finales SUR LE TEST

probs_test  <- predict(fit_final, data_test, type = "prob")[, "Yes"]
labels_test <- data_test$target

pos <- probs_test[labels_test == "Yes"]
neg <- probs_test[labels_test == "No"]
r <- rank(c(pos, neg))
auc_reel <- (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
  (length(pos) * length(neg))

bins <- 10
ece_tbl <- tibble(prob = probs_test, label = as.integer(labels_test == "Yes")) %>%
  mutate(tranche = ntile(prob, bins)) %>%
  group_by(tranche) %>%
  summarise(gap = abs(mean(prob) - mean(label)) * n(), n = n())
ece_reel <- sum(ece_tbl$gap) / sum(ece_tbl$n)

## Step 9 : Sauvegarder model et metriquea

saveRDS(fit_final, file.path(path_funcml_ready, "modele_final_xgboost_2F.rds"))

metriques_modele <- list(
  auc_test         = round(auc_reel, 3),
  ece_test         = round(ece_reel, 3),
  seuil_bas        = seuil_bas,        
  seuil_haut       = seuil_haut,      
  seuil_optimal    = seuil_bas,        
  n_fit            = nrow(data_fit),
  n_val            = nrow(data_val),
  n_test           = nrow(data_test),
  date_calcul      = Sys.Date(),
  variables_modele = vars_predicteurs
)

saveRDS(metriques_modele, file.path(path_funcml_ready, "metriques_modele_2F.rds"))

cat("\n Metriques sauvegardees :\n")
cat("  AUC test           :", metriques_modele$auc_test, "\n")
cat("  ECE                :", metriques_modele$ece_test, "\n")
cat("  Seuil bas (Youden) :", metriques_modele$seuil_bas, "\n")
cat("  Seuil haut (Rule-In))   :", metriques_modele$seuil_haut, "\n")

# Sauvegardes annexes
write.csv(as.data.frame(tuned$results),
          file.path(path_funcml_ready, "hyperparameter_grid_full_2F.csv"))

saveRDS(cal_final, file.path(path_funcml_ready, "cal_final_2F.rds"))
saveRDS(shap_patient, file.path(path_funcml_ready, "shap_patient_2F.rds"))
saveRDS(perm_cls, file.path(path_funcml_ready, "perm_importance_2F.rds"))

