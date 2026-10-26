########################################################################
# SCRIPT 03 : Analyse de sensibilité et validation de la robustesse
########################################################################

library(tidyverse)
library(funcml)
library(recipes)

project_root <- "C:/Users/ayala/OneDrive/Attachments/Documents/TEST_FUNCML/data-ADNI-stage02"
path_tabular <- file.path(project_root, "data/processed/Tabular data")

df_brut <- read_csv(file.path(path_tabular, "FINAL_DATASET.csv"), show_col_types = FALSE)

tester_horizon_corrige <- function(df, horizon_mois, seuil_na = 60) {
  
  # 1. Création de la cible
  df <- df %>%
    mutate(target = case_when(
      event == 1 & time_to_event <= horizon_mois ~ "Yes",
      TRUE                                        ~ "No"
    ),
    target = factor(target, levels = c("No", "Yes")))
  
  # 2. Filtrage des variables avec trop de NA
  miss_summary <- df %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "pct_na")
  vars_gardees <- miss_summary %>% filter(pct_na <= seuil_na) %>% pull(variable)
  
  df_sel <- df %>%
    select(all_of(vars_gardees)) %>%
    select(-any_of(c("EXAMDATE", "STATUS", "subject_id", "entry_date", "visit"))) %>%
    mutate(
      PTGENDER = factor(PTGENDER, levels = c(1, 2), labels = c("Homme", "Femme")),
      n_e4     = str_count(as.character(GENOTYPE), "4"), # Sécurisé avec as.character
      GENOTYPE = factor(GENOTYPE)
    )
  
  # 3. SPLIT AVANT IMPUTATION (Déjà correct dans votre version)
  set.seed(42)
  idx_test <- sample(nrow(df_sel), size = round(0.2 * nrow(df_sel)))
  data_train_raw <- df_sel[-idx_test, ]
  data_test_raw  <- df_sel[idx_test, ]
  
  # 4. IMPUTATION CORRIGÉE (Anti-Leakage)
  recipe_h <- recipe(target ~ ., data = data_train_raw) %>%
    # ⚠️ CORRECTION CLÉ : On exclut RID, event et time_to_event de l'imputation KNN
    update_role(RID, event, time_to_event, new_role = "ignore") %>%
    step_impute_knn(all_predictors(), neighbors = 5) # k=5 explicité pour la reproductibilité
  
  recipe_h_prep <- prep(recipe_h, training = data_train_raw)
  
  data_train <- bake(recipe_h_prep, new_data = NULL)
  data_test  <- bake(recipe_h_prep, new_data = data_test_raw)
  
  # 5. Préparation pour le modèle
  vars_predicteurs <- data_train %>%
    select(-any_of(c("RID", "event", "time_to_event", "target", "GENOTYPE"))) %>%
    names()
  
  formula_cls <- as.formula(paste("target ~", paste(vars_predicteurs, collapse = " + ")))
  
  # 6. Benchmark
  cmp <- compare_learners(
    data = data_train, formula = formula_cls,
    models = c("glm", "ranger", "xgboost"),
    resampling = cv(v = 5, seed = 42),
    metrics = c("auc"), type = "prob", seed = 42
  )
  
  cmp_liste <- unclass(cmp)
  cmp_df <- cmp_liste[[which(sapply(cmp_liste, function(x)
    is.data.frame(x) && "model" %in% names(x)))[1]]]
  
  if (is.null(cmp_df)) {
    stop("Tableau non trouvé dans cmp.")
  }
  
  # 7. Fit final et évaluation
  fit_h <- fit(formula = formula_cls, data = data_train, model = "xgboost")
  
  probs_test <- predict(fit_h, data_test, type = "prob")[, "Yes"]
  labels_test <- data_test$target
  pos <- probs_test[labels_test == "Yes"]
  neg <- probs_test[labels_test == "No"]
  r <- rank(c(pos, neg))
  perf_test <- (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
    (length(pos) * length(neg))
  
  # 8. Interprétabilité (Permutation)
  perm_h <- interpret(fit_h, data_test, method = "permute",
                      metric = "auc", type = "prob", nsim = 10, seed = 42)
  
  perm_df <- perm_h$result$scores
  col_variable <- intersect(c("feature", "Variable", "variable", "term"), names(perm_df))[1]
  col_importance <- intersect(c("importance", "Importance", "mean_importance", "score"), names(perm_df))[1]
  
  top5 <- perm_df %>%
    arrange(desc(.data[[col_importance]])) %>%
    slice_head(n = 5) %>%
    pull(.data[[col_variable]])
  
  # 9. Retour des résultats
  list(
    horizon        = horizon_mois,
    n_yes          = sum(data_train$target == "Yes") + sum(data_test$target == "Yes"),
    n_no           = sum(data_train$target == "No")  + sum(data_test$target == "No"),
    auc_cv_xgboost = cmp_df %>% filter(model == "xgboost", metric == "auc") %>% pull(mean),
    auc_test       = perf_test,
    top5_variables = paste(top5, collapse = ", ")
  )
}

## Exécution sur 3 horizons ------------------------------------------
cat("Lancement de l'analyse de robustesse temporelle ...\n")
horizons_a_tester <- c(12, 24, 36)

resultats <- map(horizons_a_tester, ~ tester_horizon_corrige(df_brut, .x))
resultats_table <- bind_rows(lapply(resultats, as_tibble))

print(resultats_table)

# Sauvegarde des nouveaux résultats
path_funcml_ready <- file.path(project_root, "data/processed/Tabular data/funcml_ready")
saveRDS(resultats_table, file.path(path_funcml_ready, "resultats_table_2F_CORRIGE.rds"))
write.csv(resultats_table, file.path(path_funcml_ready, "temporal_robustness_table_2F_CORRIGE.csv"),
          row.names = FALSE)

cat("\n✅ Analyse de robustesse terminée et sauvegardée avec succès.\n")
