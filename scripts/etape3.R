########################################################################
# SCRIPT 03 : Analyse de sensibilité et validation de la robustesse
########################################################################

library(tidyverse)
library(funcml)
library(recipes)

project_root <- "C:/Users/ayala/OneDrive/Attachments/Documents/TEST_FUNCML/data-ADNI-stage02"
path_tabular <- file.path(project_root, "data/processed/Tabular data")

df_brut <- read_csv(file.path(path_tabular, "FINAL_DATASET.csv"))

tester_horizon <- function(df, horizon_mois, seuil_na = 60) {
  
  df <- df %>%
    mutate(target = case_when(
      event == 1 & time_to_event <= horizon_mois ~ "Yes",
      TRUE                                        ~ "No"
    ),
    target = factor(target, levels = c("No", "Yes")))
  
  miss_summary <- df %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "pct_na")
  vars_gardees <- miss_summary %>% filter(pct_na <= seuil_na) %>% pull(variable)
  
  df_sel <- df %>%
    select(all_of(vars_gardees)) %>%
    select(-any_of(c("EXAMDATE", "STATUS", "subject_id", "entry_date", "visit"))) %>%
    mutate(
      PTGENDER = factor(PTGENDER, levels = c(1, 2), labels = c("Homme", "Femme")),
      n_e4     = str_count(GENOTYPE, "4"),
      GENOTYPE = factor(GENOTYPE)
    )

  # SPLIT AVANT IMPUTATION 
  set.seed(42)
  idx_test <- sample(nrow(df_sel), size = round(0.2 * nrow(df_sel)))
  data_train_raw <- df_sel[-idx_test, ]
  data_test_raw  <- df_sel[idx_test, ]
  
  # IMPUTATION : recipe apprise sur TRAIN, appliquée au TEST via bake()
  recipe_h <- recipe(target ~ ., data = data_train_raw) %>%
    step_impute_knn(all_predictors())
  recipe_h_prep <- prep(recipe_h, training = data_train_raw)
  
  data_train <- bake(recipe_h_prep, new_data = NULL)
  data_test  <- bake(recipe_h_prep, new_data = data_test_raw)
  
  vars_predicteurs <- data_train %>%
    select(-RID, -event, -time_to_event, -target, -GENOTYPE) %>%
    names()
  formula_cls <- as.formula(paste("target ~", paste(vars_predicteurs, collapse = " + ")))
  
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
    stop("Tableau non trouvé dans cmp. Eléments disponibles : ",
         paste(names(cmp_liste), collapse = ", "))
  }
  
  fit_h <- fit(formula = formula_cls, data = data_train, model = "xgboost")
  
  # Calcul manuel de l'AUC
  probs_test <- predict(fit_h, data_test, type = "prob")[, "Yes"]
  labels_test <- data_test$target
  pos <- probs_test[labels_test == "Yes"]
  neg <- probs_test[labels_test == "No"]
  r <- rank(c(pos, neg))
  perf_test <- (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
    (length(pos) * length(neg))
  
  perm_h <- interpret(fit_h, data_test, formula = formula_cls, method = "permute",
                      metric = "auc", type = "prob", nsim = 10, seed = 42)
  
  perm_df <- perm_h$result$scores
  col_variable <- intersect(c("feature", "Variable", "variable", "term"), names(perm_df))[1]
  col_importance <- intersect(c("importance", "Importance", "mean_importance", "score"), names(perm_df))[1]
  
  if (is.na(col_variable) || is.na(col_importance)) {
    stop("Colonnes non reconnues dans perm_h$result$scores. Noms réels : ",
         paste(names(perm_df), collapse = ", "))
  }
  
  top5 <- perm_df %>%
    arrange(desc(.data[[col_importance]])) %>%
    slice_head(n = 5) %>%
    pull(.data[[col_variable]])
  
  list(
    horizon        = horizon_mois,
    n_yes          = sum(data_train$target == "Yes") + sum(data_test$target == "Yes"),
    n_no           = sum(data_train$target == "No")  + sum(data_test$target == "No"),
    auc_cv_xgboost = cmp_df %>% filter(model == "xgboost", metric == "auc") %>% pull(mean),
    auc_test       = perf_test,
    top5_variables = paste(top5, collapse = ", ")
  )
}

## tester sur 3 horizons ------------------------------------------
horizons_a_tester <- c(12, 24, 36)

resultats <- map(horizons_a_tester, ~ tester_horizon(df_brut, .x))
resultats_table <- bind_rows(lapply(resultats, as_tibble))

print(resultats_table)

path_funcml_ready <- file.path(project_root, "data/processed/Tabular data/funcml_ready")
saveRDS(resultats_table, file.path(path_funcml_ready, "resultats_table_2F.rds"))
write.csv(resultats_table, file.path(path_funcml_ready, "temporal_robustness_table_2F.csv"),
          row.names = FALSE)
