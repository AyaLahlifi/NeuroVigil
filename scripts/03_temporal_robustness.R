########################################################################
# SCRIPT 03 : temporal robustness (12 / 24 / 36 months)
########################################################################
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(funcml); library(recipes) })

HORIZONS           <- c(12, 24, 36)
COMPARE_WITH_LEAKY <- FALSE

require_file(PATHS$raw, "Place FINAL_DATASET.csv in data/raw/ or set NEUROVIGIL_DATA.")
df_brut <- read_csv(PATHS$raw, show_col_types = FALSE)

tester_horizon <- function(df, horizon_mois, leaky = FALSE) {
  df <- df %>% mutate(target = factor(if_else(event == 1 & time_to_event <= horizon_mois, "Yes", "No"),
                                      levels = c("No", "Yes")))
  keep   <- names(which(colMeans(is.na(df)) <= MAX_MISSING_PCT / 100))
  df_sel <- df %>%
    select(all_of(keep), -any_of(DROP_COLS)) %>%
    mutate(PTGENDER = factor(PTGENDER, levels = c(1, 2), labels = c("Homme", "Femme")),
           n_e4 = str_count(GENOTYPE, "4"), GENOTYPE = factor(GENOTYPE))

  set.seed(SEED)                                              # split BEFORE imputation
  idx_test  <- sample(nrow(df_sel), size = round(TEST_FRACTION * nrow(df_sel)))
  train_raw <- df_sel[-idx_test, ];  test_raw <- df_sel[idx_test, ]

  if (!leaky) {
    vars_imp <- setdiff(names(train_raw), ID_COLS)
    rec <- recipe(~ ., data = train_raw[vars_imp]) %>%
      step_impute_knn(all_predictors(), neighbors = 5) %>% prep(training = train_raw[vars_imp])
    data_train <- bind_cols(train_raw[ID_COLS], bake(rec, new_data = train_raw[vars_imp]))
    data_test  <- bind_cols(test_raw[ID_COLS],  bake(rec, new_data = test_raw[vars_imp]))
  } else {                                                    # OLD, LEAKY: outcome columns in the recipe
    rec <- recipe(target ~ ., data = train_raw) %>% step_impute_knn(all_predictors()) %>% prep(training = train_raw)
    data_train <- bake(rec, new_data = NULL);  data_test <- bake(rec, new_data = test_raw)
  }

  vars_pred   <- setdiff(names(data_train), NON_PREDICTORS)
  formula_cls <- reformulate(vars_pred, response = "target")

  cmp <- compare_learners(data = data_train, formula = formula_cls,
                          models = c("glm", "ranger", "xgboost"),
                          resampling = cv(v = 5, seed = SEED), metrics = "auc", type = "prob", seed = SEED)
  cmp_list <- unclass(cmp)
  cmp_df   <- cmp_list[[which(sapply(cmp_list, function(x) is.data.frame(x) && "model" %in% names(x)))[1]]]

  fit_h <- fit(formula = formula_cls, data = data_train, model = "xgboost")
  probs <- predict(fit_h, data_test, type = "prob")[, "Yes"]
  pos <- probs[data_test$target == "Yes"]; neg <- probs[data_test$target == "No"]
  r <- rank(c(pos, neg))
  auc_test <- (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) / (length(pos) * length(neg))

  perm <- interpret(fit_h, data_test, formula = formula_cls, method = "permute",
                    metric = "auc", type = "prob", nsim = 10, seed = SEED)
  sc   <- perm$result$scores
  cv_  <- intersect(c("feature", "Variable", "variable", "term"), names(sc))[1]
  ci_  <- intersect(c("importance", "Importance", "mean_importance", "score"), names(sc))[1]
  top5 <- sc %>% arrange(desc(.data[[ci_]])) %>% slice_head(n = 5) %>% pull(.data[[cv_]])

  tibble(horizon = horizon_mois, imputation = if (leaky) "outcome in recipe (leaky)" else "predictors only",
         n_yes = sum(data_train$target == "Yes") + sum(data_test$target == "Yes"),
         n_no  = sum(data_train$target == "No")  + sum(data_test$target == "No"),
         auc_cv_xgboost = cmp_df %>% filter(model == "xgboost", metric == "auc") %>% pull(mean),
         auc_test = auc_test, top5_variables = paste(top5, collapse = ", "))
}

grid <- expand.grid(h = HORIZONS, leaky = if (COMPARE_WITH_LEAKY) c(TRUE, FALSE) else FALSE)
resultats_table <- map2(grid$h, grid$leaky, ~ tester_horizon(df_brut, .x, .y)) %>%
  bind_rows() %>% arrange(horizon, imputation)
print(resultats_table, width = Inf)

saveRDS(resultats_table, proc_path(FILES$temporal))
write.csv(resultats_table, out_path("temporal_robustness_table.csv"), row.names = FALSE)
message("Script 03 done.")
