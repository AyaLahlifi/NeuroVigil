########################################################################
# SCRIPT 02 : benchmark, tuning, final XGBoost, interpretation, calibration,
#             dual decision thresholds (validation set), test metrics
########################################################################
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(funcml); library(tidyverse) })

require_file(proc_path(FILES$train), "Run scripts/01_data_preparation.R first.")
data_train_full <- read_csv(proc_path(FILES$train), show_col_types = FALSE)
data_test       <- read_csv(proc_path(FILES$test),  show_col_types = FALSE)
data_train_full$target <- factor(data_train_full$target, levels = c("No", "Yes"))
data_test$target       <- factor(data_test$target,       levels = c("No", "Yes"))

vars_predicteurs <- setdiff(names(data_train_full), NON_PREDICTORS)
formula_cls      <- reformulate(vars_predicteurs, response = "target")

# Fit/tune subset (80%) and validation subset (20%, used ONLY to choose the thresholds)
set.seed(SEED)
idx_val  <- sample(nrow(data_train_full), size = round(VAL_FRACTION * nrow(data_train_full)))
data_val <- data_train_full[idx_val, ]
data_fit <- data_train_full[-idx_val, ]
cat("Fit:", nrow(data_fit), "| Validation:", nrow(data_val), "| Test:", nrow(data_test), "\n")

resampling_cv <- cv(v = 5, seed = SEED)

## Step 1: baseline benchmark ---------------------------------------------------------
cmp_cls <- compare_learners(
  data = data_fit, formula = formula_cls,
  models = c("glm", "rpart", "ranger", "gbm", "xgboost"),
  resampling = resampling_cv, metrics = c("accuracy", "auc", "logloss"),
  type = "prob", seed = SEED)
print(cmp_cls); plot(cmp_cls)

## Step 2: tune XGBoost ---------------------------------------------------------------
grid_xgb <- expand.grid(nrounds = c(100, 200), max_depth = c(3, 4, 6), eta = c(0.05, 0.1))
tuned <- tune(data = data_fit, formula = formula_cls, model = "xgboost",
              grid = grid_xgb, resampling = resampling_cv, metric = "auc",
              search = "random", n_evals = 8, type = "prob", seed = SEED)
print(tuned$best)
write.csv(as.data.frame(tuned$results), out_path("hyperparameter_grid.csv"), row.names = FALSE)

## Step 3: final model ----------------------------------------------------------------
fit_final <- fit(formula = formula_cls, data = data_fit, model = "xgboost",
                 spec = as.list(tuned$best[1, ]))
print(fit_final)

## Step 4: global interpretation (permutation, PDP, ALE) ------------------------------
perm_cls <- interpret(fit_final, data_test, method = "permute",
                      metric = "auc", type = "prob", nsim = 50, seed = SEED)
plot(perm_cls)

perm_df     <- as.data.frame(perm_cls$result$scores)
col_var     <- intersect(c("feature", "Variable", "variable", "term"), names(perm_df))[1]
col_imp     <- intersect(c("importance", "Importance", "mean_importance", "score"), names(perm_df))[1]
top_predictor <- perm_df %>% arrange(desc(.data[[col_imp]])) %>% slice_head(n = 1) %>% pull(.data[[col_var]])
cat("Top predictor:", top_predictor, "\n")

plot(interpret(fit_final, data_test, method = "pdp", features = top_predictor, type = "prob", pos_level = "Yes"))
plot(interpret(fit_final, data_test, method = "ale", features = top_predictor, type = "prob", pos_level = "Yes"))

## Step 5: local explanation (highest-risk test patient) ------------------------------
probs_test   <- predict(fit_final, data_test, type = "prob")[, "Yes"]
top_patient  <- data_test[which.max(probs_test), , drop = FALSE]
shap_patient <- interpret(fit_final, data_test, method = "shap", newdata = top_patient,
                          type = "prob", nsim = 50, seed = SEED)
plot(shap_patient, kind = "waterfall")

## Step 6: calibration ----------------------------------------------------------------
cal_final <- interpret(fit_final, data_test, method = "calibration",
                       type = "prob", pos_level = "Yes", bins = 10)
plot(cal_final, style = "curve")

## Step 7: dual thresholds, chosen on the VALIDATION set ------------------------------
probs_val  <- predict(fit_final, data_val, type = "prob")[, "Yes"]
labels_val <- data_val$target

seuils_test <- seq(0.01, 0.99, by = 0.001)
metrics_df  <- data.frame(
  seuil = seuils_test,
  sens  = sapply(seuils_test, function(s) mean(probs_val[labels_val == "Yes"] >= s)),
  spec  = sapply(seuils_test, function(s) mean(probs_val[labels_val == "No"]  <  s)))
metrics_df$youden <- metrics_df$sens + metrics_df$spec - 1

seuil_bas <- metrics_df$seuil[which.max(metrics_df$youden)]                 # Youden optimum

spec_cible <- 0.90
candidats  <- metrics_df %>% filter(spec >= spec_cible)
if (nrow(candidats) == 0) stop("No threshold reaches the target specificity on the validation set.")
seuil_haut <- candidats$seuil[which.max(candidats$youden)]                  # Rule-In
stopifnot(seuil_haut > seuil_bas)

row_at <- function(s) metrics_df[metrics_df$seuil == s, ]
cat(sprintf("Low threshold  (Youden)  : %.3f | sens %.3f | spec %.3f\n", seuil_bas,  row_at(seuil_bas)$sens,  row_at(seuil_bas)$spec))
cat(sprintf("High threshold (Rule-In) : %.3f | sens %.3f | spec %.3f\n", seuil_haut, row_at(seuil_haut)$sens, row_at(seuil_haut)$spec))
n_val <- length(probs_val)
cat(sprintf("Low %.1f%% | Moderate %.1f%% | High %.1f%% (validation)\n",
            100 * mean(probs_val < seuil_bas),
            100 * mean(probs_val >= seuil_bas & probs_val < seuil_haut),
            100 * mean(probs_val >= seuil_haut)))

## Step 8: final metrics on the TEST set ----------------------------------------------
labels_test <- data_test$target
pos <- probs_test[labels_test == "Yes"]; neg <- probs_test[labels_test == "No"]
r <- rank(c(pos, neg))
auc_test <- (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) / (length(pos) * length(neg))

ece_tbl <- tibble(prob = probs_test, label = as.integer(labels_test == "Yes")) %>%
  mutate(tranche = ntile(prob, 10)) %>% group_by(tranche) %>%
  summarise(gap = abs(mean(prob) - mean(label)) * n(), n = n())
ece_test <- sum(ece_tbl$gap) / sum(ece_tbl$n)
cat(sprintf("TEST AUC = %.3f | ECE = %.3f\n", auc_test, ece_test))

## Step 9: save -----------------------------------------------------------------------
metriques_modele <- list(
  auc_test = round(auc_test, 3), ece_test = round(ece_test, 3),
  seuil_bas = seuil_bas, seuil_haut = seuil_haut,
  seuil_optimal = seuil_bas,                     # kept for compatibility with older app versions
  n_fit = nrow(data_fit), n_val = nrow(data_val), n_test = nrow(data_test),
  date_calcul = Sys.Date(), variables_modele = vars_predicteurs)

saveRDS(fit_final,        proc_path(FILES$model))
saveRDS(metriques_modele, proc_path(FILES$metrics))
saveRDS(cal_final,        proc_path(FILES$cal))
saveRDS(shap_patient,     proc_path(FILES$shap))
saveRDS(perm_cls,         proc_path(FILES$perm))
writeLines(capture.output(sessionInfo()), out_path("sessionInfo_02.txt"))
message("Script 02 done.")
