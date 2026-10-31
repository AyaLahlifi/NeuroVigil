########################################################################
# SCRIPT 01 : target, variable selection, split BEFORE imputation,
#             KNN imputation fitted on TRAIN only, test imputed with the TRAIN recipe
########################################################################
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(recipes) })

require_file(PATHS$raw, "Place FINAL_DATASET.csv (ADNI, not distributed) in data/raw/ or set NEUROVIGIL_DATA.")
df_brut <- read_csv(PATHS$raw, show_col_types = FALSE)

# 1. Target: conversion within HORIZON_MONTHS ------------------------------------------
df_brut <- df_brut %>%
  mutate(target = factor(if_else(event == 1 & time_to_event <= HORIZON_MONTHS, "Yes", "No"),
                         levels = c("No", "Yes")))
print(table(df_brut$target))

# 2. Variable selection and recoding ---------------------------------------------------
keep   <- names(which(colMeans(is.na(df_brut)) <= MAX_MISSING_PCT / 100))
df_sel <- df_brut %>%
  select(all_of(keep), -any_of(DROP_COLS)) %>%
  mutate(PTGENDER = factor(PTGENDER, levels = c(1, 2), labels = c("Homme", "Femme")),
         n_e4     = str_count(GENOTYPE, "4"),            # number of APOE-e4 alleles
         GENOTYPE = factor(GENOTYPE))
cat("Columns kept:", ncol(df_sel), "\n")

# 3. Split BEFORE any imputation -------------------------------------------------------
set.seed(SEED)
idx_test  <- sample(nrow(df_sel), size = round(TEST_FRACTION * nrow(df_sel)))
train_raw <- df_sel[-idx_test, ]
test_raw  <- df_sel[idx_test, ]
cat("TRAIN:", nrow(train_raw), "| TEST:", nrow(test_raw), "\n")

# 4. KNN imputation: recipe fitted on TRAIN predictors only ----------------------------
#    The outcome columns (RID, event, time_to_event, target) are NOT given to the recipe.
vars_imp    <- setdiff(names(train_raw), ID_COLS)
recipe_prep <- recipe(~ ., data = train_raw[vars_imp]) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  prep(training = train_raw[vars_imp])

data_train <- bind_cols(train_raw[ID_COLS], bake(recipe_prep, new_data = train_raw[vars_imp]))
data_test  <- bind_cols(test_raw[ID_COLS],  bake(recipe_prep, new_data = test_raw[vars_imp]))

stopifnot(!anyNA(data_train), !anyNA(data_test),
          length(intersect(data_train$RID, data_test$RID)) == 0)
cat("Converters  TRAIN:", sum(data_train$target == "Yes"), "| TEST:", sum(data_test$target == "Yes"), "\n")

# 5. Safety check against files from a previous run ------------------------------------
old_train <- proc_path(FILES$train)
if (file.exists(old_train)) {
  old_rids <- read_csv(old_train, col_select = RID, show_col_types = FALSE)$RID
  if (!setequal(old_rids, data_train$RID))
    stop("The split differs from the existing training file (", old_train, ").\n",
         "Nothing was overwritten. Compare with your original preparation script before continuing.")
  message("Split identical to the previous run.")
}

# 6. Save ------------------------------------------------------------------------------
write_csv(data_train, proc_path(FILES$train))
write_csv(data_test,  proc_path(FILES$test))
saveRDS(idx_test,     proc_path(FILES$split))
saveRDS(recipe_prep,  proc_path(FILES$recipe))      # reused by the app (contains ADNI training data: do not commit)
message("Script 01 done.")
