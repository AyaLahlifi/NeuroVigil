########################################################################
# R/00_config.R : shared configuration, sourced at the top of every script
# ----------------------------------------------------------------------
# No absolute path anywhere. Everything is relative to the project root
# (found by the 'here' package) or to NEUROVIGIL_DATA when the ADNI data
# live outside the repository:
#   Sys.setenv(NEUROVIGIL_DATA = "D:/my_adni_data")   # or in ~/.Renviron
########################################################################

if (!requireNamespace("here", quietly = TRUE))
  stop("Please install the 'here' package: install.packages('here')")

DATA_ROOT <- Sys.getenv("NEUROVIGIL_DATA", unset = here::here("data"))

PATHS <- list(
  raw       = file.path(DATA_ROOT, "raw", "FINAL_DATASET.csv"),
  processed = file.path(DATA_ROOT, "processed"),
  outputs   = here::here("outputs")
)
for (d in c(PATHS$processed, PATHS$outputs)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Names of the files produced and consumed by the scripts
FILES <- list(
  train     = "adni_train_imputed.csv",
  test      = "adni_test_imputed.csv",
  split     = "idx_test_split.rds",
  recipe    = "recette_imputation.rds",
  model     = "modele_final_xgboost_2F.rds",
  metrics   = "metriques_modele_2F.rds",
  reference = "data_train_reference.rds",
  cal       = "cal_final_2F.rds",
  shap      = "shap_patient_2F.rds",
  perm      = "perm_importance_2F.rds",
  temporal  = "temporal_robustness_table.rds"
)
proc_path <- function(name) file.path(PATHS$processed, name)
out_path  <- function(name) file.path(PATHS$outputs, name)

require_file <- function(path, hint = NULL) {
  if (!file.exists(path))
    stop("File not found: ", path, if (!is.null(hint)) paste0("\n", hint), call. = FALSE)
  invisible(path)
}

# Constants used by several scripts
SEED            <- 42
HORIZON_MONTHS  <- 24
MAX_MISSING_PCT <- 60
TEST_FRACTION   <- 0.20
VAL_FRACTION    <- 0.20
ID_COLS         <- c("RID", "event", "time_to_event", "target")        # never used as predictors
DROP_COLS       <- c("EXAMDATE", "STATUS", "subject_id", "entry_date", "visit")
NON_PREDICTORS  <- c(ID_COLS, "GENOTYPE", "a_une_irm")                 # excluded from the model formula
