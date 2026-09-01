########################################################################
# SCRIPT 01 : Génération du jeu de TEST (imputé avec la recipe du TRAIN)
########################################################################

library(tidyverse)
library(recipes)

project_root <- "C:/Users/ayala/OneDrive/Attachments/Documents/TEST_FUNCML/data-ADNI-stage02"

path_funcml_ready <- file.path(
  project_root,
  "data/processed/Tabular data/funcml_ready"
)

path_tabular <- file.path(
  project_root,
  "data/processed/Tabular data"
)

NOM_RECETTE <- "recette_imputation.rds"

# 1. LOAD TRAIN + ORIGINAL DATA + RECIPE APPRISE SUR TRAIN

data_train <- read_csv(
  file.path(path_funcml_ready, "adni_train_imputed.csv"),
  show_col_types = FALSE
)

data_original <- read_csv(
  file.path(path_tabular, "FINAL_DATASET.csv"),
  show_col_types = FALSE
)

recipe_prep <- readRDS(
  file.path(path_funcml_ready, NOM_RECETTE)
)

cat("Nombre de lignes TRAIN :", nrow(data_train), "\n")
cat("Nombre de lignes FINAL :", nrow(data_original), "\n")

# 2. CHECK RID

if (!"RID" %in% names(data_train) ||
    !"RID" %in% names(data_original)) {
  stop("La colonne RID est absente d'un des deux fichiers.")
}

cat("RID uniques TRAIN :", n_distinct(data_train$RID), "\n")
cat("RID uniques FINAL :", n_distinct(data_original$RID), "\n")

# 3. CREATE TEST RAW = OBSERVATIONS NOT PRESENT IN TRAIN

rids_train <- unique(data_train$RID)

data_test_raw <- data_original %>%
  filter(!RID %in% rids_train)

cat("TRAIN :", nrow(data_train), "\n")
cat("TEST  :", nrow(data_test_raw), "\n")
cat("TOTAL :", nrow(data_train) + nrow(data_test_raw), "\n")



if (!all(c("event", "time_to_event") %in% names(data_test_raw))) {
  stop("Colonnes event/time_to_event absentes de FINAL_DATASET.csv - impossible de calculer target.")
}

data_test_raw <- data_test_raw %>%
  mutate(
    target = factor(
      case_when(
        event == 1 & time_to_event <= 24 ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    )
  )

identifiants_test <- data_test_raw %>%
  select(RID, event, time_to_event, target)

cat("\n Repartition de target dans le TEST :\n")
print(table(identifiants_test$target))

# 4. VERIFY NO RID OVERLAP


overlap <- intersect(
  unique(data_train$RID),
  unique(data_test_raw$RID)
)

cat("\nRID communs TRAIN / TEST :", length(overlap), "\n")

if (length(overlap) > 0) {
  stop("ERREUR : il existe des RID communs entre TRAIN et TEST.")
}

cat("Aucun RID commun entre TRAIN et TEST.\n")

# 5. PREPARE TEST RAW DANS LE MÊME FORMAT QUE LE TRAIN AVANT PREP()

data_test_raw <- data_test_raw %>%
  mutate(
    PTGENDER = factor(
      PTGENDER,
      levels = c(1, 2),
      labels = c("Homme", "Femme")
    ),
    n_e4 = str_count(as.character(GENOTYPE), "4"),
    GENOTYPE = factor(GENOTYPE)
  )

vars_a_supprimer <- intersect(
  c("EXAMDATE", "STATUS", "subject_id", "entry_date", "visit"),
  names(data_test_raw)
)

data_test_raw <- data_test_raw %>%
  select(-any_of(vars_a_supprimer))

# 6. IMPUTER LE TEST AVEC LA RECIPE APPRISE SUR LE TRAIN

data_test_imputed <- bake(recipe_prep, new_data = data_test_raw)
data_test_imputed <- bind_cols(identifiants_test, data_test_imputed)

# 7. SAVE TEST

path_test <- file.path(
  path_funcml_ready,
  "adni_test_imp_2.csv"
)

write_csv(
  data_test_imputed,
  path_test
)

cat(path_test, "\n")
cat("\nNombre de lignes TEST :", nrow(data_test_imputed), "\n")
cat("Nombre de colonnes TEST :", ncol(data_test_imputed), "\n")

# 8. FINAL CHECK

cat("\n Valeurs NA restantes dans TEST :\n")

na_summary <- data_test_imputed %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  filter(n_na > 0)

print(na_summary)
