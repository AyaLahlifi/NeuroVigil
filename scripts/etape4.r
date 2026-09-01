########################################################################
# SCRIPT 04 : Fonction de prédiction à 3 niveaux
########################################################################

library(tidyverse)
library(funcml)
library(recipes)

# ============================================================================
# 1. CHEMINS
# ============================================================================

project_root <- "C:/Users/ayala/OneDrive/Attachments/Documents/TEST_FUNCML/data-ADNI-stage02"
path_funcml_ready <- file.path(project_root, "data/processed/Tabular data/funcml_ready")
NOM_RECETTE <- "recette_imputation.rds"

# ============================================================================
# 2. CHARGER MODELE + METRIQUES + RECIPE
# ============================================================================

fit_final <- readRDS(file.path(path_funcml_ready, "modele_final_xgboost_2F.rds"))
metriques_modele <- readRDS(file.path(path_funcml_ready, "metriques_modele_2F.rds"))
recipe_prep <- readRDS(file.path(path_funcml_ready, NOM_RECETTE))

# Récupérer les DEUX seuils
SEUIL_BAS  <- metriques_modele$seuil_bas
SEUIL_HAUT <- metriques_modele$seuil_haut

cat("Seuil bas  (Youden) :", round(SEUIL_BAS, 3), "\n")
cat("Seuil haut (Rule-In)    :", round(SEUIL_HAUT, 3), "\n")

# ============================================================================
# 3. CONSTRUIRE LA REFERENCE TRAIN UNIQUEMENT
# ============================================================================

data_train <- read_csv(
  file.path(path_funcml_ready, "adni_train_imputed.csv"),
  show_col_types = FALSE
)

data_train_ref <- data_train %>%
  select(-any_of(c("RID", "event", "time_to_event", "target", "GENOTYPE", "a_une_irm")))

saveRDS(data_train_ref, file.path(path_funcml_ready, "data_train_reference.rds"))

# ============================================================================
# 4. VARIABLES ATTENDUES PAR LE MODELE ET LA RECIPE
# ============================================================================

vars_attendues <- metriques_modele$variables_modele

vars_recipe <- recipe_prep$var_info %>%
  filter(role == "predictor") %>%
  pull(variable)

# ============================================================================
# 5. RECUPERER LES NIVEAUX DES FACTEURS
# ============================================================================

ref_recipe <- recipe_prep$steps[[1]]$ref_data
niveaux_genotype <- levels(ref_recipe$GENOTYPE)
niveaux_ptgender <- levels(ref_recipe$PTGENDER)

cat("\nNiveaux GENOTYPE :", paste(niveaux_genotype, collapse = ", "), "\n")
cat("Niveaux PTGENDER :", paste(niveaux_ptgender, collapse = ", "), "\n")

# ============================================================================
# 6. FONCTION DE PREDICTION À 3 NIVEAUX
# ============================================================================

predire_nouveau_patient <- function(patient_data, fit_final, recipe_prep,
                                    seuil_bas, seuil_haut) {
  
  patient_transforme <- patient_data
  
  # --- PTGENDER ---
  if ("PTGENDER" %in% names(patient_transforme)) {
    if (is.numeric(patient_transforme$PTGENDER) || is.integer(patient_transforme$PTGENDER)) {
      patient_transforme$PTGENDER <- factor(
        patient_transforme$PTGENDER, levels = c(1, 2), labels = niveaux_ptgender
      )
    } else {
      patient_transforme$PTGENDER <- factor(
        as.character(patient_transforme$PTGENDER), levels = niveaux_ptgender
      )
    }
  } else {
    patient_transforme$PTGENDER <- factor(NA, levels = niveaux_ptgender)
  }
  
  # --- GENOTYPE ---
  if ("GENOTYPE" %in% names(patient_transforme)) {
    patient_transforme$GENOTYPE <- factor(
      as.character(patient_transforme$GENOTYPE), levels = niveaux_genotype
    )
  } else {
    patient_transforme$GENOTYPE <- factor(NA, levels = niveaux_genotype)
  }
  
  # --- n_e4 ---
  patient_transforme$n_e4 <- stringr::str_count(
    as.character(patient_transforme$GENOTYPE), "4"
  )
  
  # --- Ajouter les variables manquantes attendues par la recipe ---
  manquantes <- setdiff(vars_recipe, names(patient_transforme))
  if (length(manquantes) > 0) {
    for (col in manquantes) patient_transforme[[col]] <- NA
  }
  
  # --- Variables mesurées vs estimées ---
  variables_presentes <- intersect(vars_attendues, names(patient_transforme))
  variables_mesurees <- variables_presentes[!is.na(patient_transforme[1, variables_presentes])]
  variables_estimees <- variables_presentes[is.na(patient_transforme[1, variables_presentes])]
  
  # --- BAKE ---
  patient_impute <- bake(recipe_prep, new_data = patient_transforme)
  
  # --- Garder uniquement les variables du modèle ---
  variables_manquantes <- setdiff(vars_attendues, names(patient_impute))
  if (length(variables_manquantes) > 0) {
    stop(paste("Variables manquantes après bake:", paste(variables_manquantes, collapse = ", ")))
  }
  patient_impute <- patient_impute %>% select(all_of(vars_attendues))
  
  # --- Prédiction ---
  proba_yes <- predict(fit_final, patient_impute, type = "prob")[, "Yes"]
  
  # --- Décision à 3 NIVEAUX ---
  decision <- ifelse(
    proba_yes >= seuil_haut, "Haut risque",
    ifelse(proba_yes >= seuil_bas, "Risque modéré", "Risque faible")
  )
  
  # --- Interprétation ---
  interpretation <- case_when(
    proba_yes >= seuil_haut ~ paste0(
      "Risque ÉLEVÉ de conversion dans 24 mois ",
      "(probabilité ", round(proba_yes * 100, 1), "%, au-dessus du seuil haut ",
      round(seuil_haut, 3), ") - suivi rapproché recommandé"
    ),
    proba_yes >= seuil_bas ~ paste0(
      "Risque MODÉRÉ de conversion dans 24 mois ",
      "(probabilité ", round(proba_yes * 100, 1), "%, entre les seuils ",
      round(seuil_bas, 3), " et ", round(seuil_haut, 3), ") - suivi attentif"
    ),
    TRUE ~ paste0(
      "Risque FAIBLE de conversion dans 24 mois ",
      "(probabilité ", round(proba_yes * 100, 1), "%, sous le seuil ",
      round(seuil_bas, 3), ") - suivi standard"
    )
  )
  
  return(list(
    probabilite_conversion = round(proba_yes, 3),
    decision               = decision,
    seuil_bas              = seuil_bas,
    seuil_haut             = seuil_haut,
    variables_mesurees     = variables_mesurees,
    variables_estimees     = variables_estimees,
    interpretation         = interpretation
  ))
}

cat("\n✅ Fonction predire_nouveau_patient() chargée avec succès.\n")

# ============================================================================
# 7. EXEMPLE D'UTILISATION
# ============================================================================

nouveau_patient <- data.frame(
  RID = 9999, entry_age = 72, PTEDUCAT = 14, PTGENDER = 1,
  MMSCORE = 24, CDRSB = 4, CDGLOBAL = 0.5, MOCA = 22,
  FAQTRAVL = 3, TOTAL13 = 15, TOTSCORE = 20,
  LIMMTOTAL = 8, LDELTOTAL = 7, GENOTYPE = "3/4"
)

resultat <- predire_nouveau_patient(
  patient_data = nouveau_patient,
  fit_final    = fit_final,
  recipe_prep  = recipe_prep,
  seuil_bas    = SEUIL_BAS,
  seuil_haut   = SEUIL_HAUT
)

cat("\n==================================================\n")
cat("RÉSULTAT - Patient", nouveau_patient$RID, "\n")
cat("Probabilité :", resultat$probabilite_conversion * 100, "%\n")
cat("Décision    :", resultat$decision, "\n")
cat("Interprétation :", resultat$interpretation, "\n")
cat("==================================================\n")

