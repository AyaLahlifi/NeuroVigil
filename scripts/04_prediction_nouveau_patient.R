########################################################################
# SCRIPT 04 : three-level prediction for a new patient
########################################################################
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(funcml); library(recipes) })

for (f in c(FILES$model, FILES$metrics, FILES$recipe, FILES$train))
  require_file(proc_path(f), "Run scripts 01 and 02 first.")

fit_final        <- readRDS(proc_path(FILES$model))
metriques_modele <- readRDS(proc_path(FILES$metrics))
recipe_prep      <- readRDS(proc_path(FILES$recipe))
SEUIL_BAS  <- metriques_modele$seuil_bas
SEUIL_HAUT <- metriques_modele$seuil_haut
cat("Low threshold (Youden):", round(SEUIL_BAS, 3), "| High threshold (Rule-In):", round(SEUIL_HAUT, 3), "\n")

# Reference cohort of the app (training data only). Contains ADNI data: do not commit.
data_train_ref <- read_csv(proc_path(FILES$train), show_col_types = FALSE) %>% select(-any_of(NON_PREDICTORS))
saveRDS(data_train_ref, proc_path(FILES$reference))

vars_attendues   <- metriques_modele$variables_modele
vars_recipe      <- recipe_prep$var_info %>% filter(role == "predictor") %>% pull(variable)
ref_recipe       <- recipe_prep$steps[[1]]$ref_data
niveaux_genotype <- levels(ref_recipe$GENOTYPE)
niveaux_ptgender <- levels(ref_recipe$PTGENDER)

predire_nouveau_patient <- function(patient_data, fit_final, recipe_prep, seuil_bas, seuil_haut) {
  p <- patient_data
  p$PTGENDER <- if ("PTGENDER" %in% names(p)) {
    if (is.numeric(p$PTGENDER)) factor(p$PTGENDER, levels = c(1, 2), labels = niveaux_ptgender)
    else factor(as.character(p$PTGENDER), levels = niveaux_ptgender)
  } else factor(NA, levels = niveaux_ptgender)
  p$GENOTYPE <- if ("GENOTYPE" %in% names(p)) factor(as.character(p$GENOTYPE), levels = niveaux_genotype)
                else factor(NA, levels = niveaux_genotype)
  p$n_e4 <- stringr::str_count(as.character(p$GENOTYPE), "4")

  for (col in setdiff(vars_recipe, names(p))) p[[col]] <- NA          # columns expected by the recipe

  presentes          <- intersect(vars_attendues, names(p))
  variables_mesurees <- presentes[!is.na(p[1, presentes])]
  variables_estimees <- presentes[ is.na(p[1, presentes])]

  p_imp <- bake(recipe_prep, new_data = p)
  miss  <- setdiff(vars_attendues, names(p_imp))
  if (length(miss) > 0) stop("Variables missing after bake(): ", paste(miss, collapse = ", "))
  p_imp <- p_imp %>% select(all_of(vars_attendues))

  proba <- predict(fit_final, p_imp, type = "prob")[, "Yes"]
  decision <- ifelse(proba >= seuil_haut, "Haut risque", ifelse(proba >= seuil_bas, "Risque modéré", "Risque faible"))
  list(probabilite_conversion = round(proba, 3), decision = decision,
       seuil_bas = seuil_bas, seuil_haut = seuil_haut,
       variables_mesurees = variables_mesurees, variables_estimees = variables_estimees)
}

# Example --------------------------------------------------------------------------------
nouveau_patient <- data.frame(RID = 9999, entry_age = 72, PTEDUCAT = 14, PTGENDER = 1,
  MMSCORE = 24, CDRSB = 4, CDGLOBAL = 0.5, MOCA = 22, FAQTRAVL = 3, TOTAL13 = 15,
  TOTSCORE = 20, LIMMTOTAL = 8, LDELTOTAL = 7, GENOTYPE = "3/4")
resultat <- predire_nouveau_patient(nouveau_patient, fit_final, recipe_prep, SEUIL_BAS, SEUIL_HAUT)
cat("Probability:", resultat$probabilite_conversion * 100, "% | Decision:", resultat$decision, "\n")
cat("Measured variables:", length(resultat$variables_mesurees), "of", length(vars_attendues), "\n")
