# NeuroVigil app

`app.R` must load its objects with paths relative to this folder (shinyapps.io only deploys files inside the app folder):

```r
data_dir    <- "data"                      # app/data/, copied from data/processed/ at deployment time
modele      <- readRDS(file.path(data_dir, "modele_final_xgboost_2F.rds"))
data_ref    <- readRDS(file.path(data_dir, "data_train_reference.rds"))
metriques   <- readRDS(file.path(data_dir, "metriques_modele_2F.rds"))
recipe_prep <- readRDS(file.path(data_dir, "recette_imputation.rds"))
SEUIL_BAS   <- metriques$seuil_bas
SEUIL_HAUT  <- metriques$seuil_haut
```

Do not use `getwd()` + absolute sub-paths, and do not commit `app/data/*.rds`: the recipe and the reference cohort contain ADNI training data.
