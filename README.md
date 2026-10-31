# NeuroVigil

Interpretable and equitable prediction of 24-month MCI-to-Alzheimer's conversion on ADNI, built with
[funcml](https://github.com/ielbadisy/funcml), deployed as a trilingual (FR/EN/AR) R Shiny application.

> **Research prototype, not a medical device.** The model has not been externally validated and must not be used for clinical decisions.

- Tutorial: *(link to the published HTML tutorial)*
- Live app: https://neuropredictai.shinyapps.io/NeuroVigil_App/

## Repository layout

```
R/00_config.R                       shared configuration (paths, seeds, constants); no absolute paths
scripts/01_data_preparation.R       target, split, leakage-free KNN imputation
scripts/02_train_model.R            benchmark, tuning, final model, interpretation, calibration, thresholds
scripts/03_temporal_robustness.R    12 / 24 / 36 months
scripts/04_prediction_nouveau_patient.R   three-level prediction for a new patient
scripts/05_dca_fairness.R           decision curve analysis, fairness audit
scripts/run_all.R                   runs 01 to 05
app/app.R                           Shiny application
data/raw/                           put FINAL_DATASET.csv here (not distributed)
data/processed/, outputs/           generated files
```

## Reproducing the results

1. **Get the data.** ADNI data are not distributed. Apply for access at https://adni.loni.usc.edu/ and place
   `FINAL_DATASET.csv` in `data/raw/`, or point to another folder with `Sys.setenv(NEUROVIGIL_DATA = "path/to/data")`
   (it must contain `raw/FINAL_DATASET.csv`).
2. **Open `NeuroVigil.Rproj`** in RStudio (workspace restore is disabled on purpose) and install the packages:
   `install.packages(c("here","tidyverse","recipes","funcml","pROC","xgboost","ranger","gbm"))`
   (funcml: `remotes::install_github("ielbadisy/funcml")`; for the app also `shiny`, `DT`, `readxl`, `writexl`, `pagedown`, `base64enc`).
3. **Run** `source("scripts/run_all.R")`, or the scripts one by one, in order.

Expected main results: test AUC 0.871 (95% CI 0.824 to 0.919), ECE 0.056, thresholds 0.124 and 0.369.

## Lessons learned

In an earlier version of script 03, the imputation recipe was written as `recipe(target ~ ., data = train)`, so
`event` and `time_to_event` served as imputation predictors and leaked outcome information (test AUC inflated by up to 0.038).
Script 03 now imputes with predictors only; set `COMPARE_WITH_LEAKY <- TRUE` in that script to reproduce the comparison.

## Data use

Do not commit ADNI data or files derived from it. The saved imputation recipe and the app's reference cohort contain
ADNI training data and are excluded by `.gitignore`.
