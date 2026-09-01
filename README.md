# NeuroVigil: AI-Powered Clinical Decision Support for Alzheimer's Prediction

[![R](https://img.shields.io/badge/R-4.3.0-276DC3.svg?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-0078D7.svg?style=flat&logo=shiny&logoColor=white)](https://shiny.rstudio.com/)

**NeuroVigil** is a trilingual (French, English, Arabic) Clinical Decision Support System (CDSS) designed to predict the risk of Mild Cognitive Impairment (MCI) converting to Alzheimer's Disease (AD) within a 24-month window. 

Developed for the **Centre Mohammed VI de la Recherche et de l'Innovation (CM6RI)** and the **Université Mohammed VI des Sciences de la Santé (UM6SS)**, this tool bridges the gap between advanced, leakage-safe machine learning and frontline neurological practice.

🚀 **[Launch the Live Web Application](https://neuropredictai.shinyapps.io/NeuroVigil_App/)**

---

## Key Features

- **Leakage-Safe KNN Imputation:** Handles missing clinical data in real-time. Imputation is strictly fit on the training cohort and applied to new data, preventing data leakage and preserving statistical integrity.
- **Dual-Threshold Risk Stratification:** Moves beyond arbitrary cutoffs. Utilizes **Youden’s Index (τ = 0.124)** for sensitive screening and a **"Rule-In" operating point (τ = 0.369, Specificity ≥ 90%)** to ensure high-risk flags carry a <10% false-positive rate, justifying intensive clinical intervention.
- **Explainable AI (XAI):** Integrates SHAP, Permutation Importance, and Accumulated Local Effects (ALE) to provide transparent, interpretable risk scores for clinicians.
- **Algorithmic Fairness:** Rigorously audited across sex, APOE-ε4 status, and education levels to ensure equitable predictions and mitigate algorithmic bias.
- **Trilingual & RTL Support:** Fully localized interface for French, English, and Arabic, including native Right-to-Left (RTL) layout support.
- **Batch & Single Processing:** Supports both individual patient analysis and bulk Excel-based cohort screening for epidemiological surveillance.
- ** Automated PDF Reporting:** Generates comprehensive, branded clinical reports featuring calibrated risk gauges, DCA visualizations, and explicit "measured vs. estimated" data transparency.

---

## Architecture & Methodology

NeuroVigil is built on a robust, end-to-end machine learning pipeline validated on the Alzheimer’s Disease Neuroimaging Initiative (ADNI) cohort (N = 1,288).

1. **Strict Data Partitioning:** Raw clinical data is split into training and test sets *before* any preprocessing to strictly prevent data leakage.
2. **KNN Imputation:** Missingness (ranging from 2% to 42% in raw biomarkers) is handled using K-Nearest Neighbors (KNN, *k*=5), fit exclusively on the training subset.
3. **Model Selection & Tuning:** An Extreme Gradient Boosting (XGBoost) model is selected via 5-fold cross-validation benchmarking and tuned via random search (Mean CV AUC = 0.824).
4. **Dual-Threshold Optimization:** 
   - *Lower Threshold (Screening):* Derived via Youden’s Index (τ = 0.124) to minimize missed conversions.
   - *Upper Threshold (Confirmation):* Derived via a "Rule-In" strategy maximizing Youden under a Specificity ≥ 90% constraint (τ = 0.369).
5. **Validation:** Evaluated using Decision Curve Analysis (DCA) for clinical utility and stratified subgroup analysis for fairness (Test AUC = 0.871, ECE = 0.054).
6. **Deployment:** The serialized model and reference artifacts are deployed via an R Shiny web application with dynamic inference capabilities.


<img width="8189" height="7547" alt="archi" src="https://github.com/user-attachments/assets/346b7019-8229-49b4-ab94-62f138601638" />

---

## Tech Stack

- **Language:** R
- **Framework:** Shiny
- **Machine Learning:** XGBoost, `funcml`
- **Data Handling:** `tidyverse`, `readxl`, `writexl`, `recipes`
- **Reporting & UI:** `pagedown`, `base64enc`, `DT` (DataTables), `rmarkdown`

---

## Project Structure
```text
NeuroVigil/
├── README.md
├── NeuroVigil_App/
│   ├── app.R                  # Main Shiny application logic and UI
│   ├── www/                   # Static assets (logos, icons)
│   │   ├── logo_cm6ri.png
│   │   └── CM6RI-Picto32.png
│   └── data/                  # Serialized model artifacts and reference data
│       ├── modele_final_xgboost_2F.rds
│       ├── metriques_modele_2F.rds
│       ├── recette_imputation.rds
│       └── data_train_reference.rds
└── scripts/                   
    ├── etape1.R
    ├── etape2.R
    ├── etape3.R
    ├── etape4.R
    └── etape5.R
```
## Data Availability & Ethics
-**Training Data:** This model was trained on the Alzheimer’s Disease Neuroimaging Initiative (ADNI) dataset. ADNI data is subject to a Data Use Agreement and is not included in this repository. Researchers can request access at adni.loni.usc.edu.
-**Model Artifacts:** The repository contains only the serialized model weights (.rds files) and a synthetic reference cohort structure, ensuring no raw patient data is exposed.
-**Ethical Auditing:** The model includes built-in fairness metrics to ensure equitable performance across diverse demographic subgroups, with transparent reporting of known disparities (e.g., APOE-ε4 status).



##  How to Run Locally

To run this application on your local machine, follow these steps:

### 1. Prerequisites
Ensure you have R (>= 4.0.0) and RStudio installed.

### 2. Clone the Repository
```bash
git clone https://github.com/AyaLahlifi/NeuroVigil.git
cd NeuroVigil

