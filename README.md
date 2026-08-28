#  NeuroVigil: AI-Powered Clinical Decision Support for Alzheimer's Prediction

[![R](https://img.shields.io/badge/R-4.3.0-276DC3.svg?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-0078D7.svg?style=flat&logo=shiny&logoColor=white)](https://shiny.rstudio.com/)

**NeuroVigil** (formerly NeuroPredict AI) is a trilingual (French, English, Arabic) Clinical Decision Support System (CDSS) designed to predict the risk of Mild Cognitive Impairment (MCI) converting to Alzheimer's Disease (AD) within a 24-month window. 

Developed for the **Centre Mohammed VI de la Recherche et de l'Innovation (CM6RI)** and the **Université Mohammed VI des Sciences de la Santé (UM6SS)**, this tool bridges the gap between advanced machine learning and frontline neurological practice.

 **[Launch the Live Web Application](https://neuropredictai.shinyapps.io/test_funcml/)**

---

##  Key Features

- **🩺 Dynamic KNN Imputation:** Handles missing clinical data in real-time during prediction without discarding patient records, preserving statistical power and multivariate integrity.
- **🔍 Explainable AI (XAI):** Integrates SHAP, Partial Dependence Plots (PDP), and Accumulated Local Effects (ALE) to provide transparent, interpretable risk scores for clinicians.
- **️ Algorithmic Fairness:** Rigorously audited across sex, APOE-ε4 status, and education levels to ensure equitable predictions and mitigate algorithmic bias.
- ** Trilingual & RTL Support:** Fully localized interface for French, English, and Arabic, including native Right-to-Left (RTL) layout support.
- **📊 Batch & Single Processing:** Supports both individual patient analysis and bulk Excel-based cohort screening for epidemiological surveillance.
- ** Automated PDF Reporting:** Generates comprehensive, branded clinical reports featuring calibrated risk gauges, DCA visualizations, and explicit "measured vs. estimated" data transparency.

---

##  Architecture & Methodology

NeuroVigil is built on a robust, end-to-end machine learning pipeline validated on the Alzheimer’s Disease Neuroimaging Initiative (ADNI) cohort.

1. **Data Preprocessing:** Raw clinical data is processed using K-Nearest Neighbors (KNN, *k*=5) imputation to handle missingness (ranging from 2% to 42% in raw biomarkers).
2. **Model Selection & Tuning:** An Extreme Gradient Boosting (XGBoost) model is selected via 5-fold cross-validation benchmarking and tuned via random search.
3. **Threshold Optimization:** The decision threshold is optimized using **Youden’s Index** ($J = Sensitivity + Specificity - 1$) to balance clinical sensitivity and specificity.
4. **Validation:** Evaluated using Decision Curve Analysis (DCA) for clinical utility and stratified subgroup analysis for fairness.
5. **Deployment:** The serialized model is deployed via an R Shiny web application with dynamic inference capabilities.

<img width="6682" height="7232" alt="System_Architecture" src="https://github.com/user-attachments/assets/554bce6a-5f53-42f3-8bbd-7210ed0b96bd" />

---

## ️ Tech Stack

- **Language:** R
- **Framework:** Shiny
- **Machine Learning:** XGBoost, `funcml`, `VIM` (for KNN imputation)
- **Data Handling:** `tidyverse`, `readxl`, `writexl`
- **Reporting & UI:** `pagedown`, `base64enc`, `DT` (DataTables)

---

##  How to Run Locally

To run this application on your local machine, follow these steps:

### 1. Prerequisites
Ensure you have R (>= 4.0.0) and RStudio installed.

### 2. Clone the Repository
```bash
git clone https://github.com/YOUR_USERNAME/NeuroVigil.git
cd NeuroVigil
```

### 3. Install Dependencies
Open R or RStudio and run the following command to install the required packages:
```r
install.packages(c(
  "shiny", "tidyverse", "funcml", "VIM", "DT", 
  "pagedown", "readxl", "writexl", "base64enc"
))
```

### 4. Launch the App
In your R console, run:
```r
shiny::runApp()
```
The application will open in your default web browser at `http://127.0.0.1:3838`.

---

## 📂 Project Structure

```text
NeuroVigil/
└── ModelFuncml.Rproj      #Ml code
├── app.R                  # Main Shiny application logic and UI
├── README.md
├── outputs/                           
│   ├── hyperparameter_grid_full.csv   
│   ├── temporal_robustness_table.csv  
│   └── 03_Metriques_Fairness_EN.csv          
└── data/
    └── processed/
        └── Tabular data/
            └── FINAL_DATASET.csv
            └── train_KNN_imputed.csv
            └── adni_knn_imputed.csv
            └── modele_final_xgboost.rds    # Serialized XGBoost model
            └──data_train_reference.rds     # Reference cohort for KNN
            └── metriques_modele.rds        # Model performance metrics
```

---

## Data Availability & Ethics

- **Training Data:** This model was trained on the **Alzheimer’s Disease Neuroimaging Initiative (ADNI)** dataset. ADNI data is subject to a Data Use Agreement and is **not** included in this repository. Researchers can request access at [adni.loni.usc.edu](https://adni.loni.usc.edu/).
- **Model Artifacts:** The repository contains only the serialized model weights (`.rds` files) and a synthetic reference cohort structure, ensuring no raw patient data is exposed.
- **Ethical Auditing:** The model includes built-in fairness metrics to ensure equitable performance across diverse demographic subgroups.




