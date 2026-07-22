library(shiny)
library(tidyverse)
library(funcml)
library(VIM)
library(DT)
library(base64enc)
library(pagedown)
library(readxl)
library(writexl)

logo_path <- "www/logo_cm6ri.png"
if (file.exists(logo_path)) {
  addResourcePath("app_logo", dirname(logo_path))
  LOGO_URL <- file.path("app_logo", basename(logo_path))
  FAVICON_URL <- LOGO_URL
} else {
  warning("Logo introuvable : ", logo_path)
  LOGO_URL <- ""
  FAVICON_URL <- ""
}

##Color palette
COL_BG_LIGHT     <- "#F8F9FA"
COL_BG_CARD      <- "#FFFFFF"
COL_BG_CARD_SOFT <- "#F1F3F5"
COL_CM6RI_RED    <- "#C8102E"
COL_CM6RI_GREEN  <- "#00A651"
COL_TEXTE        <- "#2D3436"
COL_TEXTE_ATTEN  <- "#636E72"
COL_RISQUE_HAUT  <- "#D63031"
COL_RISQUE_MOD   <- "#F39C12"
COL_RISQUE_FAIBLE<- "#00B894"


# MODEL LOADING & PATHS
project_root <- getwd()
path_funcml_ready <- file.path(project_root, "data/processed/Tabular data/funcml_ready")
fichier_historique <- file.path(path_funcml_ready, "historique_predictions.csv")

modele    <- readRDS(file.path(path_funcml_ready, "modele_final_xgboost.rds"))
data_ref  <- readRDS(file.path(path_funcml_ready, "data_train_reference.rds"))
metriques <- readRDS(file.path(path_funcml_ready, "metriques_modele.rds"))
SEUIL_OPTIMAL <- metriques$seuil_optimal

if (!file.exists(fichier_historique)) {
  write_csv(data.frame(
    Date = character(), ID_Patient = character(), Age = numeric(),
    Sexe = character(), APOE = character(), MMSE = numeric(),
    CDRSB = numeric(), MoCA = numeric(), Probabilite = numeric(),
    Decision = character(), stringsAsFactors = FALSE
  ), fichier_historique)
}


# TRILINGUAL TRANSLATIONS (FR / EN / AR)
TR <- list(
  fr = list(
    app_title = "NeuroVigil", app_subtitle = "Centre Mohammed VI de la Recherche et de l'Innovation",
    nav_home = "Accueil", nav_prediction = "Prédiction", nav_import = "Import Excel", nav_history = "Historique",
    hero_title = "Bienvenue sur NeuroVigil",
    hero_subtitle = "Plateforme d'intelligence artificielle pour la prédiction du risque de conversion MCI vers la maladie d'Alzheimer à 24 mois.",
    stat_auc = "Performance (AUC)", stat_ece = "Calibration (ECE)", stat_seuil = "Seuil Optimal", stat_patients = "Patients Validés",
    about_title = "À propos", about_objectif = "Objectif", about_objectif_txt = "Identifier les patients MCI à haut risque de conversion vers Alzheimer dans les 24 mois.",
    about_methodo = "Méthodologie",
    about_methodo_1 = "Modèle XGBoost (cohorte ADNI)", about_methodo_2 = "Imputation KNN dynamique", about_methodo_3 = "Seuil optimisé (Youden)",
    about_warning_title = "Avertissement", about_warning_txt = "Aide à la décision uniquement. La décision finale revient au médecin traitant.",
    form_title = "Données du patient", form_hint = "Remplissez les champs disponibles. Les valeurs manquantes seront estimées automatiquement.",
    field_id = "ID Patient", field_age = "Âge (ans)", field_sexe = "Sexe", field_homme = "Homme", field_femme = "Femme",
    field_education = "Années d'études", section_cognitif = "Tests cognitifs",
    field_mmse = "MMSE (0-30)", field_cdrsb = "CDR-SB (0-18)", field_moca = "MoCA (0-30)",
    field_total13 = "ADAS-Cog TOTAL13", field_faq = "FAQ (autonomie)",
    field_mmse_full = "Mini-Mental State Examination — test cognitif global",
    field_cdrsb_full = "Clinical Dementia Rating, Sum of Boxes — sévérité clinique",
    field_moca_full = "Montreal Cognitive Assessment — dépistage du déclin léger",
    field_total13_full = "Alzheimer's Disease Assessment Scale-Cognitive (13 items)",
    field_faq_full = "Functional Activities Questionnaire — autonomie quotidienne",
    section_genetique = "Génétique APOE", field_genotype = "Génotype APOE", field_non_type = "Non typé",
    field_genotype_full = "Apolipoprotéine E — principal facteur de risque génétique",
    section_biomarqueurs = "Biomarqueurs LCR", field_optionnel = "Optionnels",
    field_tau = "TAU (pg/mL)", field_ptau = "PTAU (pg/mL)", field_abeta = "ABETA42 (pg/mL)",
    field_tau_full = "Protéine Tau totale — marqueur de dégât neuronal",
    field_ptau_full = "Tau phosphorylée — marqueur spécifique Alzheimer",
    field_abeta_full = "Peptide amyloïde bêta 42 — bas = plaques amyloïdes",
    btn_predict = "LANCER L'ANALYSE", result_title = "Résultat de l'analyse", btn_download_report = "Télécharger le rapport PDF",
    result_patient = "Patient", result_age = "Âge", result_sexe = "Sexe",
    result_risque_titre = "Risque de conversion à 24 mois", result_proba = "de probabilité", result_seuil = "Seuil",
    niveau_eleve = "ÉLEVÉ", niveau_modere = "MODÉRÉ", niveau_faible = "FAIBLE",
    reco_title = "Recommandations cliniques",
    reco_eleve_1 = "Suivi neurologique rapproché (tous les 6 mois)", reco_eleve_2 = "Évaluation pour thérapies anti-amyloïdes", reco_eleve_3 = "Anticipation médico-sociale et soutien aux aidants",
    reco_mod_1 = "Suivi clinique standard (tous les 12 mois)", reco_mod_2 = "Réévaluation des tests cognitifs et biomarqueurs", reco_mod_3 = "Encouragement des mesures préventives",
    reco_faible_1 = "Maintien du suivi de routine", reco_faible_2 = "Encouragement des activités de prévention cognitive",
    disclaimer = "Aide à la décision. La décision finale revient au clinicien.",
    import_step1 = "1. Télécharger le modèle", import_step1_txt = "Utilisez ce fichier Excel comme modèle.",
    btn_template = "Télécharger le modèle Excel",
    import_step2 = "2. Importer vos données", import_choose = "Choisir un fichier Excel (.xlsx ou .csv)",
    btn_batch = "LANCER L'ANALYSE EN LOT", import_step3 = "3. Résultats de l'analyse en lot",
    batch_success = "patients analysés avec succès", batch_haut = "HAUT RISQUE", batch_faible = "RISQUE FAIBLE",
    btn_download_batch = "Télécharger les résultats (Excel)",
    history_title = "Historique des prédictions", history_subtitle = "Suivi longitudinal des patients analysés",
    history_patients = "Patients analysés", btn_refresh = "Actualiser", btn_export_csv = "Exporter CSV",
    err_id_vide = "L'ID du patient ne peut pas être vide !", err_id_existe = "ID déjà utilisé !",
    err_id_existe_txt = "Ce patient existe déjà.", msg_pred_ok = "Prédiction enregistrée !", msg_pred_ok_txt = "ajouté.",
    err_fichier_vide = "Le fichier est vide !", err_colonnes = "Colonnes manquantes :",
    msg_analyse_cours = "Analyse en cours...", msg_analyse_ok = "Analyse terminée avec succès !", msg_patients_traites = "patients traités.",
    err_lecture = "Erreur de lecture :", footer_copyright = "© 2026 CM6RI - Université Mohammed VI des Sciences de la Santé",
    pdf_title = "Rapport de Prédiction Alzheimer", pdf_subtitle = "NeuroVigil — Aide à la décision",
    pdf_info_patient = "Informations du patient", pdf_id = "ID Patient", pdf_age = "Âge", pdf_sexe = "Sexe",
    pdf_education = "Années d'études", pdf_mmse = "MMSE", pdf_cdrsb = "CDR-SB", pdf_moca = "MoCA", pdf_apoe = "Génotype APOE",
    pdf_resultat = "Résultat", pdf_proba_txt = "Probabilité de conversion à 24 mois",
    pdf_warning = "Aide à la décision uniquement. La décision finale reste du ressort du médecin.",
    pdf_footer_txt = "Modèle validé sur"
  ),
  en = list(
    app_title = "NeuroVigil", app_subtitle = "Mohammed VI Center for Research and Innovation",
    nav_home = "Home", nav_prediction = "Prediction", nav_import = "Excel Import", nav_history = "History",
    hero_title = "Welcome to NeuroVigil",
    hero_subtitle = "AI platform for predicting the risk of MCI-to-Alzheimer's conversion within 24 months.",
    stat_auc = "Performance (AUC)", stat_ece = "Calibration (ECE)", stat_seuil = "Optimal Threshold", stat_patients = "Validated Patients",
    about_title = "About", about_objectif = "Objective", about_objectif_txt = "Identify MCI patients at high risk of converting to Alzheimer's within 24 months.",
    about_methodo = "Methodology",
    about_methodo_1 = "XGBoost model (ADNI cohort)", about_methodo_2 = "Dynamic KNN imputation", about_methodo_3 = "Optimized threshold (Youden)",
    about_warning_title = "Disclaimer", about_warning_txt = "Decision support only. Final decision remains with the treating physician.",
    form_title = "Patient Data", form_hint = "Fill available fields. Missing values will be estimated automatically.",
    field_id = "Patient ID", field_age = "Age (years)", field_sexe = "Sex", field_homme = "Male", field_femme = "Female",
    field_education = "Years of education", section_cognitif = "Cognitive tests",
    field_mmse = "MMSE (0-30)", field_cdrsb = "CDR-SB (0-18)", field_moca = "MoCA (0-30)",
    field_total13 = "ADAS-Cog TOTAL13", field_faq = "FAQ (functional autonomy)",
    field_mmse_full = "Mini-Mental State Examination — global cognitive test",
    field_cdrsb_full = "Clinical Dementia Rating, Sum of Boxes — clinical severity",
    field_moca_full = "Montreal Cognitive Assessment — mild cognitive impairment screening",
    field_total13_full = "Alzheimer's Disease Assessment Scale-Cognitive (13 items)",
    field_faq_full = "Functional Activities Questionnaire — daily autonomy",
    section_genetique = "APOE Genetics", field_genotype = "APOE genotype", field_non_type = "Not typed",
    field_genotype_full = "Apolipoprotein E — leading known genetic risk factor",
    section_biomarqueurs = "CSF Biomarkers", field_optionnel = "Optional",
    field_tau = "TAU (pg/mL)", field_ptau = "PTAU (pg/mL)", field_abeta = "ABETA42 (pg/mL)",
    field_tau_full = "Total Tau protein — marker of neuronal damage",
    field_ptau_full = "Phosphorylated Tau — specific Alzheimer's marker",
    field_abeta_full = "Amyloid beta 42 peptide — low = amyloid plaques",
    btn_predict = "RUN ANALYSIS", result_title = "Analysis Result", btn_download_report = "Download PDF Report",
    result_patient = "Patient", result_age = "Age", result_sexe = "Sex",
    result_risque_titre = "24-month conversion risk", result_proba = "probability", result_seuil = "Threshold",
    niveau_eleve = "HIGH", niveau_modere = "MODERATE", niveau_faible = "LOW",
    reco_title = "Clinical Recommendations",
    reco_eleve_1 = "Close neurological follow-up (every 6 months)", reco_eleve_2 = "Evaluation for anti-amyloid therapies", reco_eleve_3 = "Medico-social planning and caregiver support",
    reco_mod_1 = "Standard clinical follow-up (every 12 months)", reco_mod_2 = "Re-assessment of cognitive tests and biomarkers", reco_mod_3 = "Encourage preventive measures",
    reco_faible_1 = "Maintain routine follow-up", reco_faible_2 = "Encourage cognitive prevention activities",
    disclaimer = "Decision support only. Final decision remains with the clinician.",
    import_step1 = "1. Download Template", import_step1_txt = "Use this Excel file as a template.",
    btn_template = "Download Excel Template",
    import_step2 = "2. Import Your Data", import_choose = "Choose an Excel file (.xlsx or .csv)",
    btn_batch = "RUN BATCH ANALYSIS", import_step3 = "3. Batch Analysis Results",
    batch_success = "patients successfully analyzed", batch_haut = "HIGH RISK", batch_faible = "LOW RISK",
    btn_download_batch = "Download Results (Excel)",
    history_title = "Prediction History", history_subtitle = "Longitudinal follow-up of analyzed patients",
    history_patients = "Analyzed patients", btn_refresh = "Refresh", btn_export_csv = "Export CSV",
    err_id_vide = "Patient ID cannot be empty!", err_id_existe = "ID already used!",
    err_id_existe_txt = "This patient already exists.", msg_pred_ok = "Prediction saved!", msg_pred_ok_txt = "added.",
    err_fichier_vide = "The file is empty!", err_colonnes = "Missing columns:",
    msg_analyse_cours = "Analysis in progress...", msg_analyse_ok = "Analysis completed successfully!", msg_patients_traites = "patients processed.",
    err_lecture = "Reading error:", footer_copyright = "© 2026 CM6RI - Mohammed VI University of Health Sciences",
    pdf_title = "Alzheimer's Prediction Report", pdf_subtitle = "NeuroVigil — Clinical Decision Support",
    pdf_info_patient = "Patient Information", pdf_id = "Patient ID", pdf_age = "Age", pdf_sexe = "Sex",
    pdf_education = "Years of education", pdf_mmse = "MMSE", pdf_cdrsb = "CDR-SB", pdf_moca = "MoCA", pdf_apoe = "APOE genotype",
    pdf_resultat = "Result", pdf_proba_txt = "24-month conversion probability",
    pdf_warning = "Decision support only. Final clinical decision remains with the physician.",
    pdf_footer_txt = "Model validated on"
  ),
  ar = list(
    app_title = "نوروفيجيل", app_subtitle = "مركز محمد السادس للبحث والابتكار",
    nav_home = "الرئيسية", nav_prediction = "التنبؤ", nav_import = "استيراد Excel", nav_history = "السجل",
    hero_title = "مرحباً بكم في نوروفيجيل",
    hero_subtitle = "منصة ذكاء اصطناعي للتنبؤ بخطر تحول الضعف الإدراكي المعتدل إلى مرض الزهايمر خلال 24 شهراً.",
    stat_auc = "الأداء (AUC)", stat_ece = "المعايرة (ECE)", stat_seuil = "العتبة المثلى", stat_patients = "المرضى المعتمدون",
    about_title = "حول التطبيق", about_objectif = "الهدف", about_objectif_txt = "تحديد مرضى الضعف الإدراكي المعرضين لخطر عالٍ للتحول إلى الزهايمر خلال 24 شهراً.",
    about_methodo = "المنهجية",
    about_methodo_1 = "نموذج XGBoost (دراسة ADNI)", about_methodo_2 = "استكمال البيانات الديناميكي KNN", about_methodo_3 = "عتبة محسنة (يودن)",
    about_warning_title = "تنويه", about_warning_txt = "أداة دعم قرار فقط. القرار النهائي يبقى من مسؤولية الطبيب المعالج.",
    form_title = "بيانات المريض", form_hint = "املأ الحقول المتاحة. سيتم تقدير القيم المفقودة تلقائياً.",
    field_id = "معرف المريض", field_age = "العمر (سنوات)", field_sexe = "الجنس", field_homme = "ذكر", field_femme = "أنثى",
    field_education = "سنوات الدراسة", section_cognitif = "الاختبارات المعرفية",
    field_mmse = "MMSE (0-30)", field_cdrsb = "CDR-SB (0-18)", field_moca = "MoCA (0-30)",
    field_total13 = "ADAS-Cog TOTAL13", field_faq = "FAQ (الاستقلالية)",
    field_mmse_full = "الفحص المعرفي المصغر — اختبار معرفي شامل",
    field_cdrsb_full = "التقييم السريري للخرف — شدة الخرف السريرية",
    field_moca_full = "التقييم المعرفي لمونتريال — فحص التدهور المعرفي الخفيف",
    field_total13_full = "مقياس تقييم مرض الزهايمر المعرفي (13 عنصراً)",
    field_faq_full = "استبيان الأنشطة الوظيفية — الاستقلالية في الأنشطة اليومية",
    section_genetique = "علم الوراثة APOE", field_genotype = "النمط الجيني APOE", field_non_type = "غير مصنف",
    field_genotype_full = "أبوليبوبروتين E — عامل الخطر الجيني الرئيسي المعروف",
    section_biomarqueurs = "العلامات الحيوية للسائل الدماغي", field_optionnel = "اختياري",
    field_tau = "TAU (pg/mL)", field_ptau = "PTAU (pg/mL)", field_abeta = "ABETA42 (pg/mL)",
    field_tau_full = "بروتين تاو الكلي — علامة على تلف الخلايا العصبية",
    field_ptau_full = "تاو المفسفر — علامة أكثر تحديداً لمرض الزهايمر",
    field_abeta_full = "ببتيد أميلويد بيتا 42 — انخفاضه = وجود لويحات أميلويد",
    btn_predict = "تشغيل التحليل", result_title = "نتيجة التحليل", btn_download_report = "تحميل تقرير PDF",
    result_patient = "المريض", result_age = "العمر", result_sexe = "الجنس",
    result_risque_titre = "خطر التحول خلال 24 شهراً", result_proba = "احتمالية", result_seuil = "العتبة",
    niveau_eleve = "عالي", niveau_modere = "معتدل", niveau_faible = "منخفض",
    reco_title = "التوصيات السريرية",
    reco_eleve_1 = "متابعة عصبية وثيقة (كل 6 أشهر)", reco_eleve_2 = "تقييم للعلاجات المضادة للأميلويد", reco_eleve_3 = "التخطيط الطبي-الاجتماعي ودعم مقدمي الرعاية",
    reco_mod_1 = "متابعة سريرية قياسية (كل 12 شهراً)", reco_mod_2 = "إعادة تقييم الاختبارات والعلامات الحيوية", reco_mod_3 = "تشجيع التدابير الوقائية",
    reco_faible_1 = "الحفاظ على المتابعة الروتينية", reco_faible_2 = "تشجيع أنشطة الوقاية المعرفية",
    disclaimer = "أداة دعم قرار فقط. القرار النهائي يبقى من مسؤولية الطبيب.",
    import_step1 = "1. تحميل النموذج", import_step1_txt = "استخدم ملف Excel هذا كنموذج.",
    btn_template = "تحميل نموذج Excel",
    import_step2 = "2. استيراد بياناتك", import_choose = "اختر ملف Excel (.xlsx أو .csv)",
    btn_batch = "تشغيل التحليل الجماعي", import_step3 = "3. نتائج التحليل الجماعي",
    batch_success = "مرضى تم تحليلهم بنجاح", batch_haut = "خطر عالي", batch_faible = "خطر منخفض",
    btn_download_batch = "تحميل النتائج (Excel)",
    history_title = "سجل التنبؤات", history_subtitle = "المتابعة الطولية للمرضى المحللين",
    history_patients = "المرضى المحللون", btn_refresh = "تحديث", btn_export_csv = "تصدير CSV",
    err_id_vide = "لا يمكن أن يكون معرف المريض فارغاً!", err_id_existe = "المعرف مستخدم بالفعل!",
    err_id_existe_txt = "هذا المريض موجود بالفعل.", msg_pred_ok = "تم حفظ التنبؤ!", msg_pred_ok_txt = "تمت الإضافة.",
    err_fichier_vide = "الملف فارغ!", err_colonnes = "أعمدة مفقودة:",
    msg_analyse_cours = "جاري التحليل...", msg_analyse_ok = "تم التحليل بنجاح!", msg_patients_traites = "مرضى تمت معالجتهم.",
    err_lecture = "خطأ في القراءة:", footer_copyright = "© 2026 مركز محمد السادس - جامعة محمد السادس للعلوم الصحية",
    pdf_title = "تقرير التنبؤ بالزهايمر", pdf_subtitle = "نيوروفيجيل — أداة دعم القرار السريري",
    pdf_info_patient = "معلومات المريض", pdf_id = "معرف المريض", pdf_age = "العمر", pdf_sexe = "الجنس",
    pdf_education = "سنوات الدراسة", pdf_mmse = "MMSE", pdf_cdrsb = "CDR-SB", pdf_moca = "MoCA", pdf_apoe = "النمط الجيني APOE",
    pdf_resultat = "النتيجة", pdf_proba_txt = "احتمالية التحول خلال 24 شهراً",
    pdf_warning = "أداة دعم قرار فقط. القرار السريري النهائي يبقى من مسؤولية الطبيب المعالج.",
    pdf_footer_txt = "تم التحقق من صحة النموذج على"
  )
)


# PREDICTION FUNCTION WITH DYNAMIC KNN IMPUTATION
predire_patient <- function(patient_data, modele, data_ref, seuil) {
  patient_transforme <- patient_data %>%
    mutate(
      PTGENDER = factor(PTGENDER, levels = c(1, 2), labels = c("Homme", "Femme")),
      n_e4 = if ("GENOTYPE" %in% names(.)) str_count(as.character(GENOTYPE), "4") else NA_integer_
    )
  if ("GENOTYPE" %in% names(patient_transforme)) patient_transforme$GENOTYPE <- NULL
  
  manquantes <- setdiff(names(data_ref), names(patient_transforme))
  for (col in manquantes) patient_transforme[[col]] <- NA
  patient_transforme <- patient_transforme %>% select(all_of(names(data_ref)))
  
  data_combine <- bind_rows(data_ref, patient_transforme)
  data_impute <- VIM::kNN(data_combine, k = 5, imp_var = FALSE)
  patient_impute <- data_impute[nrow(data_impute), ]
  
  proba <- predict(modele, patient_impute, type = "prob")[, "Yes"]
  decision <- ifelse(proba >= seuil, "Haut risque", "Risque faible")
  list(proba = round(proba, 3), decision = decision)
}


# SVG GAUGE COMPONENT
creer_jauge_svg <- function(pourcentage, couleur, taille = 200, libelle = "Risk") {
  rayon <- 80; circonference <- 2 * pi * rayon
  offset <- circonference - (pourcentage / 100) * circonference
  sprintf('
  <svg width="%d" height="%d" viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg" style="direction: ltr;">
    <defs>
      <linearGradient id="grad_%s" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
        <stop offset="0%%" style="stop-color:%s;stop-opacity:1" />
        <stop offset="100%%" style="stop-color:%s;stop-opacity:0.6" />
      </linearGradient>
      <filter id="glow_%s"><feGaussianBlur stdDeviation="4" result="blur"/><feMerge><feMergeNode in="blur"/><feMergeNode in="SourceGraphic"/></feMerge></filter>
    </defs>
    <circle cx="100" cy="100" r="%d" fill="none" stroke="#E9ECEF" stroke-width="14"/>
    <circle cx="100" cy="100" r="%d" fill="none"
            stroke="url(#grad_%s)" stroke-width="14"
            stroke-dasharray="%.2f" stroke-dashoffset="%.2f"
            stroke-linecap="round" transform="rotate(-90 100 100)"
            filter="url(#glow_%s)"
            style="transition: stroke-dashoffset 1.5s ease-in-out;"/>
    <text x="100" y="94" text-anchor="middle" font-size="36" font-weight="bold" fill="%s" style="direction: ltr;">%.1f%%</text>
    <text x="100" y="124" text-anchor="middle" font-size="13" fill="#636E72" style="direction: ltr;">%s</text>
  </svg>', taille, taille, couleur, couleur, couleur, couleur, rayon, rayon, couleur,
          circonference, offset, couleur, couleur, pourcentage, libelle)
}


# CHAMP AVEC INFO-BULLE COMPACTE
champ_num_info <- function(input_id, label, tooltip, ...) {
  div(
    tags$label(
      `for` = input_id,
      style = "font-weight: 600; display: flex; align-items: center; gap: 6px; margin-bottom: 6px; font-size: 15px;",
      label,
      tags$span(title = tooltip, style = "cursor: help; color: #ADB5BD; font-size: 14px; border: 1px solid #ADB5BD; border-radius: 50%; width: 16px; height: 16px; display: inline-flex; align-items: center; justify-content: center; line-height: 1;", "i")
    ),
    numericInput(input_id, label = NULL, ...)
  )
}


# CSS 

css_global <- paste0("
@import url('https://fonts.googleapis.com/css2?family=Cairo:wght@400;600;700&family=Segoe+UI:wght@400;600;700&display=swap');

@keyframes fadeIn { from { opacity: 0; transform: translateY(8px); } to { opacity: 1; transform: translateY(0); } }
@keyframes slideIn { from { opacity: 0; transform: translateX(-16px); } to { opacity: 1; transform: translateX(0); } }

body { font-family: 'Segoe UI', 'Cairo', sans-serif; background: ", COL_BG_LIGHT, "; color: ", COL_TEXTE, "; min-height: 100vh; font-size: 16px; }
body.rtl { font-family: 'Cairo', 'Segoe UI', sans-serif; }

/* HEADER */
.navbar {
  background: ", COL_CM6RI_RED, " !important;
  box-shadow: 0 2px 12px rgba(0,0,0,0.12) !important;
  border-bottom: none !important;
  padding-top: 10px !important;
  padding-bottom: 10px !important;
}
.navbar-brand, .navbar-nav > li > a { color: #FFFFFF !important; font-weight: 600; transition: all 0.25s ease; font-size: 16px; }
.navbar-nav > li > a:hover { background: rgba(255,255,255,0.15) !important; color: #FFFFFF !important; }
.navbar-nav > li.active > a { background: rgba(255,255,255,0.22) !important; color: #FFFFFF !important; border-radius: 8px; }

.logo-navbar { height: 48px; width: auto; margin-right: 14px; background: white; padding: 4px; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.15); }
.logo-hero { height: 68px; width: auto; margin-bottom: 12px; background: white; padding: 6px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.15); }

.card-premium { background: ", COL_BG_CARD, "; border-radius: 14px; padding: 24px; box-shadow: 0 3px 16px rgba(0,0,0,0.05); border-top: 4px solid ", COL_CM6RI_RED, "; transition: all 0.25s ease; animation: fadeIn 0.5s ease-out; }
.card-premium:hover { transform: translateY(-3px); box-shadow: 0 8px 26px rgba(200,16,46,0.1); }

.sidebar-panel { background: ", COL_BG_CARD, "; border-radius: 14px; padding: 22px 24px; box-shadow: 0 3px 16px rgba(0,0,0,0.05); border-left: 4px solid ", COL_CM6RI_GREEN, "; }
.rtl .sidebar-panel { border-left: none !important; border-right: 4px solid ", COL_CM6RI_GREEN, " !important; }

.btn-premium { background: linear-gradient(135deg, ", COL_CM6RI_RED, " 0%, ", COL_CM6RI_GREEN, " 100%); color: white !important; border: none !important; border-radius: 10px !important; padding: 12px 24px !important; font-weight: 600 !important; font-size: 15px !important; box-shadow: 0 3px 12px rgba(200,16,46,0.28); transition: all 0.25s ease; }
.btn-premium:hover { transform: translateY(-2px); box-shadow: 0 5px 18px rgba(200,16,46,0.38); }
.btn-success-premium { background: linear-gradient(135deg, #00B894 0%, ", COL_CM6RI_GREEN, " 100%); color: white !important; border: none !important; border-radius: 10px !important; padding: 12px 24px !important; font-weight: 600 !important; font-size: 15px !important; }
.btn-lang { background: rgba(255,255,255,0.2) !important; color: white !important; border: 1px solid rgba(255,255,255,0.4) !important; border-radius: 20px !important; padding: 6px 16px !important; font-weight: 600 !important; font-size: 14px !important; }
.btn-lang:hover { background: rgba(255,255,255,0.32) !important; color: white !important; }

.titre-section { color: ", COL_CM6RI_RED, "; font-weight: 700; letter-spacing: -0.3px; font-size: 22px; }
.sous-titre { color: ", COL_CM6RI_RED, "; font-weight: 600; border-bottom: 2px solid rgba(200,16,46,0.2); padding-bottom: 6px; display: inline-block; margin-bottom: 12px; font-size: 16px; }

/* CARTES  */
.stat-card { 
  background: ", COL_BG_CARD, "; 
  border-radius: 12px; 
  padding: 20px; 
  box-shadow: 0 4px 12px rgba(0,0,0,0.06); 
  border-top: 4px solid; 
  animation: slideIn 0.4s ease-out;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  min-height: 140px; /* Force un aspect carré/boxy */
  transition: transform 0.2s ease, box-shadow 0.2s ease;
}
.stat-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 8px 20px rgba(0,0,0,0.1);
}
.stat-value { font-size: 38px; font-weight: 700; margin: 8px 0; line-height: 1; color: ", COL_TEXTE, " !important; }
.stat-label { font-size: 13px; color: ", COL_TEXTE_ATTEN, "; text-transform: uppercase; letter-spacing: 1px; font-weight: 600; text-align: center; }

.form-control, .selectize-input { border-radius: 8px !important; border: 1px solid #DEE2E6 !important; background: #FFFFFF !important; color: ", COL_TEXTE, " !important; padding: 8px 12px !important; font-size: 15px !important; height: auto !important; }
.form-control:focus { border-color: ", COL_CM6RI_RED, " !important; box-shadow: 0 0 0 3px rgba(200,16,46,0.12) !important; }
.selectize-dropdown { background: #FFFFFF !important; color: ", COL_TEXTE, " !important; border: 1px solid #DEE2E6 !important; }
.selectize-dropdown-content .option { color: ", COL_TEXTE, " !important; }
label { color: ", COL_TEXTE, " !important; font-weight: 600; font-size: 15px; }
.form-group { margin-bottom: 14px !important; }

.alert-info-custom { background: rgba(0,166,81,0.08); border-left: 4px solid ", COL_CM6RI_GREEN, "; border-radius: 8px; padding: 14px 16px; color: ", COL_TEXTE, " !important; font-size: 15px; }
.alert-warning-custom { background: rgba(243,156,18,0.08); border-left: 4px solid ", COL_RISQUE_MOD, "; border-radius: 8px; padding: 14px 16px; color: ", COL_TEXTE, " !important; }
.rtl .alert-info-custom, .rtl .alert-warning-custom { border-left: none !important; border-right: 4px solid !important; }

.result-box { border-radius: 14px; padding: 28px; animation: fadeIn 0.7s ease-out; border: 1px solid rgba(0,0,0,0.05); background: ", COL_BG_CARD_SOFT, "; }

.dataTable tbody tr { background: #FFFFFF !important; color: ", COL_TEXTE, " !important; font-size: 15px; }
.dataTable tbody tr:hover { background-color: rgba(200,16,46,0.05) !important; }
table.dataTable thead th { background: ", COL_BG_CARD_SOFT, " !important; color: ", COL_CM6RI_RED, " !important; border-bottom: 2px solid rgba(200,16,46,0.2) !important; font-size: 15px; }
.dataTables_wrapper input, .dataTables_wrapper select { background: #FFFFFF !important; color: ", COL_TEXTE, " !important; border: 1px solid #DEE2E6 !important; }
.dataTables_wrapper .dataTables_paginate .paginate_button.current { background: ", COL_CM6RI_RED, " !important; color: #FFFFFF !important; border-radius: 6px !important; border: none !important; }

.hero-banner { background: linear-gradient(135deg, ", COL_CM6RI_RED, " 0%, ", COL_CM6RI_GREEN, " 100%); color: white; padding: 36px 40px; border-radius: 18px; box-shadow: 0 4px 20px rgba(0,0,0,0.1); margin-bottom: 26px; position: relative; overflow: hidden; }
.file-upload-box { border: 2px dashed ", COL_CM6RI_GREEN, "; border-radius: 12px; padding: 28px; text-align: center; background: rgba(0,166,81,0.03); transition: all 0.25s ease; }
.file-upload-box:hover { border-color: ", COL_CM6RI_RED, "; background: rgba(200,16,46,0.06); }
hr { border-top: 1px solid rgba(0,0,0,0.07); margin: 18px 0; }

.shiny-notification { background-color: #FFFFFF !important; color: ", COL_TEXTE, " !important; border-radius: 10px; border: 1px solid rgba(200,16,46,0.3); box-shadow: 0 4px 15px rgba(0,0,0,0.1); font-size: 15px; }
.shiny-notification .shiny-notification-close { color: ", COL_TEXTE_ATTEN, " !important; }

.section-icon { font-size: 18px; margin-right: 8px; }
.rtl .section-icon { margin-right: 0; margin-left: 8px; }
")


# UI

ui <- tagList(
  tags$head(
    tags$style(HTML(css_global)),
    if (FAVICON_URL != "") tags$link(rel = "icon", type = "image/png", href = FAVICON_URL),
    tags$title("NeuroVigil"),
    uiOutput("rtl_style")
  ),
  div(class = "navbar navbar-default navbar-static-top",
      div(class = "container-fluid", style = "display: flex; justify-content: space-between; align-items: center;",
          div(style = "display: flex; align-items: center;",
              if (LOGO_URL != "") tags$img(src = LOGO_URL, class = "logo-navbar"),
              div(
                h4(style = "margin: 0; font-weight: 700; color: white; font-size: 24px;", "NeuroVigil"),
                p(style = "margin: 0; font-size: 13px; color: rgba(255,255,255,0.85);", "CM6RI • UM6SS • Outil d'aide à la décision • Alzheimer")
              )
          ),
          div(style = "display: flex; align-items: center; gap: 10px;",
              uiOutput("nav_links_ui"),
              actionButton("toggle_lang", "🇬🇧 English", class = "btn-lang")
          )
      )
  ),
  uiOutput("page_content")
)

# SERVER

server <- function(input, output, session) {
  
  lang <- reactiveVal("fr")
  page <- reactiveVal("home")
  
  observeEvent(input$toggle_lang, {
    current <- lang()
    if (current == "fr") {
      lang("en")
      updateActionButton(session, "toggle_lang", label = "🇸🇦 العربية")
    } else if (current == "en") {
      lang("ar")
      updateActionButton(session, "toggle_lang", label = "🇫🇷 Français")
    } else {
      lang("fr")
      updateActionButton(session, "toggle_lang", label = "🇬🇧 English")
    }
  })
  
  observeEvent(input$nav_home, page("home"))
  observeEvent(input$nav_prediction, page("prediction"))
  observeEvent(input$nav_import, page("import"))
  observeEvent(input$nav_history, page("history"))
  
  t <- function(key) TR[[lang()]][[key]]
  
  output$rtl_style <- renderUI({
    is_rtl <- lang() == "ar"
    dir <- ifelse(is_rtl, "rtl", "ltr")
    align <- ifelse(is_rtl, "right", "left")
    tags$style(HTML(paste0("
      body { direction: ", dir, " !important; text-align: ", align, " !important; }
      .sidebar-panel { border-left: none !important; border-", ifelse(is_rtl, "right", "left"), ": 4px solid ", COL_CM6RI_GREEN, " !important; }
      .form-control, .selectize-input { text-align: ", align, " !important; }
      .alert-info-custom, .alert-warning-custom { border-left: none !important; border-", ifelse(is_rtl, "right", "left"), ": 4px solid !important; }
    ")))
  })
  
  output$nav_links_ui <- renderUI({
    onglets <- list(c("nav_home", "home", "home"), c("nav_prediction", "prediction", "stethoscope"),
                    c("nav_import", "import", "upload"), c("nav_history", "history", "folder-open"))
    tagList(lapply(onglets, function(o) {
      actif <- page() == o[2]
      actionButton(paste0("nav_", o[2]), label = tagList(icon(o[3]), t(o[1])),
                   style = paste0("background:", if (actif) "rgba(255,255,255,0.2)" else "transparent",
                                  "; color: white; border: none; border-radius: 8px; padding: 8px 16px; margin: 0 2px; font-weight: 600; font-size: 15px;"))
    }))
  })
  
  lire_historique <- function() {
    if (!file.exists(fichier_historique) || file.info(fichier_historique)$size == 0) {
      return(data.frame(Date = character(), ID_Patient = character(), Age = numeric(),
                        Sexe = character(), APOE = character(), MMSE = numeric(),
                        CDRSB = numeric(), MoCA = numeric(), Probabilite = numeric(),
                        Decision = character(), stringsAsFactors = FALSE))
    }
    tryCatch({
      df <- read_csv(fichier_historique, show_col_types = FALSE)
      if ("Nb_Variables_Estimees" %in% names(df)) df <- df %>% select(-Nb_Variables_Estimees)
      df
    }, error = function(e) {
      data.frame(Date = character(), ID_Patient = character(), Age = numeric(),
                 Sexe = character(), APOE = character(), MMSE = numeric(),
                 CDRSB = numeric(), MoCA = numeric(), Probabilite = numeric(),
                 Decision = character(), stringsAsFactors = FALSE)
    })
  }
  
  valeurs <- reactiveValues(derniere_prediction = NULL)
  valeurs_batch <- reactiveValues(resultats = NULL)
  
  ## ---------------- PAGE : ACCUEIL ----------------
  page_accueil <- function() {
    div(class = "container-fluid", style = "padding: 28px;",
        div(class = "hero-banner",
            div(style = "position: relative; z-index: 1;",
                div(style = "display: flex; align-items: center; gap: 20px; margin-bottom: 14px;",
                    if (LOGO_URL != "") tags$img(src = LOGO_URL, class = "logo-hero"),
                    div(h1(style = "color: white; margin: 0; font-weight: 700; font-size: 34px;", t("hero_title")))),
                p(style = "color: rgba(255,255,255,0.92); font-size: 17px; margin: 0; max-width: 680px;", t("hero_subtitle")))
        ),
        
        # Boxes
        div(style = "display: flex; gap: 20px; justify-content: space-between; margin-bottom: 30px;",
            div(style = "flex: 1;", div(class = "stat-card", style = paste0("border-top-color:", COL_CM6RI_RED, ";"), div(class = "stat-label", t("stat_auc")), div(class = "stat-value", style = paste0("color:", COL_CM6RI_RED, ";"), as.character(metriques$auc_test)))),
            div(style = "flex: 1;", div(class = "stat-card", style = paste0("border-top-color:", COL_CM6RI_GREEN, ";"), div(class = "stat-label", t("stat_ece")), div(class = "stat-value", style = paste0("color:", COL_CM6RI_GREEN, ";"), as.character(metriques$ece_test)))),
            div(style = "flex: 1;", div(class = "stat-card", style = "border-top-color: #0984E3;", div(class = "stat-label", t("stat_seuil")), div(class = "stat-value", style = "color: #0984E3;", as.character(round(SEUIL_OPTIMAL, 2))))),
            div(style = "flex: 1;", div(class = "stat-card", style = paste0("border-top-color:", COL_RISQUE_MOD, ";"), div(class = "stat-label", t("stat_patients")), div(class = "stat-value", style = paste0("color:", COL_RISQUE_MOD, ";"), as.character(metriques$n_test))))
        ),
        
        hr(),
        div(class = "card-premium", h3(class = "titre-section", t("about_title")),
            fluidRow(
              column(6, h4(class = "sous-titre", t("about_objectif")), p(t("about_objectif_txt"), style = "font-size: 15px; line-height: 1.6;"),
                     h4(class = "sous-titre", t("about_methodo")),
                     tags$ul(tags$li(t("about_methodo_1")), tags$li(t("about_methodo_2")), tags$li(t("about_methodo_3")), style = "font-size: 15px; line-height: 1.6;")),
              column(6, h4(class = "sous-titre", t("about_warning_title")),
                     div(class = "alert-warning-custom", p(t("about_warning_txt"), style = "font-size: 15px; line-height: 1.6;")))),
            hr(), div(style = "text-align: center; padding: 10px;",
                      p(style = paste0("color:", COL_TEXTE_ATTEN, "; font-size: 13px;"), t("footer_copyright"))))
    )
  }
  
  ## ---------------- PAGE : PRÉDICTION MANUELLE----------------
  page_prediction <- function() {
    div(class = "container-fluid", style = "padding: 28px;",
        sidebarLayout(
          # Barre latérale compacte à gauche (25% de la largeur)
          sidebarPanel(width = 3, class = "sidebar-panel", style = "max-width: 380px !important; flex: 0 0 25% !important;",
                       h3(class = "titre-section", t("form_title")),
                       p(em(t("form_hint")), style = paste0("color:", COL_TEXTE_ATTEN, "; font-size: 14px; line-height: 1.5;")),
                       textInput("rid", strong(t("field_id")), value = paste0("NV-", format(Sys.Date(), "%Y%m%d"), "-001")),
                       numericInput("age", strong(t("field_age")), value = 70, min = 50, max = 100),
                       selectInput("sexe", strong(t("field_sexe")), choices = setNames(c("1", "2"), c(t("field_homme"), t("field_femme")))),
                       numericInput("education", strong(t("field_education")), value = 12, min = 0, max = 25),
                       hr(),
                       h4(class = "sous-titre", tags$span(class = "section-icon", "🧠"), t("section_cognitif")),
                       champ_num_info("mmscore", t("field_mmse"), t("field_mmse_full"), value = 25, min = 0, max = 30),
                       champ_num_info("cdrsb", t("field_cdrsb"), t("field_cdrsb_full"), value = 2, min = 0, max = 18),
                       champ_num_info("moca", t("field_moca"), t("field_moca_full"), value = 24, min = 0, max = 30),
                       champ_num_info("total13", t("field_total13"), t("field_total13_full"), value = NA, min = 0, max = 85),
                       champ_num_info("faqtravl", t("field_faq"), t("field_faq_full"), value = NA, min = 0, max = 30),
                       hr(),
                       h4(class = "sous-titre", tags$span(class = "section-icon", "🧬"), t("section_genetique")),
                       div(
                         tags$label(style = "font-weight: 600; display: flex; align-items: center; gap: 6px; margin-bottom: 6px; font-size: 15px;",
                                    t("field_genotype"),
                                    tags$span(title = t("field_genotype_full"), style = "cursor: help; color: #ADB5BD; font-size: 14px; border: 1px solid #ADB5BD; border-radius: 50%; width: 16px; height: 16px; display: inline-flex; align-items: center; justify-content: center; line-height: 1;", "i")),
                         selectInput("genotype", label = NULL, choices = setNames(c("", "3/3", "3/4", "4/4", "2/3", "2/4"), c(t("field_non_type"), "3/3", "3/4", "4/4", "2/3", "2/4")))
                       ),
                       hr(),
                       h4(class = "sous-titre", tags$span(class = "section-icon", "🩸"), t("section_biomarqueurs")),
                       p(style = paste0("font-size: 13px; color:", COL_TEXTE_ATTEN, "; margin-top: -6px;"), t("field_optionnel")),
                       champ_num_info("tau", t("field_tau"), t("field_tau_full"), value = NA),
                       champ_num_info("ptau", t("field_ptau"), t("field_ptau_full"), value = NA),
                       champ_num_info("abeta42", t("field_abeta"), t("field_abeta_full"), value = NA),
                       hr(),
                       actionButton("predict_btn", t("btn_predict"), class = "btn-premium", style = "width: 100%;")
          ),
          # Zone de résultats à droite (75% de la largeur)
          mainPanel(width = 9,
                    div(class = "card-premium", 
                        h2(class = "titre-section", t("result_title")), 
                        uiOutput("resultats")
                    ),
                    hr(),
                    downloadButton("download_report", t("btn_download_report"), class = "btn-success-premium", style = "width: 100%; font-size: 15px; padding: 10px;")
          )
        )
    )
  }
  
  output$resultats <- renderUI({
    # Message d'accueil si aucune prédiction n'a encore été faite
    if (is.null(valeurs$derniere_prediction)) {
      return(div(style = "text-align: center; padding: 60px 20px; color: #636E72; background: #F8F9FA; border-radius: 12px; border: 2px dashed #DEE2E6;",
                 icon("stethoscope", style = "font-size: 56px; margin-bottom: 15px; color: #ADB5BD;"),
                 h4(style = "color: #2D3436; margin-bottom: 10px; font-size: 20px;", "En attente d'analyse"),
                 p(style = "font-size: 16px;", "Veuillez remplir les informations du patient dans le panneau de gauche et cliquer sur le bouton d'analyse pour afficher les résultats ici.")))
    }
    
    res <- valeurs$derniere_prediction$resultat
    patient <- valeurs$derniere_prediction$patient
    proba_pct <- res$proba * 100
    
    if (res$proba >= 0.7) {
      couleur <- COL_RISQUE_HAUT; niveau <- t("niveau_eleve")
      recommandations <- tags$ul(tags$li(t("reco_eleve_1")), tags$li(t("reco_eleve_2")), tags$li(t("reco_eleve_3")), style = "font-size: 15px; line-height: 1.6;")
    } else if (res$proba >= 0.4) {
      couleur <- COL_RISQUE_MOD; niveau <- t("niveau_modere")
      recommandations <- tags$ul(tags$li(t("reco_mod_1")), tags$li(t("reco_mod_2")), tags$li(t("reco_mod_3")), style = "font-size: 15px; line-height: 1.6;")
    } else {
      couleur <- COL_RISQUE_FAIBLE; niveau <- t("niveau_faible")
      recommandations <- tags$ul(tags$li(t("reco_faible_1")), tags$li(t("reco_faible_2")), style = "font-size: 15px; line-height: 1.6;")
    }
    
    jauge_svg <- creer_jauge_svg(proba_pct, couleur, libelle = if (lang() == "fr") "Risque" else if (lang() == "en") "Risk" else "الخطر")
    
    div(
      div(class = "alert-info-custom", style = "margin-bottom: 18px;",
          fluidRow(column(6, p(strong(paste0(t("result_patient"), " : ")), patient$RID, style = "font-size: 15px;")),
                   column(6, p(strong(paste0(t("result_age"), " : ")), patient$entry_age, " | ", strong(paste0(t("result_sexe"), " : ")), ifelse(patient$PTGENDER == 1, t("field_homme"), t("field_femme")), style = "font-size: 15px;")))),
      div(class = "result-box", style = paste0("border-color:", couleur, "40;"),
          fluidRow(
            column(5, div(style = "text-align: center;", HTML(jauge_svg))),
            column(7, div(style = "padding: 18px;",
                          div(style = paste0("display: inline-block; background: ", couleur, "; color: #FFFFFF; padding: 8px 20px; border-radius: 20px; font-weight: 700; font-size: 14px; text-transform: uppercase; letter-spacing: 1px;"), niveau),
                          h2(style = paste0("color: ", couleur, "; margin-top: 14px; font-size: 22px;"), t("result_risque_titre")),
                          p(style = paste0("font-size: 18px; color:", COL_TEXTE, ";"), strong(paste0(proba_pct, "%")), " ", t("result_proba")),
                          p(style = paste0("font-size: 14px; color:", COL_TEXTE_ATTEN, "; font-style: italic;"), paste0(t("result_seuil"), " : ", round(SEUIL_OPTIMAL, 2), " (Youden)"))
            )))
      ),
      hr(), div(class = "card-premium", h3(class = "titre-section", t("reco_title")), recommandations),
      p(em(t("disclaimer")), style = paste0("color:", COL_TEXTE_ATTEN, "; font-size: 13px; text-align: center; margin-top: 20px;"))
    )
  })
  
  observeEvent(input$predict_btn, {
    id_patient <- trimws(input$rid)
    if (id_patient == "" || is.na(id_patient)) { showNotification(t("err_id_vide"), type = "error", duration = 5); return() }
    historique_actuel <- lire_historique()
    if (id_patient %in% historique_actuel$ID_Patient) {
      showNotification(HTML(paste0("<strong>", t("err_id_existe"), "</strong><br>", t("err_id_existe_txt"))), type = "error", duration = 8); return()
    }
    patient <- data.frame(RID = id_patient, entry_age = input$age, PTEDUCAT = input$education,
                          PTGENDER = as.integer(input$sexe), MMSCORE = input$mmscore,
                          CDRSB = input$cdrsb, MOCA = input$moca, stringsAsFactors = FALSE)
    if (input$genotype != "") patient$GENOTYPE <- input$genotype
    if (!is.na(input$total13))  patient$TOTAL13  <- input$total13
    if (!is.na(input$faqtravl)) patient$FAQTRAVL <- input$faqtravl
    if (!is.na(input$tau))      patient$TAU      <- input$tau
    if (!is.na(input$ptau))     patient$PTAU     <- input$ptau
    if (!is.na(input$abeta42))  patient$ABETA42  <- input$abeta42
    
    res <- predire_patient(patient, modele, data_ref, seuil = SEUIL_OPTIMAL)
    
    nouvelle_ligne <- data.frame(
      Date = as.character(format(Sys.time(), "%d/%m/%Y %H:%M")), ID_Patient = as.character(id_patient),
      Age = as.numeric(input$age), Sexe = as.character(ifelse(input$sexe == "1", "Homme", "Femme")),
      APOE = as.character(ifelse(input$genotype == "", "Non type", input$genotype)),
      MMSE = as.numeric(input$mmscore), CDRSB = as.numeric(input$cdrsb), MoCA = as.numeric(input$moca),
      Probabilite = as.numeric(res$proba), Decision = as.character(res$decision), stringsAsFactors = FALSE
    )
    write_csv(bind_rows(historique_actuel, nouvelle_ligne), fichier_historique)
    valeurs$derniere_prediction <- list(patient = patient, resultat = res)
    showNotification(HTML(paste0("<strong>", t("msg_pred_ok"), "</strong><br>", id_patient, " ", t("msg_pred_ok_txt"))), type = "message", duration = 4)
  })
  
  ## ---------------- PAGE : IMPORT EXCEL (BATCH) ----------------
  page_import <- function() {
    div(class = "container-fluid", style = "padding: 28px;",
        # FIRST ROW: TWO CARDS SIDE BY SIDE
        fluidRow(
          column(6, 
                 div(class = "card-premium", 
                     h3(class = "titre-section", t("import_step1")), 
                     p(t("import_step1_txt"), style = "font-size: 15px; line-height: 1.5;"),
                     downloadButton("download_template", t("btn_template"), class = "btn-success-premium", style = "width: 100%; margin-top: 10px;")
                 )
          ),
          column(6,
                 div(class = "card-premium",
                     h3(class = "titre-section", t("import_step2")),
                     div(class = "file-upload-box", fileInput("file_excel", t("import_choose"), accept = c(".xlsx", ".xls", ".csv"))),
                     actionButton("btn_analyser_fichier", t("btn_batch"), class = "btn-premium", style = "width: 100%; margin-top: 14px;")
                 )
          )
        ),
        
        hr(),
        
        div(class = "card-premium", 
            h3(class = "titre-section", t("import_step3")), 
            uiOutput("resume_batch"), 
            hr(), 
            uiOutput("download_batch")
        )
    )
  }
  
  output$download_template <- downloadHandler(
    filename = function() "Template_Patients.xlsx",
    content = function(file) {
      template <- data.frame(RID = c("P001", "P002", "P003"), entry_age = c(72, 68, 75), PTGENDER = c(1, 2, 1),
                             PTEDUCAT = c(12, 16, 10), MMSCORE = c(24, 28, 22), CDRSB = c(3, 1, 5), MOCA = c(22, 27, 20),
                             FAQTRAVL = c(4, 1, 8), TOTAL13 = c(18, 8, 25), GENOTYPE = c("3/4", "3/3", "4/4"),
                             TAU = c(450, 250, 680), PTAU = c(55, 28, 92), ABETA42 = c(380, 650, 220))
      writexl::write_xlsx(template, file)
    }
  )
  
  observeEvent(input$btn_analyser_fichier, {
    req(input$file_excel)
    tryCatch({
      df <- read_excel(input$file_excel$datapath)
      if (nrow(df) == 0) { showNotification(t("err_fichier_vide"), type = "error"); return() }
      colonnes_requises <- c("entry_age", "PTGENDER", "MMSCORE", "CDRSB", "MOCA")
      colonnes_manquantes <- setdiff(colonnes_requises, names(df))
      if (length(colonnes_manquantes) > 0) {
        showNotification(paste(t("err_colonnes"), paste(colonnes_manquantes, collapse = ", ")), type = "error", duration = 8); return()
      }
      showNotification(t("msg_analyse_cours"), type = "message", duration = 6)
      resultats_list <- lapply(seq_len(nrow(df)), function(i) {
        patient_row <- df[i, , drop = FALSE]
        res <- predire_patient(patient_row, modele, data_ref, seuil = SEUIL_OPTIMAL)
        data.frame(ID_Patient = ifelse("RID" %in% names(df), df$RID[i], paste0("Patient_", i)),
                   Age = df$entry_age[i], MMSE = df$MMSCORE[i], CDRSB = df$CDRSB[i], MoCA = df$MOCA[i],
                   Probabilite = res$proba, Decision = res$decision, stringsAsFactors = FALSE)
      })
      valeurs_batch$resultats <- bind_rows(resultats_list)
      showNotification(paste(t("msg_analyse_ok"), nrow(df), t("msg_patients_traites")), type = "message", duration = 5)
    }, error = function(e) showNotification(paste(t("err_lecture"), e$message), type = "error", duration = 8))
  })
  
  output$resume_batch <- renderUI({
    req(valeurs_batch$resultats)
    res <- valeurs_batch$resultats
    nb_haut <- sum(res$Decision == "Haut risque", na.rm = TRUE)
    nb_faible <- sum(res$Decision == "Risque faible", na.rm = TRUE)
    div(
      h4(paste(nrow(res), t("batch_success")), style = "font-size: 18px;"),
      fluidRow(
        column(6, div(class = "stat-card", style = paste0("border-top-color:", COL_CM6RI_RED, ";"), div(class = "stat-value", style = paste0("color:", COL_CM6RI_RED, "; font-size: 32px;"), nb_haut), div(class = "stat-label", t("batch_haut")))),
        column(6, div(class = "stat-card", style = paste0("border-top-color:", COL_CM6RI_GREEN, ";"), div(class = "stat-value", style = paste0("color:", COL_CM6RI_GREEN, "; font-size: 32px;"), nb_faible), div(class = "stat-label", t("batch_faible"))))
      ), hr(), DTOutput("table_batch")
    )
  })
  
  output$table_batch <- renderDT({
    req(valeurs_batch$resultats)
    datatable(valeurs_batch$resultats, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle('Decision', backgroundColor = styleEqual(c("Haut risque", "Risque faible"), c(paste0(COL_RISQUE_HAUT, "20"), paste0(COL_RISQUE_FAIBLE, "20"))), fontWeight = 'bold') %>%
      formatStyle('Probabilite', backgroundColor = styleInterval(c(0.4, 0.7), c(paste0(COL_RISQUE_FAIBLE, "20"), paste0(COL_RISQUE_MOD, "20"), paste0(COL_RISQUE_HAUT, "20"))), fontWeight = 'bold')
  })
  
  output$download_batch <- renderUI({
    req(valeurs_batch$resultats)
    downloadButton("download_resultats_excel", t("btn_download_batch"), class = "btn-success-premium", style = "width: 100%;")
  })
  
  output$download_resultats_excel <- downloadHandler(
    filename = function() paste0("Resultats_Lot_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(valeurs_batch$resultats, file)
  )
  
  ## ---------------- PAGE : HISTORIQUE ----------------
  page_history <- function() {
    div(class = "container-fluid", style = "padding: 28px;",
        div(class = "hero-banner", style = "padding: 24px 28px;",
            div(h2(style = "color: white; margin: 0; font-size: 24px;", t("history_title")),
                p(style = "color: rgba(255,255,255,0.9); margin: 6px 0 0 0; font-size: 16px;", t("history_subtitle")))),
        div(class = "card-premium",
            div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 18px;",
                h3(class = "titre-section", t("history_patients")),
                div(actionButton("refresh_history", t("btn_refresh"), class = "btn-success-premium", style = "margin-right: 10px;"),
                    downloadButton("download_history", t("btn_export_csv"), class = "btn-premium"))),
            DTOutput("table_historique"))
    )
  }
  
  output$table_historique <- renderDT({
    historique <- lire_historique()
    if (nrow(historique) == 0) return(datatable(data.frame(Message = "-"), rownames = FALSE))
    datatable(historique, options = list(pageLength = 10, order = list(list(0, 'desc')), dom = 'frtip'), rownames = FALSE, selection = "single") %>%
      formatStyle('Probabilite', backgroundColor = styleInterval(c(0.4, 0.7), c(paste0(COL_RISQUE_FAIBLE, "20"), paste0(COL_RISQUE_MOD, "20"), paste0(COL_RISQUE_HAUT, "20"))), fontWeight = 'bold') %>%
      formatStyle('Decision', backgroundColor = styleEqual(c('Risque faible', 'Haut risque'), c(paste0(COL_RISQUE_FAIBLE, "20"), paste0(COL_RISQUE_HAUT, "20"))), fontWeight = 'bold')
  })
  
  observeEvent(input$refresh_history, {
    output$table_historique <- renderDT({
      historique <- lire_historique()
      datatable(historique, options = list(pageLength = 10, order = list(list(0, 'desc'))), rownames = FALSE, selection = "single")
    })
  })
  
  output$download_history <- downloadHandler(
    filename = function() paste0("Historique_", Sys.Date(), ".csv"),
    content = function(file) file.copy(fichier_historique, file)
  )
  
  ## ---------------- RAPPORT PDF ----------------
  output$download_report <- downloadHandler(
    filename = function() { req(valeurs$derniere_prediction); paste0("Rapport_", valeurs$derniere_prediction$patient$RID, "_", Sys.Date(), ".pdf") },
    content = function(file) {
      req(valeurs$derniere_prediction)
      res <- valeurs$derniere_prediction$resultat
      patient <- valeurs$derniere_prediction$patient
      proba_pct <- round(res$proba * 100, 1)
      logo_base64 <- if (file.exists(logo_path)) base64enc::dataURI(file = logo_path, mime = "image/png") else ""
      couleur_risque <- ifelse(res$proba >= 0.7, COL_RISQUE_HAUT, ifelse(res$proba >= 0.4, COL_RISQUE_MOD, COL_RISQUE_FAIBLE))
      html_temp <- tempfile(fileext = ".html")
      is_rtl <- lang() == "ar"
      dir_attr <- ifelse(is_rtl, 'dir="rtl"', '')
      font_family <- ifelse(is_rtl, "'Cairo', sans-serif", "'Segoe UI', Arial, sans-serif")
      
      rapport_html <- paste0("<!DOCTYPE html><html ", dir_attr, "><head><meta charset='UTF-8'><title>", t("pdf_title"), "</title>
        <style>@page { size: A4; margin: 1.5cm; } body { font-family: ", font_family, "; margin: 0; padding: 20px; background: #FFFFFF; color: #2D3436; font-size: 15px; }
        .header { background: ", COL_CM6RI_RED, "; padding: 25px; border-radius: 12px; display: flex; align-items: center; gap: 20px; margin-bottom: 25px; color: white; }
        .header img { height: 60px; background: white; padding: 4px; border-radius: 6px; }
        .header h1 { margin: 0; font-size: 24px; } .header h2 { margin: 5px 0; font-size: 18px; font-weight: normal; opacity: 0.9; } .header p { margin: 0; font-size: 14px; opacity: 0.8; }
        .section { background: #FFFFFF; padding: 20px; border-radius: 10px; margin-bottom: 20px; border: 1px solid rgba(0,0,0,0.05); border-left: 4px solid ", COL_CM6RI_GREEN, "; }
        .section h2 { color: ", COL_CM6RI_RED, "; margin-top: 0; font-size: 18px; border-bottom: 1px solid rgba(0,0,0,0.05); padding-bottom: 8px; }
        table { border-collapse: collapse; width: 100%; margin: 10px 0; } th, td { border: 1px solid #DEE2E6; padding: 10px; text-align: left; font-size: 15px; } th { background: #F8F9FA; font-weight: 600; width: 35%; color: #2D3436; }
        .result-box { background: #F8F9FA; padding: 25px; border-radius: 12px; text-align: center; margin: 15px 0; border: 2px solid ", couleur_risque, "; }
        .result-box .proba { font-size: 48px; font-weight: 700; color: ", couleur_risque, "; margin: 10px 0; }
        .result-box .decision { font-size: 20px; font-weight: 600; color: ", couleur_risque, "; text-transform: uppercase; }
        .footer { margin-top: 30px; padding-top: 15px; border-top: 1px solid rgba(0,0,0,0.05); text-align: center; font-size: 13px; color: #636E72; }
        .warning { background: rgba(243,156,18,0.08); padding: 10px; border-radius: 6px; border-left: 4px solid ", COL_RISQUE_MOD, "; font-size: 14px; color: #2D3436; margin-top: 15px; }
        </style></head><body>
        <div class='header'>", if (logo_base64 != "") paste0("<img src='", logo_base64, "'/>") else "", "<div><h1>NeuroVigil</h1><h2>", t("pdf_title"), "</h2><p>", t("pdf_subtitle"), "</p></div></div>
        <div class='section'><h2>", t("pdf_info_patient"), "</h2><table>
          <tr><th>", t("pdf_id"), "</th><td>", patient$RID, "</td></tr><tr><th>", t("pdf_age"), "</th><td>", patient$entry_age, "</td></tr>
          <tr><th>", t("pdf_sexe"), "</th><td>", ifelse(patient$PTGENDER == 1, t("field_homme"), t("field_femme")), "</td></tr>
          <tr><th>", t("pdf_education"), "</th><td>", patient$PTEDUCAT, "</td></tr>
          <tr><th>", t("pdf_mmse"), "</th><td>", patient$MMSCORE, " / 30</td></tr><tr><th>", t("pdf_cdrsb"), "</th><td>", patient$CDRSB, " / 18</td></tr>
          <tr><th>", t("pdf_moca"), "</th><td>", patient$MOCA, " / 30</td></tr><tr><th>", t("pdf_apoe"), "</th><td>", ifelse("GENOTYPE" %in% names(patient), as.character(patient$GENOTYPE), t("field_non_type")), "</td></tr>
        </table></div>
        <div class='section'><h2>", t("pdf_resultat"), "</h2>
          <div class='result-box'><div style='font-size: 15px;'>", t("pdf_proba_txt"), "</div><div class='proba'>", proba_pct, " %</div><div class='decision'>", res$decision, "</div>
          <div style='font-size: 13px; color:#636E72; margin-top: 10px;'>", t("result_seuil"), " : ", round(SEUIL_OPTIMAL, 2), " (Youden)</div></div>
        </div>
        <div class='warning'><strong>", t("pdf_warning"), "</strong></div>
        <div class='footer'>", t("pdf_footer_txt"), " ", metriques$n_test, " - AUC=", metriques$auc_test, " - ECE=", metriques$ece_test, "<br>", format(Sys.time(), '%d/%m/%Y %H:%M'), "<br><strong>CM6RI - UM6SS</strong></div>
        </body></html>")
      
      writeLines(rapport_html, html_temp)
      tryCatch({ pagedown::chrome_print(input = html_temp, output = file, wait = 2, timeout = 30) },
               error = function(e) { warning("PDF impossible : ", e$message); file.copy(html_temp, file) })
      if (file.exists(html_temp)) file.remove(html_temp)
    }
  )
  
  ## ---------------- ROUTAGE DE PAGE ----------------
  output$page_content <- renderUI({
    switch(page(),
           "home" = page_accueil(),
           "prediction" = page_prediction(),
           "import" = page_import(),
           "history" = page_history()
    )
  })
}

shinyApp(ui = ui, server = server)