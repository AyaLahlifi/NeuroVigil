library(shiny)
library(tidyverse)
library(funcml)
library(recipes)
library(DT)
library(base64enc)
library(readxl)
library(writexl)


logo_path <- "www/logo_cm6ri.png"
if (file.exists(logo_path)) {
  addResourcePath("app_logo", dirname(logo_path))
  LOGO_URL <- file.path("app_logo", basename(logo_path))
} else {
  warning("Logo introuvable : ", logo_path)
  LOGO_URL <- ""
}

favicon_path <- "www/CM6RI-Picto32.png"
FAVICON_URL <- if (file.exists(favicon_path)) "www/CM6RI-Picto32.png" else LOGO_URL

COL_BG_LIGHT      <- "#F8F9FA"
COL_BG_CARD       <- "#FFFFFF"
COL_BG_CARD_SOFT  <- "#F1F3F5"
COL_CM6RI_RED     <- "#C8102E"
COL_CM6RI_GREEN   <- "#00A651"
COL_TEXTE         <- "#2D3436"
COL_TEXTE_ATTEN   <- "#636E72"
COL_RISQUE_HAUT   <- "#D63031"
COL_RISQUE_MOD    <- "#F39C12"
COL_RISQUE_FAIBLE <- "#00B894"

data_dir <- "data"

modele      <- readRDS(file.path(data_dir, "modele_final_xgboost_2F.rds"))
data_ref    <- readRDS(file.path(data_dir, "data_train_reference.rds"))
metriques   <- readRDS(file.path(data_dir, "metriques_modele_2F.rds"))
recipe_prep <- readRDS(file.path(data_dir, "recette_imputation.rds"))

fichier_historique <- file.path(data_dir, "historique_predictions.csv")

if (!file.exists(fichier_historique)) {
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  write_csv(
    data.frame(
      Date = character(), ID_Patient = character(), Age = numeric(),
      Sexe = character(), APOE = character(), MMSE = numeric(),
      CDRSB = numeric(), MoCA = numeric(), Probabilite = numeric(),
      Decision = character(), stringsAsFactors = FALSE
    ),
    fichier_historique
  )
}

SEUIL_BAS  <- metriques$seuil_bas
SEUIL_HAUT <- metriques$seuil_haut

cat("Seuils chargés : Bas =", round(SEUIL_BAS, 3), "| Haut =", round(SEUIL_HAUT, 3), "\n")


generer_prochain_id <- function() {
  prefix_date <- paste0("NV-", format(Sys.Date(), "%Y%m%d"), "-")
  
  if (!file.exists(fichier_historique)) return(paste0(prefix_date, "001"))
  
  hist <- tryCatch(
    read_csv(fichier_historique, show_col_types = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(hist) || nrow(hist) == 0) return(paste0(prefix_date, "001"))
  
  ids_du_jour <- hist$ID_Patient[startsWith(hist$ID_Patient, prefix_date)]
  
  if (length(ids_du_jour) == 0) return(paste0(prefix_date, "001"))
  
  numeros <- as.integer(substr(ids_du_jour, nchar(prefix_date) + 1, nchar(ids_du_jour)))
  prochain_numero <- max(numeros, na.rm = TRUE) + 1
  
  return(paste0(prefix_date, sprintf("%03d", prochain_numero)))
}


TR <- list(
  fr = list(
    app_title = "NeuroVigil", app_subtitle = "Centre Mohammed VI de la Recherche et de l'Innovation",
    nav_home = "Accueil", nav_prediction = "Prédiction", nav_import = "Import Excel", nav_history = "Historique",
    hero_title = "Bienvenue sur NeuroVigil", hero_subtitle = "Plateforme d'intelligence artificielle pour la prédiction du risque de conversion MCI vers la maladie d'Alzheimer à 24 mois.",
    stat_auc = "Performance (AUC)", stat_ece = "Calibration (ECE)", stat_seuil = "Seuils Optimaux", stat_patients = "Patients Validés",
    about_title = "À propos", about_objectif = "Objectif", about_objectif_txt = "Identifier les patients MCI à haut risque de conversion vers Alzheimer dans les 24 mois.",
    about_methodo = "Méthodologie", about_methodo_1 = "Modèle XGBoost (cohorte ADNI)", about_methodo_2 = "Imputation KNN (recipe apprise sur le train)",
    about_methodo_3 = paste0("3 niveaux : Faible (<", round(SEUIL_BAS, 2), "), Modéré (", round(SEUIL_BAS, 2), "-", round(SEUIL_HAUT, 2), "), Élevé (≥", round(SEUIL_HAUT, 2), ")"),
    about_warning_title = "Avertissement", about_warning_txt = "Aide à la décision uniquement. La décision finale revient au médecin traitant.",
    form_title = "Données du patient", form_hint = "Remplissez les champs disponibles. Les valeurs manquantes seront estimées automatiquement.",
    field_id = "ID Patient", field_age = "Âge (ans)", field_sexe = "Sexe", field_homme = "Homme", field_femme = "Femme", field_education = "Années d'études",
    section_cognitif = "Tests cognitifs", field_mmse = "MMSE (0-30)", field_cdrsb = "CDR-SB (0-18)", field_moca = "MoCA (0-30)", field_total13 = "ADAS-Cog TOTAL13", field_faq = "FAQ (autonomie)",
    field_mmse_full = "Mini-Mental State Examination", field_cdrsb_full = "Clinical Dementia Rating Sum of Boxes", field_moca_full = "Montreal Cognitive Assessment",
    field_total13_full = "ADAS-Cog (13 items)", field_faq_full = "Functional Activities Questionnaire",
    section_genetique = "Génétique APOE", field_genotype = "Génotype APOE", field_non_type = "Non typé", field_genotype_full = "Apolipoprotéine E — principal facteur de risque génétique",
    section_biomarqueurs = "Biomarqueurs LCR", field_optionnel = "Optionnels", field_tau = "TAU (pg/mL)", field_ptau = "PTAU (pg/mL)", field_abeta = "ABETA42 (pg/mL)",
    field_tau_full = "Protéine Tau totale", field_ptau_full = "Tau phosphorylée", field_abeta_full = "Peptide amyloïde bêta 42",
    btn_predict = "LANCER L'ANALYSE", result_title = "Résultat de l'analyse", btn_download_report = "Télécharger le rapport PDF",
    result_patient = "Patient", result_age = "Âge", result_sexe = "Sexe", result_risque_titre = "Risque de conversion à 24 mois", result_proba = "de probabilité", result_seuils = "Seuils",
    niveau_eleve = "ÉLEVÉ", niveau_modere = "MODÉRÉ", niveau_faible = "FAIBLE",
    reco_title = "Recommandations cliniques",
    reco_eleve_1 = "Suivi neurologique rapproché (tous les 3 à 6 mois)", reco_eleve_2 = "Évaluation pour thérapies anti-amyloïdes (ex: Lecanemab, Donanemab)", reco_eleve_3 = "Anticipation médico-sociale et soutien actif aux aidants",
    reco_mod_1 = "Suivi clinique attentif (tous les 6 à 12 mois)", reco_mod_2 = "Répéter les biomarqueurs LCR et l'IRM dans 6 mois", reco_mod_3 = "Encouragement des mesures préventives (activité physique et cognitive)",
    reco_faible_1 = "Maintien du suivi de routine (12 à 18 mois)", reco_faible_2 = "Encouragement des activités de prévention cognitive",
    disclaimer = "Aide à la décision. La décision finale revient au clinicien.",
    import_step1 = "1. Télécharger le modèle", import_step1_txt = "Utilisez ce fichier Excel comme modèle.", btn_template = "Télécharger le modèle Excel",
    import_step2 = "2. Importer vos données", import_choose = "Choisir un fichier Excel (.xlsx ou .csv)", btn_batch = "LANCER L'ANALYSE EN LOT",
    import_step3 = "3. Résultats de l'analyse en lot", batch_success = "patients analysés avec succès", batch_haut = "HAUT RISQUE", batch_mod = "RISQUE MODÉRÉ", batch_faible = "RISQUE FAIBLE",
    btn_download_batch = "Télécharger les résultats (Excel)",
    history_title = "Historique des prédictions", history_subtitle = "Suivi longitudinal des patients analysés", history_patients = "Patients analysés",
    btn_refresh = "Actualiser", btn_export_csv = "Exporter CSV", btn_clear_history = "Effacer l'historique", msg_history_cleared = "Historique effacé avec succès !",
    err_id_vide = "L'ID du patient ne peut pas être vide !", err_id_existe = "ID déjà utilisé !", err_id_existe_txt = "Ce patient existe déjà.",
    msg_pred_ok = "Prédiction enregistrée !", msg_pred_ok_txt = "ajouté.", err_fichier_vide = "Le fichier est vide !", err_colonnes = "Colonnes manquantes :",
    msg_analyse_cours = "Analyse en cours...", msg_analyse_ok = "Analyse terminée avec succès !", msg_patients_traites = "patients traités.", err_lecture = "Erreur de lecture :",
    footer_copyright = "© 2026 CM6RI - Université Mohammed VI des Sciences de la Santé",
    pdf_title = "Rapport de Prédiction Alzheimer", pdf_subtitle = "NeuroVigil — Aide à la décision", pdf_info_patient = "Informations du patient",
    pdf_id = "ID Patient", pdf_age = "Âge", pdf_sexe = "Sexe", pdf_education = "Années d'études", pdf_mmse = "MMSE", pdf_cdrsb = "CDR-SB", pdf_moca = "MoCA", pdf_apoe = "Génotype APOE",
    pdf_resultat = "Résultat", pdf_proba_txt = "Probabilité de conversion à 24 mois", pdf_warning = "Aide à la décision uniquement. La décision finale reste du ressort du médecin.",
    pdf_footer_txt = "Modèle validé sur", pdf_footer_patients = "patients de test",
    pdf_generating = "Génération du rapport PDF…", pdf_popup_blocked = "Le navigateur a bloqué la fenêtre d'impression. Autorisez les fenêtres pop-up pour ce site, puis réessayez.",
    pdf_generated_on = "Rapport généré le"
  ),
  en = list(
    app_title = "NeuroVigil", app_subtitle = "Mohammed VI Center for Research and Innovation",
    nav_home = "Home", nav_prediction = "Prediction", nav_import = "Excel Import", nav_history = "History",
    hero_title = "Welcome to NeuroVigil", hero_subtitle = "AI platform for predicting the risk of MCI-to-Alzheimer's conversion within 24 months.",
    stat_auc = "Performance (AUC)", stat_ece = "Calibration (ECE)", stat_seuil = "Optimal Thresholds", stat_patients = "Validated Patients",
    about_title = "About", about_objectif = "Objective", about_objectif_txt = "Identify MCI patients at high risk of converting to Alzheimer's within 24 months.",
    about_methodo = "Methodology", about_methodo_1 = "XGBoost model (ADNI cohort)", about_methodo_2 = "KNN imputation (recipe learned on train)",
    about_methodo_3 = paste0("3 levels: Low (<", round(SEUIL_BAS, 2), "), Moderate (", round(SEUIL_BAS, 2), "-", round(SEUIL_HAUT, 2), "), High (≥", round(SEUIL_HAUT, 2), ")"),
    about_warning_title = "Disclaimer", about_warning_txt = "Decision support only. Final decision remains with the treating physician.",
    form_title = "Patient Data", form_hint = "Fill available fields. Missing values will be estimated automatically.",
    field_id = "Patient ID", field_age = "Age (years)", field_sexe = "Sex", field_homme = "Male", field_femme = "Female", field_education = "Years of education",
    section_cognitif = "Cognitive tests", field_mmse = "MMSE (0-30)", field_cdrsb = "CDR-SB (0-18)", field_moca = "MoCA (0-30)", field_total13 = "ADAS-Cog TOTAL13", field_faq = "FAQ (functional autonomy)",
    field_mmse_full = "Mini-Mental State Examination", field_cdrsb_full = "Clinical Dementia Rating Sum of Boxes", field_moca_full = "Montreal Cognitive Assessment",
    field_total13_full = "ADAS-Cog (13 items)", field_faq_full = "Functional Activities Questionnaire",
    section_genetique = "APOE Genetics", field_genotype = "APOE genotype", field_non_type = "Not typed", field_genotype_full = "Apolipoprotein E — leading genetic risk factor",
    section_biomarqueurs = "CSF Biomarkers", field_optionnel = "Optional", field_tau = "TAU (pg/mL)", field_ptau = "PTAU (pg/mL)", field_abeta = "ABETA42 (pg/mL)",
    field_tau_full = "Total Tau protein", field_ptau_full = "Phosphorylated Tau", field_abeta_full = "Amyloid beta 42 peptide",
    btn_predict = "RUN ANALYSIS", result_title = "Analysis Result", btn_download_report = "Download PDF Report",
    result_patient = "Patient", result_age = "Age", result_sexe = "Sex", result_risque_titre = "24-month conversion risk", result_proba = "probability", result_seuils = "Thresholds",
    niveau_eleve = "HIGH", niveau_modere = "MODERATE", niveau_faible = "LOW",
    reco_title = "Clinical Recommendations",
    reco_eleve_1 = "Close neurological follow-up (every 3 to 6 months)", reco_eleve_2 = "Evaluation for anti-amyloid therapies (e.g., Lecanemab, Donanemab)", reco_eleve_3 = "Medico-social planning and active caregiver support",
    reco_mod_1 = "Attentive clinical follow-up (every 6 to 12 months)", reco_mod_2 = "Repeat CSF biomarkers and MRI in 6 months", reco_mod_3 = "Encourage preventive measures (physical and cognitive activity)",
    reco_faible_1 = "Maintain routine follow-up (12 to 18 months)", reco_faible_2 = "Encourage cognitive prevention activities",
    disclaimer = "Decision support only. Final decision remains with the clinician.",
    import_step1 = "1. Download Template", import_step1_txt = "Use this Excel file as a template.", btn_template = "Download Excel Template",
    import_step2 = "2. Import Your Data", import_choose = "Choose an Excel file (.xlsx or .csv)", btn_batch = "RUN BATCH ANALYSIS",
    import_step3 = "3. Batch Analysis Results", batch_success = "patients successfully analyzed", batch_haut = "HIGH RISK", batch_mod = "MODERATE RISK", batch_faible = "LOW RISK",
    btn_download_batch = "Download Results (Excel)",
    history_title = "Prediction History", history_subtitle = "Longitudinal follow-up of analyzed patients", history_patients = "Analyzed patients",
    btn_refresh = "Refresh", btn_export_csv = "Export CSV", btn_clear_history = "Clear History", msg_history_cleared = "History cleared successfully!",
    err_id_vide = "Patient ID cannot be empty!", err_id_existe = "ID already used!", err_id_existe_txt = "This patient already exists.",
    msg_pred_ok = "Prediction saved!", msg_pred_ok_txt = "added.", err_fichier_vide = "The file is empty!", err_colonnes = "Missing columns:",
    msg_analyse_cours = "Analysis in progress...", msg_analyse_ok = "Analysis completed successfully!", msg_patients_traites = "patients processed.", err_lecture = "Reading error:",
    footer_copyright = "© 2026 CM6RI - Mohammed VI University of Health Sciences",
    pdf_title = "Alzheimer's Prediction Report", pdf_subtitle = "NeuroVigil — Clinical Decision Support", pdf_info_patient = "Patient Information",
    pdf_id = "Patient ID", pdf_age = "Age", pdf_sexe = "Sex", pdf_education = "Years of education", pdf_mmse = "MMSE", pdf_cdrsb = "CDR-SB", pdf_moca = "MoCA", pdf_apoe = "APOE genotype",
    pdf_resultat = "Result", pdf_proba_txt = "24-month conversion probability", pdf_warning = "Decision support only. Final clinical decision remains with the physician.",
    pdf_footer_txt = "Model validated on", pdf_footer_patients = "test patients",
    pdf_generating = "Generating the PDF report…", pdf_popup_blocked = "Your browser blocked the print window. Allow pop-ups for this site, then try again.",
    pdf_generated_on = "Report generated on"
  ),
  ar = list(
    app_title = "نوروفيجيل", app_subtitle = "مركز محمد السادس للبحث والابتكار",
    nav_home = "الرئيسية", nav_prediction = "التنبؤ", nav_import = "استيراد Excel", nav_history = "السجل",
    hero_title = "مرحباً بكم في نوروفيجيل", hero_subtitle = "منصة ذكاء اصطناعي للتنبؤ بخطر تحول الضعف الإدراكي المعتدل إلى مرض الزهايمر خلال 24 شهراً.",
    stat_auc = "الأداء (AUC)", stat_ece = "المعايرة (ECE)", stat_seuil = "العتبات المثلى", stat_patients = "المرضى المعتمدون",
    about_title = "حول التطبيق", about_objectif = "الهدف", about_objectif_txt = "تحديد مرضى الضعف الإدراكي المعرضين لخطر عالٍ للتحول إلى الزهايمر خلال 24 شهراً.",
    about_methodo = "المنهجية", about_methodo_1 = "نموذج XGBoost (دراسة ADNI)", about_methodo_2 = "استكمال البيانات KNN",
    about_methodo_3 = paste0("3 مستويات: منخفض (<", round(SEUIL_BAS, 2), "), معتدل (", round(SEUIL_BAS, 2), "-", round(SEUIL_HAUT, 2), "), مرتفع (≥", round(SEUIL_HAUT, 2), ")"),
    about_warning_title = "تنويه", about_warning_txt = "أداة دعم قرار فقط. القرار النهائي يبقى من مسؤولية الطبيب المعالج.",
    form_title = "بيانات المريض", form_hint = "املأ الحقول المتاحة. سيتم تقدير القيم المفقودة تلقائياً.",
    field_id = "معرف المريض", field_age = "العمر (سنوات)", field_sexe = "الجنس", field_homme = "ذكر", field_femme = "أنثى", field_education = "سنوات الدراسة",
    section_cognitif = "الاختبارات المعرفية", field_mmse = "MMSE (0-30)", field_cdrsb = "CDR-SB (0-18)", field_moca = "MoCA (0-30)", field_total13 = "ADAS-Cog TOTAL13", field_faq = "FAQ (الاستقلالية)",
    field_mmse_full = "الفحص المعرفي المصغر", field_cdrsb_full = "التقييم السريري للخرف", field_moca_full = "التقييم المعرفي لمونتريال",
    field_total13_full = "ADAS-Cog (13 عنصراً)", field_faq_full = "استبيان الأنشطة الوظيفية",
    section_genetique = "علم الوراثة APOE", field_genotype = "النمط الجيني APOE", field_non_type = "غير مصنف", field_genotype_full = "أبوليبوبروتين E",
    section_biomarqueurs = "العلامات الحيوية للسائل الدماغي", field_optionnel = "اختياري", field_tau = "TAU (pg/mL)", field_ptau = "PTAU (pg/mL)", field_abeta = "ABETA42 (pg/mL)",
    field_tau_full = "بروتين تاو الكلي", field_ptau_full = "تاو المفسفر", field_abeta_full = "ببتيد أميلويد بيتا 42",
    btn_predict = "تشغيل التحليل", result_title = "نتيجة التحليل", btn_download_report = "تحميل تقرير PDF",
    result_patient = "المريض", result_age = "العمر", result_sexe = "الجنس", result_risque_titre = "خطر التحول خلال 24 شهراً", result_proba = "احتمالية", result_seuils = "العتبات",
    niveau_eleve = "مرتفع", niveau_modere = "معتدل", niveau_faible = "منخفض",
    reco_title = "التوصيات السريرية",
    reco_eleve_1 = "متابعة عصبية وثيقة (كل 3 إلى 6 أشهر)", reco_eleve_2 = "تقييم للعلاجات المضادة للأميلويد (مثل: ليكانيماب، دونانيماب)", reco_eleve_3 = "التخطيط الطبي-الاجتماعي والدعم النشط لمقدمي الرعاية",
    reco_mod_1 = "متابعة سريرية منتبهة (كل 6 إلى 12 شهراً)", reco_mod_2 = "إعادة فحص العلامات الحيوية للسائل الدماغي والرنين المغناطيسي خلال 6 أشهر", reco_mod_3 = "تشجيع التدابير الوقائية (النشاط البدني والمعرفي)",
    reco_faible_1 = "الحفاظ على المتابعة الروتينية (12 إلى 18 شهراً)", reco_faible_2 = "تشجيع أنشطة الوقاية المعرفية",
    disclaimer = "أداة دعم قرار فقط. القرار النهائي يبقى من مسؤولية الطبيب.",
    import_step1 = "1. تحميل النموذج", import_step1_txt = "استخدم ملف Excel هذا كنموذج.", btn_template = "تحميل نموذج Excel",
    import_step2 = "2. استيراد بياناتك", import_choose = "اختر ملف Excel (.xlsx أو .csv)", btn_batch = "تشغيل التحليل الجماعي",
    import_step3 = "3. نتائج التحليل الجماعي", batch_success = "مرضى تم تحليلهم بنجاح", batch_haut = "خطر مرتفع", batch_mod = "خطر معتدل", batch_faible = "خطر منخفض",
    btn_download_batch = "تحميل النتائج (Excel)",
    history_title = "سجل التنبؤات", history_subtitle = "المتابعة الطولية للمرضى المحللين", history_patients = "المرضى المحللون",
    btn_refresh = "تحديث", btn_export_csv = "تصدير CSV", btn_clear_history = "مسح السجل", msg_history_cleared = "تم مسح السجل بنجاح!",
    err_id_vide = "لا يمكن أن يكون معرف المريض فارغاً!", err_id_existe = "المعرف مستخدم بالفعل!", err_id_existe_txt = "هذا المريض موجود بالفعل.",
    msg_pred_ok = "تم حفظ التنبؤ!", msg_pred_ok_txt = "تمت الإضافة.", err_fichier_vide = "الملف فارغ!", err_colonnes = "أعمدة مفقودة:",
    msg_analyse_cours = "جاري التحليل...", msg_analyse_ok = "تم التحليل بنجاح!", msg_patients_traites = "مرضى تمت معالجتهم.", err_lecture = "خطأ في القراءة:",
    footer_copyright = "© 2026 مركز محمد السادس - جامعة محمد السادس للعلوم الصحية",
    pdf_title = "تقرير التنبؤ بالزهايمر", pdf_subtitle = "نيوروفيجيل — أداة دعم القرار السريري", pdf_info_patient = "معلومات المريض",
    pdf_id = "معرف المريض", pdf_age = "العمر", pdf_sexe = "الجنس", pdf_education = "سنوات الدراسة", pdf_mmse = "MMSE", pdf_cdrsb = "CDR-SB", pdf_moca = "MoCA", pdf_apoe = "النمط الجيني APOE",
    pdf_resultat = "النتيجة", pdf_proba_txt = "احتمالية التحول خلال 24 شهراً", pdf_warning = "أداة دعم قرار فقط. القرار السريري النهائي يبقى من مسؤولية الطبيب المعالج.",
    pdf_footer_txt = "تم التحقق من صحة النموذج على", pdf_footer_patients = "مريض في مجموعة الاختبار",
    pdf_generating = "جارٍ إنشاء تقرير PDF…", pdf_popup_blocked = "حظر المتصفح نافذة الطباعة. اسمح بالنوافذ المنبثقة لهذا الموقع ثم أعد المحاولة.",
    pdf_generated_on = "تاريخ إنشاء التقرير"
  )
)


predire_patient <- function(patient_data, modele, recipe_prep, data_ref, seuil_bas, seuil_haut) {
  patient_transforme <- patient_data
  
  ref_step <- recipe_prep$steps %>% keep(~ !is.null(.x$ref_data)) %>% first()
  
  niveaux_ptgender <- if (!is.null(ref_step$ref_data$PTGENDER)) levels(ref_step$ref_data$PTGENDER) else c("Homme", "Femme")
  niveaux_genotype <- if (!is.null(ref_step$ref_data$GENOTYPE)) levels(ref_step$ref_data$GENOTYPE) else c("2/2", "2/3", "2/4", "3/3", "3/4", "4/4")
  
  if ("PTGENDER" %in% names(patient_transforme)) {
    if (is.numeric(patient_transforme$PTGENDER) || is.integer(patient_transforme$PTGENDER)) {
      patient_transforme$PTGENDER <- factor(patient_transforme$PTGENDER, levels = c(1, 2), labels = niveaux_ptgender)
    } else {
      patient_transforme$PTGENDER <- factor(as.character(patient_transforme$PTGENDER), levels = niveaux_ptgender)
    }
  } else {
    patient_transforme$PTGENDER <- factor(NA, levels = niveaux_ptgender)
  }
  
  if ("GENOTYPE" %in% names(patient_transforme)) {
    patient_transforme$GENOTYPE <- factor(as.character(patient_transforme$GENOTYPE), levels = niveaux_genotype)
  } else {
    patient_transforme$GENOTYPE <- factor(NA, levels = niveaux_genotype)
  }
  
  patient_transforme$n_e4 <- stringr::str_count(as.character(patient_transforme$GENOTYPE), "4")
  
  vars_recipe <- recipe_prep$var_info %>% filter(role == "predictor") %>% pull(variable)
  manquantes_recipe <- setdiff(vars_recipe, names(patient_transforme))
  for (col in manquantes_recipe) patient_transforme[[col]] <- NA
  
  patient_impute <- bake(recipe_prep, new_data = patient_transforme)
  
  vars_modele <- names(data_ref)
  vars_manquantes <- setdiff(vars_modele, names(patient_impute))
  if (length(vars_manquantes) > 0) stop(paste("Variables manquantes après bake:", paste(vars_manquantes, collapse = ", ")))
  patient_impute <- patient_impute %>% select(all_of(vars_modele))
  
  proba <- predict(modele, patient_impute, type = "prob")[, "Yes"]
  decision <- ifelse(proba >= seuil_haut, "Haut risque", ifelse(proba >= seuil_bas, "Risque modéré", "Risque faible"))
  
  list(proba = round(proba, 3), decision = decision)
}

creer_jauge_svg <- function(pourcentage, couleur, taille = 200, libelle = "Risk") {
  rayon <- 80
  circonference <- 2 * pi * rayon
  offset <- circonference - (pourcentage / 100) * circonference
  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg" style="direction: ltr;">
      <defs>
        <linearGradient id="grad_%s" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
          <stop offset="0%%" style="stop-color:%s;stop-opacity:1" /><stop offset="100%%" style="stop-color:%s;stop-opacity:0.6" />
        </linearGradient>
        <filter id="glow_%s"><feGaussianBlur stdDeviation="4" result="blur"/><feMerge><feMergeNode in="blur"/><feMergeNode in="SourceGraphic"/></feMerge></filter>
      </defs>
      <circle cx="100" cy="100" r="%d" fill="none" stroke="#E9ECEF" stroke-width="14"/>
      <circle cx="100" cy="100" r="%d" fill="none" stroke="url(#grad_%s)" stroke-width="14" stroke-dasharray="%.2f" stroke-dashoffset="%.2f" stroke-linecap="round" transform="rotate(-90 100 100)" filter="url(#glow_%s)" style="transition: stroke-dashoffset 1.5s ease-in-out;"/>
      <text x="100" y="94" text-anchor="middle" font-size="36" font-weight="bold" fill="%s" style="direction: ltr;">%.1f%%</text>
      <text x="100" y="124" text-anchor="middle" font-size="13" fill="#636E72" style="direction: ltr;">%s</text>
    </svg>',
    taille, taille, couleur, couleur, couleur, couleur, rayon, rayon, couleur, circonference, offset, couleur, couleur, pourcentage, libelle
  )
}

champ_num_info <- function(input_id, label, tooltip, ...) {
  div(
    tags$label(`for` = input_id, style = "font-weight: 600; display: flex; align-items: center; gap: 6px; margin-bottom: 6px; font-size: 15px;", label,
               tags$span(title = tooltip, style = "cursor: help; color: #ADB5BD; font-size: 14px; border: 1px solid #ADB5BD; border-radius: 50%; width: 16px; height: 16px; display: inline-flex; align-items: center; justify-content: center; line-height: 1;", "i")
    ),
    numericInput(input_id, label = NULL, ...)
  )
}


HTML2CANVAS_URL <- "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"
JSPDF_URL       <- "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js"

js_pdf <- "
$(function () {
  // Fallback: print the report from a hidden iframe (choose Save as PDF).
  function printFallback(msg) {
    var fr = document.createElement('iframe');
    fr.style.position = 'fixed'; fr.style.right = '0'; fr.style.bottom = '0';
    fr.style.width = '0'; fr.style.height = '0'; fr.style.border = '0';
    document.body.appendChild(fr);
    var d = fr.contentWindow.document;
    d.open(); d.write(msg.page); d.close();
    setTimeout(function () {
      fr.contentWindow.focus(); fr.contentWindow.print();
      setTimeout(function () { document.body.removeChild(fr); }, 2000);
    }, 600);
  }
  // True if the captured image is (almost) only white.
  function isBlank(c) {
    try {
      var ctx = c.getContext('2d'), w = c.width, h = c.height, dark = 0;
      if (!w || !h) return true;
      for (var y = 5; y < h; y += Math.max(1, Math.floor(h / 60))) {
        for (var x = 5; x < w; x += Math.max(1, Math.floor(w / 40))) {
          var p = ctx.getImageData(x, y, 1, 1).data;
          if (p[3] > 0 && (p[0] < 235 || p[1] < 235 || p[2] < 235)) dark++;
        }
      }
      return dark < 5;
    } catch (e) { return false; }
  }
  // Puts the captured image on A4 pages (10 mm margins), splitting it if it is taller than one page.
  function canvasToPdf(canvas, filename) {
    var pdf = new window.jspdf.jsPDF({ unit: 'mm', format: 'a4', orientation: 'portrait' });
    var margin = 10, pageW = 210 - 2 * margin, pageH = 297 - 2 * margin;
    var pxPerMm = canvas.width / pageW;
    var sliceH = Math.floor(pageH * pxPerMm);
    for (var y = 0, page = 0; y < canvas.height; y += sliceH, page++) {
      var h = Math.min(sliceH, canvas.height - y);
      var part = document.createElement('canvas');
      part.width = canvas.width; part.height = h;
      var ctx = part.getContext('2d');
      ctx.fillStyle = '#ffffff'; ctx.fillRect(0, 0, part.width, h);
      ctx.drawImage(canvas, 0, y, canvas.width, h, 0, 0, canvas.width, h);
      if (page > 0) pdf.addPage();
      pdf.addImage(part.toDataURL('image/jpeg', 0.95), 'JPEG', margin, margin, pageW, h / pxPerMm);
    }
    pdf.save(filename);
  }
  Shiny.addCustomMessageHandler('nv_pdf', function (msg) {
    var done = function () { Shiny.setInputValue('nv_pdf_done', Date.now()); };
    if (typeof html2canvas === 'undefined' || !window.jspdf) { printFallback(msg); done(); return; }

    // Capture from the top-left corner of the page, at a fixed width.
    var sx = window.pageXOffset, sy = window.pageYOffset;
    window.scrollTo(0, 0);
    var host = document.createElement('div');
    host.setAttribute('dir', 'ltr');
    host.style.cssText = 'position:absolute;left:0;top:0;width:720px;margin:0;padding:0;background:#fff;z-index:100000;text-align:left;';
    host.innerHTML = msg.html;
    document.body.appendChild(host);
    var report = host.querySelector('.nv-report') || host;
    var cleanup = function () { if (host.parentNode) host.parentNode.removeChild(host); window.scrollTo(sx, sy); };

    var fontsReady = (document.fonts && document.fonts.ready) ? document.fonts.ready : Promise.resolve();
    fontsReady
      .then(function () {
        return html2canvas(report, { scale: 2, useCORS: true, backgroundColor: '#ffffff', scrollX: 0, scrollY: 0 });
      })
      .then(function (canvas) {
        if (isBlank(canvas)) throw new Error('blank capture');
        canvasToPdf(canvas, msg.filename);
        cleanup(); done();
      })
      .catch(function (e) { console.error('NeuroVigil PDF:', e); cleanup(); printFallback(msg); done(); });
  });
});
"

css_global <- paste0(
  "@import url('https://fonts.googleapis.com/css2?family=Cairo:wght@400;600;700&family=Segoe+UI:wght@400;600;700&display=swap');",
  "@keyframes fadeIn { from { opacity: 0; transform: translateY(8px); } to { opacity: 1; transform: translateY(0); } }",
  "@keyframes slideIn { from { opacity: 0; transform: translateX(-16px); } to { opacity: 1; transform: translateX(0); } }",
  "body { font-family: 'Segoe UI', 'Cairo', sans-serif; background: ", COL_BG_LIGHT, "; color: ", COL_TEXTE, "; min-height: 100vh; font-size: 16px; }",
  "body.rtl { font-family: 'Cairo', 'Segoe UI', sans-serif; }",
  ".navbar { background: ", COL_CM6RI_RED, " !important; box-shadow: 0 2px 12px rgba(0,0,0,0.12) !important; padding-top: 10px !important; padding-bottom: 10px !important; }",
  ".navbar-brand, .navbar-nav > li > a { color: #FFFFFF !important; font-weight: 600; font-size: 16px; }",
  ".navbar-nav > li > a:hover { background: rgba(255,255,255,0.15) !important; }",
  ".logo-navbar { height: 48px; width: auto; margin-right: 14px; background: white; padding: 4px; border-radius: 6px; }",
  ".logo-hero { height: 68px; width: auto; margin-bottom: 12px; background: white; padding: 6px; border-radius: 8px; }",
  ".card-premium { background: ", COL_BG_CARD, "; border-radius: 14px; padding: 24px; box-shadow: 0 3px 16px rgba(0,0,0,0.05); border-top: 4px solid ", COL_CM6RI_RED, "; animation: fadeIn 0.5s ease-out; }",
  ".sidebar-panel { background: ", COL_BG_CARD, "; border-radius: 14px; padding: 22px 24px; box-shadow: 0 3px 16px rgba(0,0,0,0.05); border-left: 4px solid ", COL_CM6RI_GREEN, "; }",
  ".btn-premium { background: linear-gradient(135deg, ", COL_CM6RI_RED, " 0%, ", COL_CM6RI_GREEN, " 100%); color: white !important; border: none !important; border-radius: 10px !important; padding: 12px 24px !important; font-weight: 600 !important; }",
  ".btn-success-premium { background: linear-gradient(135deg, #00B894 0%, ", COL_CM6RI_GREEN, " 100%); color: white !important; border: none !important; border-radius: 10px !important; padding: 12px 24px !important; font-weight: 600 !important; }",
  ".btn-lang { background: rgba(255,255,255,0.2) !important; color: white !important; border: 1px solid rgba(255,255,255,0.4) !important; border-radius: 20px !important; padding: 6px 16px !important; font-weight: 600 !important; }",
  ".titre-section { color: ", COL_CM6RI_RED, "; font-weight: 700; font-size: 22px; }",
  ".sous-titre { color: ", COL_CM6RI_RED, "; font-weight: 600; border-bottom: 2px solid rgba(200,16,46,0.2); padding-bottom: 6px; display: inline-block; margin-bottom: 12px; font-size: 16px; }",
  ".stat-card { background: ", COL_BG_CARD, "; border-radius: 12px; padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.06); border-top: 4px solid; animation: slideIn 0.4s ease-out; display: flex; flex-direction: column; justify-content: center; align-items: center; min-height: 140px; }",
  ".stat-value { font-size: 38px; font-weight: 700; margin: 8px 0; line-height: 1; }",
  ".stat-label { font-size: 13px; color: ", COL_TEXTE_ATTEN, "; text-transform: uppercase; letter-spacing: 1px; font-weight: 600; text-align: center; }",
  ".form-control, .selectize-input { border-radius: 8px !important; border: 1px solid #DEE2E6 !important; background: #FFFFFF !important; padding: 8px 12px !important; font-size: 15px !important; }",
  "label { color: ", COL_TEXTE, " !important; font-weight: 600; font-size: 15px; }",
  ".alert-info-custom { background: rgba(0,166,81,0.08); border-left: 4px solid ", COL_CM6RI_GREEN, "; border-radius: 8px; padding: 14px 16px; }",
  ".alert-warning-custom { background: rgba(243,156,18,0.08); border-left: 4px solid ", COL_RISQUE_MOD, "; border-radius: 8px; padding: 14px 16px; }",
  ".result-box { border-radius: 14px; padding: 28px; animation: fadeIn 0.7s ease-out; background: ", COL_BG_CARD_SOFT, "; }",
  ".dataTable tbody tr { background: #FFFFFF !important; font-size: 15px; }",
  "table.dataTable thead th { background: ", COL_BG_CARD_SOFT, " !important; color: ", COL_CM6RI_RED, " !important; }",
  ".hero-banner { background: linear-gradient(135deg, ", COL_CM6RI_RED, " 0%, ", COL_CM6RI_GREEN, " 100%); color: white; padding: 36px 40px; border-radius: 18px; margin-bottom: 26px; }",
  ".file-upload-box { border: 2px dashed ", COL_CM6RI_GREEN, "; border-radius: 12px; padding: 28px; text-align: center; background: rgba(0,166,81,0.03); }",
  "hr { border-top: 1px solid rgba(0,0,0,0.07); margin: 18px 0; }"
)


ui <- tagList(
  tags$head(
    tags$style(HTML(css_global)),
    if (FAVICON_URL != "") tags$link(rel = "icon", type = "image/png", href = FAVICON_URL),
    tags$title("NeuroVigil"),
    tags$script(src = HTML2CANVAS_URL),
    tags$script(src = JSPDF_URL),
    tags$script(HTML(js_pdf)),
    uiOutput("rtl_style")
  ),
  div(
    class = "navbar navbar-default navbar-static-top",
    div(
      class = "container-fluid", style = "display: flex; justify-content: space-between; align-items: center;",
      div(
        style = "display: flex; align-items: center;",
        if (LOGO_URL != "") tags$img(src = LOGO_URL, class = "logo-navbar"),
        div(
          h4(style = "margin: 0; font-weight: 700; color: white; font-size: 24px;", "NeuroVigil"),
          p(style = "margin: 0; font-size: 13px; color: rgba(255,255,255,0.85);", "CM6RI • UM6SS • 3 niveaux de risque")
        )
      ),
      div(
        style = "display: flex; align-items: center; gap: 10px;",
        uiOutput("nav_links_ui"),
        actionButton("toggle_lang", "🇬🇧 English", class = "btn-lang")
      )
    )
  ),
  uiOutput("page_content")
)


server <- function(input, output, session) {
  
  lang <- reactiveVal("fr")
  page <- reactiveVal("home")
  
  observeEvent(input$toggle_lang, {
    current <- lang()
    if (current == "fr") { lang("en"); updateActionButton(session, "toggle_lang", label = "🇸🇦 العربية") } 
    else if (current == "en") { lang("ar"); updateActionButton(session, "toggle_lang", label = "🇫🇷 Français") } 
    else { lang("fr"); updateActionButton(session, "toggle_lang", label = "🇬🇧 English") }
  })
  
  observeEvent(input$nav_home, page("home"))
  observeEvent(input$nav_prediction, {
    page("prediction")
    # Met à jour l'ID automatiquement quand on arrive sur l'onglet Prédiction
    updateTextInput(session, "rid", value = generer_prochain_id())
  })
  observeEvent(input$nav_import, page("import"))
  observeEvent(input$nav_history, page("history"))
  
  t <- function(key) TR[[lang()]][[key]]
  
  output$rtl_style <- renderUI({
    is_rtl <- lang() == "ar"
    dir <- ifelse(is_rtl, "rtl", "ltr")
    align <- ifelse(is_rtl, "right", "left")
    tags$style(HTML(paste0(
      "body { direction: ", dir, " !important; text-align: ", align, " !important; }",
      ".sidebar-panel { border-left: none !important; border-", ifelse(is_rtl, "right", "left"), ": 4px solid ", COL_CM6RI_GREEN, " !important; }"
    )))
  })
  
  output$nav_links_ui <- renderUI({
    onglets <- list(c("nav_home", "home", "home"), c("nav_prediction", "prediction", "stethoscope"), c("nav_import", "import", "upload"), c("nav_history", "history", "folder-open"))
    tagList(lapply(onglets, function(o) {
      actif <- page() == o[2]
      bg_color <- if (actif) "rgba(255,255,255,0.2)" else "transparent"
      actionButton(paste0("nav_", o[2]), label = tagList(icon(o[3]), t(o[1])), style = paste0("background:", bg_color, "; color: white; border: none; border-radius: 8px; padding: 8px 16px; margin: 0 2px; font-weight: 600;"))
    }))
  })
  
  lire_historique <- function() {
    empty_df <- data.frame(Date = character(), ID_Patient = character(), Age = numeric(), Sexe = character(), APOE = character(), MMSE = numeric(), CDRSB = numeric(), MoCA = numeric(), Probabilite = numeric(), Decision = character(), stringsAsFactors = FALSE)
    if (!file.exists(fichier_historique)) return(empty_df)
    tryCatch({
      df <- read_csv(fichier_historique, show_col_types = FALSE)
      required_cols <- c("Date", "ID_Patient", "Age", "Sexe", "APOE", "MMSE", "CDRSB", "MoCA", "Probabilite", "Decision")
      if (!all(required_cols %in% names(df))) { file.remove(fichier_historique); return(empty_df) }
      df %>% mutate(Date = as.character(Date), ID_Patient = as.character(ID_Patient), Age = as.numeric(Age), Sexe = as.character(Sexe), APOE = as.character(APOE), MMSE = as.numeric(MMSE), CDRSB = as.numeric(CDRSB), MoCA = as.numeric(MoCA), Probabilite = as.numeric(Probabilite), Decision = as.character(Decision))
    }, error = function(e) { if (file.exists(fichier_historique)) file.remove(fichier_historique); return(empty_df) })
  }
  
  valeurs <- reactiveValues(derniere_prediction = NULL)
  valeurs_batch <- reactiveValues(resultats = NULL)
  
  # --- PAGE : ACCUEIL ---
  page_accueil <- function() {
    div(class = "container-fluid", style = "padding: 28px;",
        div(class = "hero-banner",
            div(style = "display: flex; align-items: center; gap: 20px; margin-bottom: 14px;",
                if (LOGO_URL != "") tags$img(src = LOGO_URL, class = "logo-hero"),
                div(h1(style = "color: white; margin: 0; font-weight: 700; font-size: 34px;", t("hero_title")))
            ),
            p(style = "color: rgba(255,255,255,0.92); font-size: 17px; margin: 0; max-width: 680px;", t("hero_subtitle"))
        ),
        div(style = "display: flex; gap: 20px; justify-content: space-between; margin-bottom: 30px;",
            div(style = "flex: 1;", div(class = "stat-card", style = paste0("border-top-color:", COL_CM6RI_RED, ";"), div(class = "stat-label", t("stat_auc")), div(class = "stat-value", style = paste0("color:", COL_CM6RI_RED, ";"), as.character(metriques$auc_test)))),
            div(style = "flex: 1;", div(class = "stat-card", style = paste0("border-top-color:", COL_CM6RI_GREEN, ";"), div(class = "stat-label", t("stat_ece")), div(class = "stat-value", style = paste0("color:", COL_CM6RI_GREEN, ";"), as.character(metriques$ece_test)))),
            div(style = "flex: 1;", div(class = "stat-card", style = "border-top-color: #0984E3;", div(class = "stat-label", t("stat_seuil")), div(class = "stat-value", style = "color: #0984E3; font-size: 22px;", paste0(round(SEUIL_BAS, 2), " / ", round(SEUIL_HAUT, 2))))),
            div(style = "flex: 1;", div(class = "stat-card", style = paste0("border-top-color:", COL_RISQUE_MOD, ";"), div(class = "stat-label", t("stat_patients")), div(class = "stat-value", style = paste0("color:", COL_RISQUE_MOD, ";"), as.character(metriques$n_test))))
        ),
        hr(),
        div(class = "card-premium", h3(class = "titre-section", t("about_title")),
            fluidRow(
              column(6, h4(class = "sous-titre", t("about_objectif")), p(t("about_objectif_txt")), h4(class = "sous-titre", t("about_methodo")), tags$ul(tags$li(t("about_methodo_1")), tags$li(t("about_methodo_2")), tags$li(t("about_methodo_3")))),
              column(6, h4(class = "sous-titre", t("about_warning_title")), div(class = "alert-warning-custom", p(t("about_warning_txt"))))
            ),
            hr(), div(style = "text-align: center; padding: 10px;", p(style = paste0("color:", COL_TEXTE_ATTEN, "; font-size: 13px;"), t("footer_copyright")))
        )
    )
  }
  
  # --- PAGE : PRÉDICTION ---
  page_prediction <- function() {
    div(class = "container-fluid", style = "padding: 28px;",
        h2(class = "titre-section", t("form_title"), style = "margin-bottom: 10px;"),
        p(em(t("form_hint")), style = paste0("color:", COL_TEXTE_ATTEN, "; margin-bottom: 25px;")),
        fluidRow(
          column(4, div(class = "card-premium", style = "height: 100%;", h4(class = "sous-titre", "Informations de base"),
                        textInput("rid", strong(t("field_id")), value = generer_prochain_id()),
                        numericInput("age", strong(t("field_age")), value = 70, min = 50, max = 100),
                        selectInput("sexe", strong(t("field_sexe")), choices = setNames(c("1", "2"), c(t("field_homme"), t("field_femme")))),
                        numericInput("education", strong(t("field_education")), value = 12, min = 0, max = 25)
          )),
          column(4, div(class = "card-premium", style = "height: 100%;", h4(class = "sous-titre", " ", t("section_cognitif")),
                        champ_num_info("mmscore", t("field_mmse"), t("field_mmse_full"), value = 25, min = 0, max = 30),
                        champ_num_info("cdrsb", t("field_cdrsb"), t("field_cdrsb_full"), value = 2, min = 0, max = 18),
                        champ_num_info("moca", t("field_moca"), t("field_moca_full"), value = 24, min = 0, max = 30),
                        champ_num_info("total13", t("field_total13"), t("field_total13_full"), value = NA, min = 0, max = 85),
                        champ_num_info("faqtravl", t("field_faq"), t("field_faq_full"), value = NA, min = 0, max = 30)
          )),
          column(4, div(class = "card-premium", style = "height: 100%;", h4(class = "sous-titre", t("section_genetique")),
                        selectInput("genotype", t("field_genotype"), choices = setNames(c("", "3/3", "3/4", "4/4", "2/3", "2/4"), c(t("field_non_type"), "3/3", "3/4", "4/4", "2/3", "2/4"))),
                        hr(), h4(class = "sous-titre", t("section_biomarqueurs")), p(t("field_optionnel"), style = paste0("font-size: 13px; color:", COL_TEXTE_ATTEN, ";")),
                        champ_num_info("tau", t("field_tau"), t("field_tau_full"), value = NA),
                        champ_num_info("ptau", t("field_ptau"), t("field_ptau_full"), value = NA),
                        champ_num_info("abeta42", t("field_abeta"), t("field_abeta_full"), value = NA)
          ))
        ),
        div(style = "text-align: center; margin: 30px 0;", actionButton("predict_btn", t("btn_predict"), class = "btn-premium", style = "width: 320px; font-size: 16px; padding: 12px 24px;")),
        hr(),
        div(class = "card-premium", h2(class = "titre-section", t("result_title")), uiOutput("resultats")),
        hr(),
        uiOutput("download_report_ui") # <-- BOUTON PDF CONDITIONNEL
    )
  }
  
  output$resultats <- renderUI({
    if (is.null(valeurs$derniere_prediction)) {
      return(div(style = "text-align: center; padding: 60px 20px; color: #636E72; background: #F8F9FA; border-radius: 12px; border: 2px dashed #DEE2E6;", icon("stethoscope", style = "font-size: 56px; margin-bottom: 15px; color: #ADB5BD;"), h4("En attente d'analyse"), p("Veuillez remplir les informations et cliquer sur le bouton d'analyse.")))
    }
    res <- valeurs$derniere_prediction$resultat
    patient <- valeurs$derniere_prediction$patient
    proba_pct <- res$proba * 100
    
    if (res$proba >= SEUIL_HAUT) { couleur <- COL_RISQUE_HAUT; niveau <- t("niveau_eleve"); recommandations <- tags$ul(tags$li(t("reco_eleve_1")), tags$li(t("reco_eleve_2")), tags$li(t("reco_eleve_3"))) } 
    else if (res$proba >= SEUIL_BAS) { couleur <- COL_RISQUE_MOD; niveau <- t("niveau_modere"); recommandations <- tags$ul(tags$li(t("reco_mod_1")), tags$li(t("reco_mod_2")), tags$li(t("reco_mod_3"))) } 
    else { couleur <- COL_RISQUE_FAIBLE; niveau <- t("niveau_faible"); recommandations <- tags$ul(tags$li(t("reco_faible_1")), tags$li(t("reco_faible_2"))) }
    
    libelle_jauge <- switch(lang(), "fr" = "Risque", "en" = "Risk", "الخطر")
    jauge_svg <- creer_jauge_svg(proba_pct, couleur, libelle = libelle_jauge)
    sexe_txt <- ifelse(patient$PTGENDER == 1, t("field_homme"), t("field_femme"))
    
    div(
      div(class = "alert-info-custom", style = "margin-bottom: 18px;", fluidRow(column(6, p(strong(paste0(t("result_patient"), " : ")), patient$RID)), column(6, p(strong(paste0(t("result_age"), " : ")), patient$entry_age, " | ", strong(paste0(t("result_sexe"), " : ")), sexe_txt)))),
      div(class = "result-box", style = paste0("border: 2px solid ", couleur, ";"),
          fluidRow(
            column(5, div(style = "text-align: center;", HTML(jauge_svg))),
            column(7, div(style = "padding: 18px;",
                          div(style = paste0("display: inline-block; background: ", couleur, "; color: #FFFFFF; padding: 8px 20px; border-radius: 20px; font-weight: 700; font-size: 14px; text-transform: uppercase;"), niveau),
                          h2(style = paste0("color: ", couleur, "; margin-top: 14px;"), t("result_risque_titre")),
                          p(style = "font-size: 18px;", strong(paste0(proba_pct, "%")), " ", t("result_proba")),
                          p(style = paste0("font-size: 14px; color:", COL_TEXTE_ATTEN, "; font-style: italic;"), paste0(t("result_seuils"), " : Faible < ", round(SEUIL_BAS, 2), " ≤ Modéré < ", round(SEUIL_HAUT, 2), " ≤ Élevé"))
            ))
          )
      ),
      hr(), div(class = "card-premium", h3(class = "titre-section", t("reco_title")), recommandations),
      p(em(t("disclaimer")), style = paste0("color:", COL_TEXTE_ATTEN, "; font-size: 13px; text-align: center; margin-top: 20px;"))
    )
  })
  
  observeEvent(input$predict_btn, {
    tryCatch({
      id_patient <- trimws(input$rid)
      if (id_patient == "" || is.na(id_patient)) { showModal(modalDialog(title = "Erreur", t("err_id_vide"), easyClose = TRUE)); return() }
      historique_actuel <- lire_historique()
      if (id_patient %in% historique_actuel$ID_Patient) { showModal(modalDialog(title = t("err_id_existe"), t("err_id_existe_txt"), easyClose = TRUE)); return() }
      
      patient <- data.frame(RID = id_patient, entry_age = as.numeric(input$age), PTEDUCAT = as.numeric(input$education), PTGENDER = as.integer(input$sexe), MMSCORE = as.numeric(input$mmscore), CDRSB = as.numeric(input$cdrsb), MOCA = as.numeric(input$moca), stringsAsFactors = FALSE)
      if (!is.null(input$genotype) && input$genotype != "") patient$GENOTYPE <- input$genotype
      if (!is.na(input$total13)) patient$TOTAL13 <- as.numeric(input$total13)
      if (!is.na(input$faqtravl)) patient$FAQTRAVL <- as.numeric(input$faqtravl)
      if (!is.na(input$tau)) patient$TAU <- as.numeric(input$tau)
      if (!is.na(input$ptau)) patient$PTAU <- as.numeric(input$ptau)
      if (!is.na(input$abeta42)) patient$ABETA42 <- as.numeric(input$abeta42)
      
      res <- predire_patient(patient, modele, recipe_prep, data_ref, seuil_bas = SEUIL_BAS, seuil_haut = SEUIL_HAUT)
      
      apoe_txt <- ifelse(is.null(input$genotype) || input$genotype == "", "Non type", input$genotype)
      sexe_txt <- ifelse(input$sexe == "1", "Homme", "Femme")
      
      nouvelle_ligne <- data.frame(Date = as.character(format(Sys.time(), "%d/%m/%Y %H:%M")), ID_Patient = as.character(id_patient), Age = as.numeric(input$age), Sexe = as.character(sexe_txt), APOE = as.character(apoe_txt), MMSE = as.numeric(input$mmscore), CDRSB = as.numeric(input$cdrsb), MoCA = as.numeric(input$moca), Probabilite = as.numeric(res$proba), Decision = as.character(res$decision), stringsAsFactors = FALSE)
      
      historique_actuel <- lire_historique()
      nouvelle_ligne <- nouvelle_ligne[, names(historique_actuel)]
      write_csv(bind_rows(historique_actuel, nouvelle_ligne), fichier_historique)
      
      valeurs$derniere_prediction <- list(patient = patient, resultat = res)
      showNotification(HTML(paste0("<strong>", t("msg_pred_ok"), "</strong><br>", id_patient, " ", t("msg_pred_ok_txt"))), type = "message", duration = 4)
      
      # --- INCRÉMENTATION AUTOMATIQUE DE L'ID ---
      prochain_id <- generer_prochain_id()
      updateTextInput(session, "rid", value = prochain_id)
      
    }, error = function(e) {
      showModal(modalDialog(title = "Erreur d'analyse", HTML(paste0("<code>", e$message, "</code>")), easyClose = TRUE))
    })
  })
  
  # --- PAGE : IMPORT EXCEL ---
  page_import <- function() {
    div(class = "container-fluid", style = "padding: 28px;",
        fluidRow(
          column(6, div(class = "card-premium", h3(class = "titre-section", t("import_step1")), p(t("import_step1_txt")), downloadButton("download_template", t("btn_template"), class = "btn-success-premium", style = "width: 100%;"))),
          column(6, div(class = "card-premium", h3(class = "titre-section", t("import_step2")), div(class = "file-upload-box", fileInput("file_excel", t("import_choose"), accept = c(".xlsx", ".xls", ".csv"))), actionButton("btn_analyser_fichier", t("btn_batch"), class = "btn-premium", style = "width: 100%; margin-top: 14px;")))
        ),
        hr(), div(class = "card-premium", h3(class = "titre-section", t("import_step3")), uiOutput("resume_batch"), hr(), uiOutput("download_batch"))
    )
  }
  
  output$download_template <- downloadHandler(filename = function() "Template_Patients.xlsx", content = function(file) {
    template <- data.frame(RID = c("P001", "P002", "P003", "P004"), entry_age = c(72, 68, 75, 90), PTGENDER = c(1, 2, 1, 2), PTEDUCAT = c(12, 16, 10, 7), MMSCORE = c(24, 28, 22, 25), CDRSB = c(3, 1, 5, 4.5), MOCA = c(22, 27, 20, 26), FAQTRAVL = c(4, 1, 8, 9), TOTAL13 = c(18, 8, 25, 11), GENOTYPE = c("3/4", "3/3", "4/4", ""), TAU = c(450, 250, 680, 769), PTAU = c(55, 28, 92, 533), ABETA42 = c(380, 650, 220, 128))
    writexl::write_xlsx(template, file)
  })
  
  observeEvent(input$btn_analyser_fichier, {
    req(input$file_excel)
    tryCatch({
      df <- read_excel(input$file_excel$datapath)
      if (nrow(df) == 0) { showModal(modalDialog(title = "Erreur", t("err_fichier_vide"), easyClose = TRUE)); return() }
      colonnes_requises <- c("entry_age", "PTGENDER", "MMSCORE", "CDRSB", "MOCA")
      colonnes_manquantes <- setdiff(colonnes_requises, names(df))
      if (length(colonnes_manquantes) > 0) { showModal(modalDialog(title = "Erreur", paste(t("err_colonnes"), paste(colonnes_manquantes, collapse = ", ")), easyClose = TRUE)); return() }
      showNotification(t("msg_analyse_cours"), type = "message", duration = 6)
      resultats_list <- lapply(seq_len(nrow(df)), function(i) {
        patient_row <- df[i, , drop = FALSE]
        res <- predire_patient(patient_row, modele, recipe_prep, data_ref, seuil_bas = SEUIL_BAS, seuil_haut = SEUIL_HAUT)
        id_pat <- ifelse("RID" %in% names(df), as.character(df$RID[i]), paste0("Patient_", i))
        data.frame(ID_Patient = id_pat, Age = as.numeric(df$entry_age[i]), MMSE = as.numeric(df$MMSCORE[i]), CDRSB = as.numeric(df$CDRSB[i]), MoCA = as.numeric(df$MOCA[i]), Probabilite = as.numeric(res$proba), Decision = as.character(res$decision), stringsAsFactors = FALSE)
      })
      valeurs_batch$resultats <- bind_rows(resultats_list)
      showNotification(paste(t("msg_analyse_ok"), nrow(df), t("msg_patients_traites")), type = "message", duration = 5)
    }, error = function(e) { showModal(modalDialog(title = "Erreur", HTML(paste0(t("err_lecture"), "<br><code>", e$message, "</code>")), easyClose = TRUE)) })
  })
  
  output$resume_batch <- renderUI({
    req(valeurs_batch$resultats)
    res <- valeurs_batch$resultats
    nb_haut <- sum(res$Decision == "Haut risque", na.rm = TRUE)
    nb_mod <- sum(res$Decision == "Risque modéré", na.rm = TRUE)
    nb_faible <- sum(res$Decision == "Risque faible", na.rm = TRUE)
    div(h4(paste(nrow(res), t("batch_success"))),
        fluidRow(
          column(4, div(class = "stat-card", style = paste0("border-top-color:", COL_RISQUE_HAUT, ";"), div(class = "stat-value", style = paste0("color:", COL_RISQUE_HAUT, "; font-size: 32px;"), nb_haut), div(class = "stat-label", t("batch_haut")))),
          column(4, div(class = "stat-card", style = paste0("border-top-color:", COL_RISQUE_MOD, ";"), div(class = "stat-value", style = paste0("color:", COL_RISQUE_MOD, "; font-size: 32px;"), nb_mod), div(class = "stat-label", t("batch_mod")))),
          column(4, div(class = "stat-card", style = paste0("border-top-color:", COL_RISQUE_FAIBLE, ";"), div(class = "stat-value", style = paste0("color:", COL_RISQUE_FAIBLE, "; font-size: 32px;"), nb_faible), div(class = "stat-label", t("batch_faible"))))
        ), hr(), DTOutput("table_batch")
    )
  })
  
  output$table_batch <- renderDT({
    req(valeurs_batch$resultats)
    datatable(valeurs_batch$resultats, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle('Decision', backgroundColor = styleEqual(c("Haut risque", "Risque modéré", "Risque faible"), c(paste0(COL_RISQUE_HAUT, "20"), paste0(COL_RISQUE_MOD, "20"), paste0(COL_RISQUE_FAIBLE, "20"))), fontWeight = 'bold')
  })
  
  output$download_batch <- renderUI({ req(valeurs_batch$resultats); downloadButton("download_resultats_excel", t("btn_download_batch"), class = "btn-success-premium", style = "width: 100%;") })
  output$download_resultats_excel <- downloadHandler(filename = function() paste0("Resultats_Lot_", Sys.Date(), ".xlsx"), content = function(file) writexl::write_xlsx(valeurs_batch$resultats, file))
  
  # --- PAGE : HISTORIQUE ---
  page_history <- function() {
    div(class = "container-fluid", style = "padding: 28px;",
        div(class = "hero-banner", style = "padding: 24px 28px;", h2(style = "color: white; margin: 0;", t("history_title")), p(style = "color: rgba(255,255,255,0.9); margin: 6px 0 0 0;", t("history_subtitle"))),
        div(class = "card-premium",
            div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 18px;", h3(class = "titre-section", t("history_patients")),
                div(actionButton("clear_history", t("btn_clear_history"), style = "background: #e74c3c; color: white; border: none; border-radius: 8px; padding: 8px 16px; margin-right: 10px; font-weight: 600;"),
                    actionButton("refresh_history", t("btn_refresh"), class = "btn-success-premium", style = "margin-right: 10px;"),
                    downloadButton("download_history", t("btn_export_csv"), class = "btn-premium")
                )
            ),
            DTOutput("table_historique")
        )
    )
  }
  
  output$table_historique <- renderDT({
    historique <- lire_historique()
    if (nrow(historique) == 0) return(datatable(data.frame(Message = "L'historique est vide"), rownames = FALSE))
    datatable(historique, options = list(pageLength = 10, order = list(list(0, 'desc'))), rownames = FALSE) %>%
      formatStyle('Decision', backgroundColor = styleEqual(c('Risque faible', 'Risque modéré', 'Haut risque'), c(paste0(COL_RISQUE_FAIBLE, "20"), paste0(COL_RISQUE_MOD, "20"), paste0(COL_RISQUE_HAUT, "20"))), fontWeight = 'bold')
  })
  
  observeEvent(input$refresh_history, { output$table_historique <- renderDT({ historique <- lire_historique(); datatable(historique, options = list(pageLength = 10, order = list(list(0, 'desc'))), rownames = FALSE) }) })
  
  observeEvent(input$clear_history, {
    showModal(modalDialog(title = "Confirmation", "Êtes-vous sûr de vouloir supprimer définitivement tout l'historique des prédictions ?", footer = tagList(modalButton("Annuler"), actionButton("confirm_clear", "Oui, effacer", style = "background: #e74c3c; color: white; border: none; border-radius: 6px; padding: 6px 12px;"))))
  })
  
  observeEvent(input$confirm_clear, {
    removeModal()
    write_csv(data.frame(Date = character(), ID_Patient = character(), Age = numeric(), Sexe = character(), APOE = character(), MMSE = numeric(), CDRSB = numeric(), MoCA = numeric(), Probabilite = numeric(), Decision = character(), stringsAsFactors = FALSE), fichier_historique)
    output$table_historique <- renderDT({ datatable(data.frame(Message = "L'historique est vide"), options = list(dom = 't'), rownames = FALSE) })
    showNotification(t("msg_history_cleared"), type = "message", duration = 3)
  })
  
  output$download_history <- downloadHandler(filename = function() paste0("Historique_", Sys.Date(), ".csv"), content = function(file) file.copy(fichier_historique, file))
  
  
  
  output$download_report_ui <- renderUI({
    req(valeurs$derniere_prediction)
    actionButton("download_report", label = tagList(icon("file-pdf"), t("btn_download_report")),
                 class = "btn-success-premium", style = "width: 100%; margin-top: 10px;")
  })
  
  # Builds the report as HTML. Returns the fragment given to the browser (html2canvas + jsPDF) and a
  # full page used by the print fallback. Every user-supplied value is escaped.
  construire_rapport <- function(patient, res) {
    esc   <- function(x) htmltools::htmlEscape(as.character(x))
    val   <- function(col, suffix = "") {
      if (col %in% names(patient) && !is.na(patient[[col]]) && as.character(patient[[col]]) != "")
        paste0('<span dir="ltr">', esc(patient[[col]]), suffix, '</span>') else "-"
    }
    proba     <- as.numeric(res$proba)
    proba_pct <- formatC(proba * 100, format = "f", digits = 1)
    
    if (proba >= SEUIL_HAUT) {
      couleur <- COL_RISQUE_HAUT; niveau <- t("niveau_eleve")
      recos   <- c(t("reco_eleve_1"), t("reco_eleve_2"), t("reco_eleve_3"))
    } else if (proba >= SEUIL_BAS) {
      couleur <- COL_RISQUE_MOD; niveau <- t("niveau_modere")
      recos   <- c(t("reco_mod_1"), t("reco_mod_2"), t("reco_mod_3"))
    } else {
      couleur <- COL_RISQUE_FAIBLE; niveau <- t("niveau_faible")
      recos   <- c(t("reco_faible_1"), t("reco_faible_2"))
    }
    
    sexe_txt <- if (as.character(patient$PTGENDER) == "1") t("field_homme") else t("field_femme")
    apoe_txt <- if ("GENOTYPE" %in% names(patient) && !is.na(patient$GENOTYPE) && patient$GENOTYPE != "")
      esc(patient$GENOTYPE) else t("field_non_type")
    
    logo <- ""
    if (file.exists(logo_path)) {
      uri  <- tryCatch(base64enc::dataURI(file = logo_path, mime = "image/png"), error = function(e) "")
      if (nzchar(uri)) logo <- sprintf('<img src="%s" alt="">', uri)
    }
    
    is_rtl <- lang() == "ar"
    dir    <- if (is_rtl) "rtl" else "ltr"
    align  <- if (is_rtl) "right" else "left"
    font   <- if (is_rtl) "'Cairo', 'Segoe UI', Arial, sans-serif" else "'Segoe UI', Arial, sans-serif"
    fmt3   <- function(x) formatC(x, format = "f", digits = 3)
    
    # all rules are scoped to .nv-report so they never leak into the app page
    css <- paste0(
      ".nv-report{font-family:", font, ";color:#2D3436;font-size:13px;line-height:1.5;text-align:", align, ";background:#fff;}",
      ".nv-report *{box-sizing:border-box;}",
      ".nv-report .nv-head{background:", COL_CM6RI_RED, ";color:#fff;padding:18px 20px;border-radius:10px;display:flex;align-items:center;gap:18px;margin-bottom:16px;}",
      ".nv-report .nv-head img{height:56px;width:auto;background:#fff;padding:4px;border-radius:6px;}",
      ".nv-report .nv-head h1{margin:0;font-size:24px;font-weight:700;color:#fff;}",
      ".nv-report .nv-head p{margin:3px 0 0;font-size:15px;color:#fff;}",
      ".nv-report .nv-sec{border:1px solid #E9ECEF;border-", align, ":4px solid ", COL_CM6RI_GREEN, ";border-radius:8px;padding:12px 16px;margin-bottom:12px;}",
      ".nv-report h2{color:", COL_CM6RI_RED, ";font-size:16px;margin:0 0 10px;padding-bottom:6px;border-bottom:1px solid #E9ECEF;}",
      ".nv-report table{border-collapse:collapse;width:100%;margin:0;}",
      ".nv-report th,.nv-report td{border:1px solid #DEE2E6;padding:6px 9px;font-size:13px;text-align:", align, ";}",
      ".nv-report th{background:#F8F9FA;font-weight:600;width:40%;}",
      ".nv-report .nv-result{background:#F8F9FA;border:3px solid ", couleur, ";border-radius:10px;padding:16px;text-align:center;}",
      ".nv-report .nv-proba{font-size:40px;font-weight:700;color:", couleur, ";margin:4px 0;direction:ltr;}",
      ".nv-report .nv-level{display:inline-block;background:", couleur, ";color:#fff;padding:5px 16px;border-radius:16px;font-weight:700;}",
      ".nv-report .nv-thr{margin-top:10px;color:#636E72;font-size:12px;}",
      ".nv-report ul{margin:0;padding-", align, ":20px;}",
      ".nv-report .nv-warn{background:#FFF8E7;border-", align, ":4px solid ", COL_RISQUE_MOD, ";border-radius:6px;padding:10px 12px;font-size:12px;margin-top:4px;}",
      ".nv-report .nv-foot{margin-top:16px;padding-top:10px;border-top:1px solid #DEE2E6;text-align:center;font-size:11px;color:#636E72;}",
      ".nv-report .nv-avoid{page-break-inside:avoid;break-inside:avoid;}"
    )
    
    ligne <- function(lbl, v) sprintf("<tr><th>%s</th><td>%s</td></tr>", lbl, v)
    corps <- paste0(
      '<div class="nv-report" dir="', dir, '" lang="', lang(), '">',
      '<div class="nv-head nv-avoid">', logo, '<div><h1>NeuroVigil</h1><p>', t("pdf_title"), '</p><p>', t("pdf_subtitle"), '</p></div></div>',
      '<div class="nv-sec nv-avoid"><h2>', t("pdf_info_patient"), '</h2><table>',
      ligne(t("pdf_id"), paste0('<span dir="ltr">', esc(patient$RID), '</span>')),
      ligne(t("pdf_age"), val("entry_age")),
      ligne(t("pdf_sexe"), sexe_txt),
      ligne(t("pdf_education"), val("PTEDUCAT")),
      ligne(t("pdf_mmse"), val("MMSCORE", " / 30")),
      ligne(t("pdf_cdrsb"), val("CDRSB", " / 18")),
      ligne(t("pdf_moca"), val("MOCA", " / 30")),
      ligne(t("pdf_apoe"), apoe_txt),
      '</table></div>',
      '<div class="nv-sec nv-avoid"><h2>', t("pdf_resultat"), '</h2><div class="nv-result">',
      '<div>', t("pdf_proba_txt"), '</div><div class="nv-proba">', proba_pct, ' %</div>',
      '<div class="nv-level">', niveau, '</div>',
      '<div class="nv-thr">', t("result_seuils"), ' : <span dir="ltr">',
      t("niveau_faible"), ' &lt; ', fmt3(SEUIL_BAS), ' &le; ', t("niveau_modere"), ' &lt; ', fmt3(SEUIL_HAUT), ' &le; ', t("niveau_eleve"),
      '</span></div></div></div>',
      '<div class="nv-sec nv-avoid"><h2>', t("reco_title"), '</h2><ul>',
      paste0("<li>", recos, "</li>", collapse = ""), '</ul></div>',
      '<div class="nv-warn nv-avoid"><strong>', t("pdf_warning"), '</strong></div>',
      '<div class="nv-foot nv-avoid">', t("pdf_footer_txt"), ' ', metriques$n_test, ' ', t("pdf_footer_patients"),
      ' | AUC = ', fmt3(metriques$auc_test), ' | ECE = ', fmt3(metriques$ece_test), '<br>',
      t("pdf_generated_on"), ' ', format(Sys.time(), "%d/%m/%Y %H:%M"), '<br><strong>CM6RI - UM6SS</strong></div>',
      '</div>'
    )
    fragment <- paste0("<style>", css, "</style>", corps)
    page <- paste0('<!DOCTYPE html><html lang="', lang(), '" dir="', dir, '"><head><meta charset="UTF-8"><title>',
                   t("pdf_title"), '</title><style>@page{size:A4;margin:12mm;}body{margin:0;}', css,
                   '</style></head><body>', corps, '</body></html>')
    list(fragment = fragment, page = page)
  }
  
  observeEvent(input$download_report, {
    req(valeurs$derniere_prediction)
    patient <- valeurs$derniere_prediction$patient
    rapport <- tryCatch(construire_rapport(patient, valeurs$derniere_prediction$resultat),
                        error = function(e) { showModal(modalDialog(title = "PDF", e$message, easyClose = TRUE)); NULL })
    req(rapport)
    nom <- paste0("Rapport_NeuroVigil_", gsub("[^A-Za-z0-9_-]", "_", as.character(patient$RID)), "_",
                  format(Sys.Date(), "%Y%m%d"), ".pdf")
    showNotification(t("pdf_generating"), id = "nv_pdf_note", type = "message", duration = NULL)
    session$sendCustomMessage("nv_pdf", list(html = rapport$fragment, page = rapport$page,
                                             filename = nom, popup_blocked = t("pdf_popup_blocked")))
  })
  observeEvent(input$nv_pdf_done, removeNotification("nv_pdf_note"))
  
  # --- ROUTAGE ---
  output$page_content <- renderUI({
    switch(page(), "home" = page_accueil(), "prediction" = page_prediction(), "import" = page_import(), "history" = page_history())
  })
}

shinyApp(ui = ui, server = server)