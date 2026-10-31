# Runs the whole pipeline in order. Start from a clean R session.
for (s in c("01_data_preparation.R", "02_train_model.R", "03_temporal_robustness.R",
            "04_prediction_nouveau_patient.R", "05_dca_fairness.R")) {
  message("==== ", s, " ====")
  source(here::here("scripts", s), echo = FALSE)
}
