source(here::here("R", "00_config.R"))
APP_DIR <- here::here("app")

needed <- c(FILES$model, FILES$metrics, FILES$recipe, FILES$reference)
for (f in needed) require_file(proc_path(f), "Run scripts/run_all.R first.")
dir.create(file.path(APP_DIR, "data"), showWarnings = FALSE)
stopifnot(all(file.copy(proc_path(needed), file.path(APP_DIR, "data"), overwrite = TRUE)))

# do not upload the local patient history
hist_file <- file.path(APP_DIR, "data", "historique_predictions.csv")
if (file.exists(hist_file)) file.remove(hist_file)

rsconnect::deployApp(APP_DIR, appName = "NeuroVigil_App", account = "neuropredictai",
                     server = "shinyapps.io", forceUpdate = TRUE)
