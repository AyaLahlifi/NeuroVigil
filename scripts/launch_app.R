source(here::here("R", "00_config.R"))

APP_DIR <- here::here("app")
require_file(file.path(APP_DIR, "app.R"), "Put your app.R in the app/ folder.")

needed <- c(FILES$model, FILES$metrics, FILES$recipe, FILES$reference)
for (f in needed) require_file(proc_path(f), "Run scripts/run_all.R first (script 04 creates the reference file).")

dir.create(file.path(APP_DIR, "data"), showWarnings = FALSE)
ok <- file.copy(proc_path(needed), file.path(APP_DIR, "data"), overwrite = TRUE)
stopifnot(all(ok))
message("Copied: ", paste(needed, collapse = ", "))

shiny::runApp(APP_DIR, launch.browser = TRUE)
