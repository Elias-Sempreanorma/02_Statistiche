library(here)
library(rsconnect)

scripts <- c(
  "01_Functions.R",
  "02_Data_Load.R",
  "03_Data_Frequency.R",
  "04_Counter_Analysis.R"
)

for (script in scripts) {
  source(
    here("01_Code", "Test", script),
    local = .GlobalEnv
  )
}

# Crea il pacchetto temporaneo per shinyapps.io
deploy_dir <- tempfile("5san_test_")

dir.create(
  file.path(deploy_dir, "02_Output", "Test"),
  recursive = TRUE
)

# Copia nel pacchetto di deploy le immagini degli schemi. Vengono incluse
# soltanto le immagini, non gli altri file eventualmente presenti in cartella.
dir.create(
  file.path(deploy_dir, "00_Data", "02_CdS"),
  recursive = TRUE
)

immagini_cds <- list.files(
  here("00_Data", "02_CdS"),
  pattern = "\\.(png|jpg|jpeg)$",
  full.names = TRUE,
  ignore.case = TRUE
)

file.copy(
  immagini_cds,
  file.path(deploy_dir, "00_Data", "02_CdS"),
  overwrite = TRUE
)

# 05_Plots.R diventa l'app.R della versione test
file.copy(
  here("01_Code", "Test", "05_Plots.R"),
  file.path(deploy_dir, "app.R")
)

file.copy(
  here("02_Output", "Test", "sensor_count_increment.rds"),
  file.path(deploy_dir, "02_Output", "Test",
            "sensor_count_increment.rds")
)

file.copy(
  here("02_Output", "Test", "raw_data.rds"),
  file.path(deploy_dir, "02_Output", "Test", "raw_data.rds")
)

rsconnect::deployApp(
  appDir = deploy_dir,
  appName = "01_5san",
  account = "kb5mot-elias0forma",
  server = "shinyapps.io",
  launch.browser = FALSE
)