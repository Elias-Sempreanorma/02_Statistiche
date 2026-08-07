# Aggiorna dati e pubblica la dashboard

library(here)
library(rsconnect)

scripts <- c(
  "01_Functions.R",
  "02_Data_Load.R",
  "03_Data_Frequency.R",
  "04_Counter_Analysis.R"
)

for (script in scripts) {
  message("Eseguo: ", script)
  source(here("01_Code", script), local = .GlobalEnv)
}

rsconnect::deployApp(
  appDir = here::here(),
  appFiles = c(
    "app.R",
    "01_Code/05_Plots.R",
    "02_Output/sensor_count_increment.rds",
    "02_Output/raw_data.rds"
  ),
  appName = "01_5san",
  account = "kb5mot-elias0forma",
  server = "shinyapps.io",
  launch.browser = FALSE
)