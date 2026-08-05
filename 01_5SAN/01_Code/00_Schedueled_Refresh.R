# Questo è il codice da runnare

library(here)

scripts <- c(
  "01_Functions.R",
  "02_Data_Load.R",
  "03_Data_Frequency.R",
  "04_Counter_analysis.R",
  "06_Run_Dashboard.R"
)

for (script in scripts) {
  message("Eseguo: ", script)
  source(here("01_Code", script), local = .GlobalEnv)
}