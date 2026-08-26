library(here)

dir.create(
  here("02_Output"),
  recursive = TRUE,
  showWarnings = FALSE
)

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

message("ETL completato correttamente")