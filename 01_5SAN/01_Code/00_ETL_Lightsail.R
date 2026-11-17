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
  "04_Counter_Analysis.R",
  "07_Alerts.R"
)

# Ambiente nuovo per ogni esecuzione: nessun riuso di oggetti di ETL precedenti.
local({
  etl_env <- new.env(parent = globalenv())
  etl_env$.etl_read_input <- function(name, path) {
    if (exists(name, envir = etl_env, inherits = FALSE)) {
      get(name, envir = etl_env, inherits = FALSE)
    } else {
      readRDS(path)
    }
  }
  for (script in scripts) {
    message("Eseguo: ", script)
    source(here("01_Code", script), local = etl_env)
  }
})

message("ETL completato correttamente")
