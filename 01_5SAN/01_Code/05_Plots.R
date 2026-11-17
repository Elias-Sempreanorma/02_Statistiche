# Punto di ingresso della dashboard. Ordine e ambienti di valutazione invariati.
# Le parti 01-08 appartengono all’app; le parti 10-22 alla singola sessione.
library(here)

source(here("01_Code", "dashboard", "01_accesso.R"), local = TRUE)
source(here("01_Code", "dashboard", "02_dati.R"), local = TRUE)
source(here("01_Code", "dashboard", "03_grafica.R"), local = TRUE)
source(here("01_Code", "dashboard", "04_mappe.R"), local = TRUE)
source(here("01_Code", "dashboard", "06_assets.R"), local = TRUE)
source(here("01_Code", "dashboard", "07_script.R"), local = TRUE)
source(here("01_Code", "dashboard", "08_stile.R"), local = TRUE)
source(here("01_Code", "dashboard", "05_ui.R"), local = TRUE)

# Parsing una sola volta per processo; valutazione separata per ogni sessione.
dashboard_server_parts <- lapply(c(
  "10_sessione.R",
  "11_finestre.R",
  "12_report_calcoli.R",
  "13_report_vista.R",
  "14_home.R",
  "15_nok.R",
  "16_segnalazioni.R",
  "17_vita.R",
  "18_manovre.R",
  "19_nok_storico.R",
  "20_utilizzo.R",
  "21_disponibilita.R",
  "22_tabelle.R"
), function(file) parse(here("01_Code", "dashboard", file), keep.source = FALSE))

server <- function(input, output, session) {
  for (part in dashboard_server_parts) {
    eval(part, envir = environment())
  }
}

shinyApp(ui, server)
