library(xml2)
library(dplyr)
library(here)
library(purrr) 
library(DBI)
library(RMariaDB)
library(lubridate)
library(stringr)
library(writexl)

# CONNESSIONE A DB
readRenviron("Config_connection.Renviron")

connection <- dbConnect(
  RMariaDB::MariaDB(),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Elenco tabelle
tabelle <- dbGetQuery(connection, "SHOW TABLES;")

# Caricamento della tabella san_macchine dal configuratore
san_macchine <- dbGetQuery(connection, "SELECT id, revision_date, is_completed, revision, typology, deleted, owner_name, company, intended_use, model, product_code, brand, serial_number, company_serial_number, ir FROM san_machine;") |>
  distinct()

nome_azienda <- c("Agriform Sca Formaggi")

estrazione <- san_macchine |>
  filter(owner_name %in% nome_azienda,
         is_completed == 1,
         deleted == 0,
         is.na(product_code) == F) |>
  select(
    Proprietario = owner_name,
    Azienda = company,
    Revisione = revision,
    Marca = brand,
    Coupon = product_code,
    # Completata = is_completed,
    # Eliminata = deleted,
    Tipologia = typology,
    Seriale = serial_number,
    Matricola_interna = company_serial_number,
    ir) |>
  group_by(Coupon) |>
  slice_max(Revisione, n = 1, with_ties = FALSE) |>
  ungroup()

# DISCONNESSIONE DA DB
dbDisconnect(connection)

write_xlsx(estrazione, here("02_Output", "Estrazioni VdR", paste0(nome_azienda,"_VdR.xlsx")))
