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
san_macchine <- dbGetQuery(connection, "SELECT id, revision_date, revision, typology, deleted, company, intended_use, model, brand FROM san_machine;") |>
  distinct()

## pulizia nomi ##
san_macchine$typology <- tolower(san_macchine$typology)
san_macchine$intended_use <- tolower(san_macchine$intended_use)

pattern <- " - meccanica gm|-|gr\\.|kg|,|\\.|\\(|\\)|numeri di serie|/|\\||s\\.n\\.|\\+|&|°|tipo|_|[0-9]+|:|demo|lllll|prova|<|>|[\"'’“”]|cod\\."

san_macchine_corretto <- san_macchine |>
  filter(year(revision_date) > 2023,
         deleted == 0) |>
  mutate(
    tipologia = gsub(pattern, "", typology, ignore.case = TRUE), # rimuove i valori sopra
    tipologia = gsub("\\b[[:alpha:]]\\b", "", tipologia),        # rimuove le lettere singole
    tipologia = gsub("\\b[[:alpha:]]{2}\\b", "", tipologia),     # rimuove le parole di due lettere
    tipologia = trimws(gsub("\\s+", " ", tipologia)),            # rimuove i doppi spazi
    uso =  gsub(pattern, "", intended_use, ignore.case = TRUE), # rimuove i valori sopra
    uso = gsub("\\b[[:alpha:]]\\b", "", uso),        # rimuove le lettere singole
    uso = gsub("\\b[[:alpha:]]{2}\\b", "", uso),     # rimuove le parole di due lettere
    uso = trimws(gsub("\\s+", " ", uso)),            # rimuove i doppi spazi
  )

typos <- san_macchine_corretto |>
  distinct(id, tipologia) |>
  group_by(tipologia) |>
  summarise(n = n()) |>
  filter(n > 1) |>
  ungroup() |>
  mutate(
    prima_parola = word(tipologia, 1),
    seconda_parola = word(tipologia, 2),
    prima_parola = gsub("\u00A0", " ", prima_parola),
    prima_parola = trimws(prima_parola, whitespace = "[\\h\\v]"),
    prima_parola = gsub("cqtcn", "", prima_parola, ignore.case = TRUE),
    prima_parola = gsub("[[:space:]]+", " ", prima_parola),
    seconda_parola = gsub("\u00A0", " ", seconda_parola),
    seconda_parola = trimws(seconda_parola, whitespace = "[\\h\\v]"),
    seconda_parola = gsub("cqtcn", "", seconda_parola, ignore.case = TRUE),
    seconda_parola = gsub("[[:space:]]+", " ", seconda_parola)
  ) |>
  unique()

write_xlsx(typos, "categorizzazione_macchine.xlsx")
