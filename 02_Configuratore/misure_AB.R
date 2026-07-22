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
  distinct(uso) |>
  mutate(
    prima_parola = word(uso, 1),
    seconda_parola = word(uso, 2),
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


### Misure richieste da Andrea Brunetti ###


## Scomposizione per tipo di macchina (di cosa) ##



# conteggio tipologie
conteggio_tipologie_macchine <- san_macchine |>
  filter(year(revision_date) > 2022,
         deleted == 0,
         !is.na(typology),
         !is.na(intended_use),
         intended_use != c("----", "-")) |>
  mutate(typology = tolower(typology),
         intended_use = tolower(intended_use),
         n_tipologie = n_distinct(typology),
         n_intenti = n_distinct(intended_use)) |>
  select(n_tipologie, n_intenti) |> unique()

# tipologie presenti solo una volta
tipologie_uniche <- san_macchine |>
  mutate(typology = tolower(typology),
         intended_use = tolower(intended_use))|>
  filter(year(revision_date) > 2022,
         deleted == 0,
         !is.na(typology),
         !is.na(intended_use),
         intended_use != c("----", "-")) |>
  group_by(typology) |>
  summarise(n = n()) |>
  select(tipologie = typology, n) |>
  filter(n == 1) |>
  select(tipologie)

# tipologie utilizzi presenti solo una volta (sono 1704)
utilizzi_unici <- san_macchine |>
  mutate(typology = tolower(typology),
         intended_use = tolower(intended_use)) |>
  filter(year(revision_date) > 2022,
         deleted == 0,
         !is.na(typology),
         !is.na(intended_use),
         intended_use != c("----", "-")) |>
  group_by(intended_use) |>
  summarise(n = n()) |>
  select(utilizzi = intended_use, n) |>
  filter(n == 1) |>
  select(utilizzi)


## Scomposizone macchine ok per tipo di macchina ##

select COUNT(distinct sm.typology) as numero_tipologie, count(distinct intended_use) as numero_gruppo_intenti
from san_machine sm 
where revision = 1
and year(sm.revision_date) > 2022
and sm.deleted = 0
and not isnull(sm.intended_use)
and not isnull(sm.typology);

# numero macchine ok per tipologia 
select sm.typology as tipologia, sum(sm.revision) as numero_macchine_ok
from san_machine sm 
where revision = 1
and year(sm.revision_date) > 2022
and sm.deleted = 0
and not isnull(sm.intended_use)
and not isnull(sm.typology)
group by sm.typology;

select sm.intended_use as tipologia, sum(sm.revision) as numero_macchine_ok
from san_machine sm 
where revision = 1
and year(sm.revision_date) > 2022
and sm.deleted = 0
and not isnull(sm.intended_use)
and not sm.intended_use = "-"
and not sm.intended_use = "----"
and not isnull(sm.typology)
group by sm.intended_use;

select tipologia
from(select sm.intended_use as tipologia, sum(sm.revision) as numero_macchine_ok
     from san_machine sm 
     where revision = 1
     and year(sm.revision_date) > 2022
     and sm.deleted = 0
     and not isnull(sm.intended_use)
     and not sm.intended_use = "-"
     and not sm.intended_use = "----"
     and not isnull(sm.typology)
     group by sm.intended_use) as sub
where numero_macchine_ok = 1;

# Scomposizione per settore merciologico (di cosa)
# Scomposizione merciologica di chi ha rinnovato
# Scomposizione merciologica di chi non ha rinnovato
# Classifica delle macchine rinnovate

# DISCONNESSIONE DA DB
dbDisconnect(connection)
