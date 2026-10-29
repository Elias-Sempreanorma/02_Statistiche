# Rscript tests/test_alerts_incremental.R (dalla cartella 01_5SAN)
# Test offline: nessun accesso al DB operativo e nessuna mail.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(jsonlite)
})
source("01_Code/01_Functions.R")
# Carica definizioni senza eseguire il job operativo o caricare SMTP/DB.
for (expr in parse("01_Code/07_Alerts.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      (is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function")) ||
       identical(as.character(expr[[2]]), "insert_alert_sql"))) eval(expr)
}
disconnect_events_path <- disconnect_sensor_events_path <- tempfile()

start <- as.POSIXct("2026-10-28 10:00:00", tz = "Europe/Rome")
example <- tibble(
  coupon = c("A", "A", "B"), alert_type = "activation_hourly",
  period_start = start, period_end = start + hours(1),
  signal_description = "Molte piu' manovre", alert_scope = "sensor",
  gateway_id = c("G1", "G2", "G3"), gateway_name = c("G1", "G2", "G3"),
  sensor_id = c("1", "2", "1"), cds_name = c("S1", "S2", "S1"),
  sensor_description = c("Porta", "Riparo", "Porta"),
  functional_unit = cds_name, signal_data_text = c("10 manovre", "20 manovre", "10 manovre"),
  signal_data_json = '{"hourly_count":10}', generated_at = start + hours(2),
  event_key = c("old1", "old2", "old3"), type_code = "ACTH"
)
grouped <- raggruppa_segnalazioni(example)
stopifnot(nrow(grouped) == 2L)
a <- filter(grouped, coupon == "A")
stopifnot(a$alert_scope == "machine", grepl("S1", a$signal_data_text),
          grepl("S2", a$signal_data_text), nrow(fromJSON(a$signal_data_json)$sensors) == 2L)
shuffled <- raggruppa_segnalazioni(example[3:1, ])
stopifnot(identical(sort(grouped$event_key), sort(shuffled$event_key)))
separate <- bind_rows(example[1, ], mutate(example[2, ], signal_description = "Meno manovre"),
                      mutate(example[2, ], period_start = start + hours(1), period_end = start + hours(2)))
stopifnot(nrow(raggruppa_segnalazioni(separate)) == 3L)
stopifnot(nrow(raggruppa_segnalazioni(example[FALSE, ])) == 0L)
# Migrazione: singoli ID conservati; nuovi gruppi tracciabili verso gli ID originali.
legacy <- mutate(example, alert_id = c("ID1", "ID2", "ID3"), created_at = start)
migrated <- raggruppa_segnalazioni(legacy)
stopifnot(is.na(filter(migrated, coupon == "A")$alert_id),
          filter(migrated, coupon == "B")$alert_id == "ID3",
          grepl("ID1", filter(migrated, coupon == "A")$signal_data_json))
# Mezzanotte locale e ora legale: giorni di 23 e 25 ore.
stopifnot(format(inizio_giorno(as.Date("2026-10-28")), "%H:%M", tz = "Europe/Rome") == "00:00")
for (pair in list(c("2026-03-29", 23), c("2026-10-25", 25))) {
  d <- as.Date(pair[1])
  stopifnot(as.numeric(difftime(inizio_giorno(d + 1), inizio_giorno(d), units = "hours")) == as.numeric(pair[2]))
}
# Dati su 31 giorni: due sensori della stessa macchina e uno su altra macchina.
day <- as.Date("2026-10-28")
raw <- tidyr::expand_grid(i = 1:3, d = seq(day - 30L, day, by = "day")) |>
  mutate(company = "Test", field = "Test", project = "Test",
    coupon = if_else(i == 3L, "B", "A"), machine_name = "Test", machine_serial_number = "Test",
    gateway_id = paste0("G", i), gateway_name = gateway_id,
    sensor_id = as.character(i), cds_name = paste0("S", i), sensor_description = cds_name,
    cds_description = "Test", cds_brand = "Test", cds_use = "Test", cds_vds = "Test",
    timestamp = as.POSIXct(paste(d, "10:00:00"), tz = "Europe/Rome"),
    day = d, count = as.numeric(d - min(d)) + 1, status = 1,
    increment = if_else(d == .env$day, 10, 1), daily_uptime = 1,
    gateway_disconnect_affected = FALSE)
base <- aggrega_valori_giornalieri_nok_data(raw) |>
  mutate(rate = 1, size = Inf, train_end = day - 1L)
attr(raw, "giornalieri_nok") <- base
result <- calcola_segnalazioni_giorno(day, raw, raw)
hourly <- filter(result, alert_type == "activation_hourly")
stopifnot(nrow(hourly) == 2L, filter(hourly, coupon == "A")$alert_scope == "machine",
          all(as.Date(result$period_start, tz = "Europe/Rome") == day))
# Un giorno futuro estremo non cambia classificazione o chiave del giorno valutato.
future <- mutate(filter(raw, day == .env$day), timestamp = timestamp + days(1), day = day + 1L, increment = 1e8)
combined <- bind_rows(raw, future)
attr(combined, "giornalieri_nok") <- bind_rows(base, mutate(filter(base, day == .env$day), day = day + 1L, daily_count = 1e8))
again <- calcola_segnalazioni_giorno(day, combined, combined)
stopifnot(identical(sort(result$event_key), sort(again$event_key)),
          identical(sort(result$signal_data_text), sort(again$signal_data_text)))
# Export della query per il test Postgres isolato (se richiesto).
if (nzchar(Sys.getenv("ALERT_TEST_SQL"))) writeLines(insert_alert_sql, Sys.getenv("ALERT_TEST_SQL"))
cat("OK: raggruppamento, separazione comportamenti/periodi/macchine, migrazione ID, DST, valutazione giornaliera e assenza di dati futuri.\n")

# Test del job con DB simulato: retry, recupero giorni e rollback del checkpoint.
# Le query di produzione restano intatte; nessuna connessione esterna.
state <- new.env()
state$migrated <- FALSE
state$days <- as.Date(character())
state$rows <- list()
state$attempts <- as.Date(character())
state$fail_day <- as.Date(NA)
state$schema_changes <- 0L
legacy_for_test <- mutate(legacy, project = "Test", machine_name = "Test",
                          machine_serial_number = "Test")
connetti_postgres <- function(...) state
dbDisconnect <- function(...) invisible(TRUE)
dbGetQuery <- function(con, sql, params = NULL) {
  if (grepl("pg_try_advisory_lock", sql, fixed = TRUE)) return(data.frame(locked = TRUE))
  if (grepl("FROM public.alert_migrations", sql, fixed = TRUE)) return(tibble(version = if (state$migrated) 2L else integer()))
  if (grepl("MAX(evaluation_day)", sql, fixed = TRUE)) return(tibble(day = if (length(state$days)) max(state$days) else as.Date(NA)))
  if (grepl("AS signal_data_json", sql, fixed = TRUE)) return(legacy_for_test)
  if (grepl("WHERE alert_type = 'gateway_disconnection'", sql, fixed = TRUE)) return(tibble())
  if (grepl("FROM public.alerts", sql, fixed = TRUE)) return(bind_rows(state$rows))
  stop("Query inattesa: ", sql)
}
dbExecute <- function(con, sql, params = NULL) {
  stopifnot(!grepl("DROP TABLE", sql, fixed = TRUE))
  if (identical(sql, insert_alert_sql)) {
    stopifnot(grepl("ON CONFLICT (event_key) DO NOTHING", sql, fixed = TRUE))
    key <- params[[3]]
    if (is.null(state$rows[[key]])) state$rows[[key]] <- tibble(
      event_key = key, alert_id = if (is.na(params[[21]])) paste0("NEW", length(state$rows)) else params[[21]],
      generated_at = params[[2]], signal_data_text = params[[19]],
      period_start = params[[6]], period_end = params[[7]])
  } else if (grepl("INSERT INTO public.alert_evaluation_days", sql, fixed = TRUE)) {
    state$days <- sort(unique(c(state$days, params[[1]])))
  } else if (grepl("INSERT INTO public.alert_migrations", sql, fixed = TRUE)) {
    state$migrated <- TRUE
  } else if (grepl("ALTER TABLE", sql, fixed = TRUE)) state$schema_changes <- state$schema_changes + 1L
  invisible(1L)
}
dbWithTransaction <- function(con, code) {
  snapshot <- as.list(state)
  tryCatch(eval(substitute(code), envir = parent.frame()), error = function(e) {
    for (n in names(snapshot)) state[[n]] <- snapshot[[n]]
    stop(e)
  })
}
real_calcola <- calcola_segnalazioni_giorno
calcola_segnalazioni_giorno <- function(evaluation_day, ...) {
  state$attempts <- c(state$attempts, evaluation_day)
  if (!is.na(state$fail_day) && evaluation_day == state$fail_day) stop("Errore simulato")
  out <- example[1, ] |>
    mutate(project = "Test", machine_serial_number = "Test", machine_name = "Test",
      period_start = inizio_giorno(evaluation_day) + hours(10),
      period_end = period_start + hours(1), generated_at = Sys.time()) |>
    raggruppa_segnalazioni()
  # Il 29 e' una giornata senza segnalazioni: deve essere comunque consolidata.
  if (evaluation_day == as.Date("2026-10-29")) out[FALSE, ] else out
}
alerts_output_path <- tempfile(fileext = ".rds")
sensor_count_increment <- raw_data <- raw
clock <- as.POSIXct("2026-10-29 07:00:00", tz = "Europe/Rome")
Sys.time <- function() clock
aggiorna_segnalazioni()
first_rows <- state$rows
first_attempts <- state$attempts
aggiorna_segnalazioni()
stopifnot(identical(state$rows, first_rows), identical(state$attempts, first_attempts),
          state$schema_changes == 4L, length(state$days) == 1L)
# ON CONFLICT non cambia testo, ID o data della segnalazione esistente.
existing <- calcola_segnalazioni_giorno(as.Date("2026-10-28"))
existing$signal_data_text <- "Questo testo non deve sovrascrivere la segnalazione"
inserisci_segnalazioni(state, existing)
stopifnot(identical(state$rows, first_rows))
clock <- as.POSIXct("2026-10-30 06:00:00", tz = "Europe/Rome")
aggiorna_segnalazioni()
stopifnot(length(state$days) == 1L)
clock <- as.POSIXct("2026-11-01 07:00:00", tz = "Europe/Rome")
state$fail_day <- as.Date("2026-10-31")
failed <- tryCatch({ aggiorna_segnalazioni(); FALSE }, error = function(e) TRUE)
stopifnot(failed, max(state$days) == as.Date("2026-10-30"),
          as.Date("2026-10-29") %in% state$days)
state$fail_day <- as.Date(NA)
aggiorna_segnalazioni()
stopifnot(max(state$days) == as.Date("2026-10-31"), length(state$days) == 4L,
          identical(readRDS(alerts_output_path), bind_rows(state$rows)))
unlink(alerts_output_path)
rm(Sys.time)
calcola_segnalazioni_giorno <- real_calcola
cat("OK: job incrementale, migrazione singola, retry senza sovrascrittura, giorni senza alert, recupero interruzioni e rollback.\n")
