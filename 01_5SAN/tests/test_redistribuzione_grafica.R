# Eseguire dalla cartella 01_5SAN:
# Rscript tests/test_redistribuzione_grafica.R
library(dplyr)
library(lubridate)
# Carica solo la funzione pura: nessun avvio Shiny, DB o ricalcolo anomalie.
exprs <- parse("01_Code/05_Plots.R")
for (expr in exprs) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("redistribuisci_manovre_grafico"))) eval(expr)
}
make <- function(start, end) tibble(
  coupon = "C", gateway_id = "G", sensor_id = "S", cds_name = "NC1",
  sensor_description = "Test", timestamp = as.POSIXct(c(start, end), tz = "Europe/Rome"),
  increment = c(0, 24), gateway_disconnect_affected = c(FALSE, TRUE))
flags <- function(x, abnormal = TRUE) x |>
  slice_tail(n = 1) |>
  transmute(coupon, gateway_id, sensor_id, cds_name, sensor_description,
    day = as.Date(timestamp, tz = "Europe/Rome"), outlier_lof = abnormal)
x <- make("2026-11-01 22:30:00", "2026-11-02 01:30:00")
original <- x
result <- redistribuisci_manovre_grafico(x, flags(x))
stopifnot(identical(x, original), abs(sum(result$attivazioni_orarie) - 24) < 1e-9,
  identical(result$attivazioni_orarie[order(result$hour)], c(4, 8, 8, 4)),
  all(result$redistribuite), n_distinct(result$day) == 2)
# Un accumulo non anomalo resta nel timestamp originale.
r <- redistribuisci_manovre_grafico(x, flags(x, FALSE))
stopifnot(!any(r$redistribuite), r$attivazioni_orarie[r$hour == floor_date(x$timestamp[2], "hour")] == 24)
# Un'anomalia senza disconnessione non viene redistribuita.
y <- mutate(x, gateway_disconnect_affected = FALSE)
stopifnot(!any(redistribuisci_manovre_grafico(y, flags(y))$redistribuite))
# Le chiavi separano gateway e sensori con lo stesso codice componente.
y <- mutate(x, sensor_id = "OTHER", increment = c(0, 10), gateway_disconnect_affected = FALSE)
r <- redistribuisci_manovre_grafico(bind_rows(x, y), flags(x))
stopifnot(abs(sum(r$attivazioni_orarie) - 34) < 1e-9,
  r$attivazioni_orarie[r$hour == floor_date(x$timestamp[2], "hour")] == 14)
# Estremo esatto: nessuna quota viene aggiunta all'ora successiva.
y <- make("2026-11-01 22:00:00", "2026-11-02 01:00:00")
r <- redistribuisci_manovre_grafico(y, flags(y))
stopifnot(sum(r$attivazioni_orarie) == 24, max(r$hour) < y$timestamp[2])
# Ora legale: conserva il totale e distingue le due ore locali ripetute.
y <- make("2026-10-25 00:30:00", "2026-10-25 04:30:00")
r <- redistribuisci_manovre_grafico(y, flags(y))
stopifnot(abs(sum(r$attivazioni_orarie) - 24) < 1e-9,
  n_distinct(as.numeric(r$hour)) == 6)
# Nessun dato e nessuna evidenza mantengono lo schema.
r <- redistribuisci_manovre_grafico(x[FALSE, ], flags(x)[FALSE, ])
stopifnot(nrow(r) == 0, all(c("hour", "attivazioni_orarie", "redistribuite") %in% names(r)))
# Il filtro riguarda soltanto la lettura dashboard, non il file persistito.
for (expr in exprs) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("leggi_alerts_dashboard"))) eval(expr)
}
testo_manovre <- identity
path <- tempfile(fileext = ".rds")
alerts <- tibble(alert_type = c("gateway_disconnection", "sensor_open", "activation_daily"),
  period_start = rep(x$timestamp[1], 3), period_end = rep(x$timestamp[2], 3),
  generated_at = rep(x$timestamp[2], 3), signal_description = "Test", signal_data_text = "Test")
saveRDS(alerts, path)
visible <- leggi_alerts_dashboard(path)
stopifnot(nrow(visible) == 2, !any(visible$alert_type == "gateway_disconnection"),
  identical(readRDS(path), alerts))
unlink(path)
cat("OK: totali, mezzanotte, ora legale, anomalie, isolamento sensori, input immutati e filtro dashboard.\n")
