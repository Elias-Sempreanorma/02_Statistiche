# Eseguire dalla cartella 01_5SAN:
# Rscript 01_Code/Test/test_machine_availability.R
library(dplyr)
library(lubridate)

for (path in c("01_Code/03_Data_Frequency.R", "01_Code/05_Plots.R")) {
  expressions <- parse(path)
  if (grepl("03_Data_Frequency", path)) {
    for (expr in expressions) {
      if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
          identical(expr[[2]], as.name("calcola_disponibilita_macchina"))) eval(expr)
    }
  }
}
ts <- function(x) as.POSIXct(x, tz = "Europe/Rome")
start <- ts("2026-10-19 08:00:00")
ints <- tibble(coupon = "M", gateway_id = "G", start, end = start + 4 * 3600)
readings <- function(sensor, hours, status, gateway = "G") {
  tibble(coupon = "M", gateway_id = gateway, sensor_id = sensor,
         timestamp = start + hours * 3600, status = as.numeric(status))
}
check <- function(raw, intervals, available, gateway_hours = 4) {
  out <- calcola_disponibilita_macchina(raw, intervals)
  stopifnot(isTRUE(all.equal(sum(out$daily_available_hours), available)),
            isTRUE(all.equal(sum(out$daily_gateway_hours), gateway_hours)),
            all(out$daily_available_hours <= out$daily_gateway_hours + 1e-9),
            all(out$daily_available_hours >= 0))
  invisible(out)
}
# Tutti uno, interruzione al primo 0, ripresa solo con tutti a uno.
base <- readings("A", c(0, 1, 2), c(1, 0, 1))
check(bind_rows(base, readings("B", c(0, 1.5, 3), c(1, 0, 1))), ints, 2)
# Misure ripetute non moltiplicano la durata.
check(bind_rows(readings("A", c(0, 1, 2, 3), rep(1, 4)),
                readings("B", 0, 1)), ints, 4)
# Prima misura mancante e status ignoto non autorizzano disponibilita'.
check(bind_rows(readings("A", 0, 1), readings("B", 1, 1)), ints, 3)
check(bind_rows(readings("A", c(0, 1, 2), c(1, NA, 1)),
                readings("B", 0, 1)), ints, 3)
# Duplicati discordanti allo stesso timestamp non devono diventare disponibili.
check(bind_rows(readings("A", c(0, 1, 1, 2), c(1, 1, 0, 1)),
                readings("B", 0, 1)), ints, 3)
# Nessun riuso dello stato precedente allo spegnimento.
split <- bind_rows(ints |> mutate(end = start + 3600),
                   ints |> mutate(start = start + 2 * 3600))
check(bind_rows(readings("A", c(0, 2.5), c(1, 1)),
                readings("B", c(0, 3), c(1, 1))), split, 2, 3)
# Letture fuori uptime escluse, anche se a uno.
check(bind_rows(readings("A", c(-1, 1, 5), c(1, 1, 1)),
                readings("B", 0, 1)), ints, 3)
# Piu' gateway: conta soltanto la loro intersezione e tutti i sensori.
multi <- bind_rows(ints, ints |> mutate(gateway_id = "H", start = start + 3600))
check(bind_rows(readings("A", 0, 1), readings("B", 1, 1, "H")), multi, 3, 3)
# Nessuna lettura valida: zero ore disponibili, uptime conservato.
check(bind_rows(readings("A", 0, NA), readings("B", 0, 1)), ints, 0)
# Spezzatura a mezzanotte e cambio ora legale: giornate locali da 23/25 ore.
for (day in c("2026-03-29", "2026-10-25")) {
  from <- ts(paste(day, "00:00:00"))
  to <- ts(paste(as.Date(day) + 1L, "00:00:00"))
  duration <- as.numeric(difftime(to, from, units = "hours"))
  raw <- readings("A", 0, 1) |> mutate(timestamp = from)
  out <- check(raw, ints |> mutate(start = from, end = to), duration, duration)
  stopifnot(nrow(out) == 1L, out$day == as.Date(day))
}
overnight <- ints |> mutate(start = ts("2026-10-19 23:00:00"),
                           end = ts("2026-10-20 01:00:00"))
out <- check(readings("A", 0, 1) |> mutate(timestamp = overnight$start), overnight, 2, 2)
stopifnot(nrow(out) == 2L, all(out$daily_available_hours == 1))
# Input vuoti.
stopifnot(nrow(calcola_disponibilita_macchina(readings("A", 0, 1)[FALSE, ], ints)) == 0L)
message("Disponibilita macchina: tutti i controlli superati")
