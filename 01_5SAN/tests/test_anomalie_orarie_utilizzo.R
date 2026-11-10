# Rscript tests/test_anomalie_orarie_utilizzo.R
library(dplyr)
library(lubridate)
source("01_Code/01_Functions.R")
# Ora anomala anche se non si usa alcuna classificazione giornaliera.
x <- tibble(coupon = "C", gateway_id = "G", sensor_id = "S", cds_name = "NC1",
  sensor_description = "Test", day = rep(as.Date("2026-01-01") + 0:13, each = 4),
  hourly_count = c(rep(c(1,2), 27), 1, 400)) |>
  mutate(daily_count = hourly_count, daily_value = hourly_count, daily_uptime = 1)
m <- tibble(coupon = "C", gateway_id = "G", sensor_id = "S",
  rate = 2, size = Inf, family = "poisson", train_end = max(x$day))
r <- classifica_manovre_orarie(x, m)
stopifnot(r$anomalia_oraria[56], r$lof_score[56] > 2,
  r$hourly_count[56] > r$limite_orario_sup[56], !any(r$anomalia_oraria[1:55]))
# Il solo LOF non basta se il conteggio rientra nei limiti probabilistici.
y <- x; y$hourly_count[56] <- 3; y$daily_count <- y$daily_value <- y$hourly_count
stopifnot(!any(classifica_manovre_orarie(y,m)$anomalia_oraria))
# La maturita' resta 7 giorni distinti, non 7 righe orarie.
y <- x; y$day <- as.Date("2026-01-01")
stopifnot(!any(classifica_manovre_orarie(y,m)$anomalia_oraria),
  is.null(stima_modello_manovre(y)))
# U macchina su settimane concluse e confronto solo con settimane precedenti.
d <- tibble(coupon = "C", gateway_id = "G", sensor_id = "S", cds_name = "NC1",
  sensor_description = "Test", day = as.Date("2026-01-05") + 0:55,
  daily_value = c(rep(c(.9,1.1,.9,1.1,.9,1.1,1),7), rep(3,7)), outlier_lof = FALSE)
original <- d
u <- valuta_utilizzo_settimanale(d, as.Date("2026-03-01"))
stopifnot(nrow(u)==1, u$anomalous, u$utilization > u$upper_limit,
  u$reference_weeks==7, identical(d, original),
  nrow(valuta_utilizzo_settimanale(d,as.Date("2026-02-28")))==0)
# Giorni futuri non influenzano soglie e risultato.
future <- mutate(d, day = day + 200, daily_value = 1000)
stopifnot(identical(u,valuta_utilizzo_settimanale(bind_rows(d,future),as.Date("2026-03-01"))))
# Prima di 7 settimane di riferimento non viene emesso un allarme.
short <- valuta_utilizzo_settimanale(filter(d, day >= as.Date("2026-01-12")),
  as.Date("2026-03-01"))
stopifnot(!any(short$anomalous %in% TRUE), all(is.na(short$lower_limit)))
# Con utilizzo abituale il test non segnala anomalie.
d$daily_value[50:56] <- c(.9,1.1,.9,1.1,.9,1.1,1)
stopifnot(!valuta_utilizzo_settimanale(d,as.Date("2026-03-01"))$anomalous)
cat("OK: doppia conferma oraria, maturita', settimane concluse, riferimenti storici e isolamento futuro.\n")

# Stesso valutatore, stesso risultato in grafici e allarmi.
stopifnot(identical(valuta_utilizzo_settimanale(d, as.Date("2026-03-01")),
  valuta_utilizzo_periodo(d, as.Date("2026-02-23"), as.Date("2026-03-01"))))
partial <- valuta_utilizzo_periodo(d, as.Date("2026-02-23"), as.Date("2026-02-25"))
stopifnot(!any(partial$anomalous %in% TRUE), all(is.na(partial$lower_limit)))

# Mesi di calendario: solo il mese concluso, stesso valutatore del grafico.
month_data <- tibble(coupon = "C", gateway_id = "G", sensor_id = "S", cds_name = "NC1",
  sensor_description = "Test", day = seq(as.Date("2025-01-01"), as.Date("2025-08-31"), by = "day")) |>
  mutate(daily_value = if_else(format(day, "%m") == "08", 3,
    if_else(as.integer(day) %% 2L == 0L, .9, 1.1)), outlier_lof = FALSE)
monthly <- valuta_utilizzo_mensile(month_data, as.Date("2025-08-31"))
stopifnot(nrow(monthly) == 1, monthly$anomalous,
  monthly$reference_weeks == 7,
  identical(monthly, valuta_utilizzo_periodo(month_data,
    as.Date("2025-08-01"), as.Date("2025-08-31"), "Mese")),
  nrow(valuta_utilizzo_mensile(month_data, as.Date("2025-08-30"))) == 0)
short_months <- valuta_utilizzo_mensile(filter(month_data, day >= as.Date("2025-02-01")),
  as.Date("2025-08-31"))
stopifnot(!any(short_months$anomalous %in% TRUE), all(is.na(short_months$lower_limit)))
