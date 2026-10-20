# Eseguire dalla cartella 01_5SAN: Rscript tests/test_gateway_disconnections.R
library(dplyr)
library(lubridate)
code <- readLines("01_Code/03_Data_Frequency.R")
invisible(parse("01_Code/03_Data_Frequency.R"))
invisible(parse("01_Code/07_Alerts.R"))
counts_start <- grep("^count_events <-", code)
counts_end <- grep("^# Helper: unisce intervalli", code) - 1L
reset_start <- grep("^empty_reset_result <-", code)
reset_end <- grep("^gateway_disconnect_events <-", code) - 1L
run_case <- function(increments, states) {
  t0 <- as.POSIXct("2026-10-19 08:00:00", tz = "Europe/Rome")
  ids <- paste0("S", seq_along(increments))
  progetti_componenti_b10d_san <- tibble(coupon = "C", cds = ids, stato = states)
  raw_data <- bind_rows(lapply(seq_along(increments), function(i) {
    tibble(coupon = "C", gateway_id = "G", gateway_name = "GW",
      sensor_id = ids[i], cds_name = ids[i], timestamp = t0 + minutes(c(30, 90)),
      count = c(10, 10 + increments[i]))
  }))
  reset_candidates <- tibble(coupon = "C", gateway_id = "G", gateway_name = "GW",
    previous_timestamp = t0, timestamp = t0 + hours(3),
    previous_uptime = 10000, uptime = 3600, restart_ts = t0 + hours(2))
  eval(parse(text = code[counts_start:counts_end]))
  eval(parse(text = code[reset_start:reset_end]))
  reset_results[[1]]
}
for (n in 0:2) stopifnot(is.null(run_case(n, "NC")$event))
nc <- run_case(3, "NC")
stopifnot(nc$event$max_first_increment == 3,
          nc$sensor_events$accumulated_increment == 3)
stopifnot(is.null(run_case(100, "NO")$event), is.null(run_case(100, NA_character_)$event))
mixed <- run_case(c(100, 3), c("NO", "NC"))
stopifnot(mixed$event$evidence_sensors == 1,
          identical(mixed$sensor_events$sensor_id, "S2"))
stopifnot(is.null(run_case(c(100, 2), c("NO", "NC"))$event))
cat("OK: incrementi 0/1/2 esclusi, NC > 2 incluso, NO/non specificati esclusi, gateway misti.\n")
