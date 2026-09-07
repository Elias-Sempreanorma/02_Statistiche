library(dplyr)
library(lubridate)
library(here)

# carico i dati 
raw_data <- readRDS(here("02_Output", "Test", "raw_data.rds"))

# stima del tempo di accensione della macchina e salva
uptime <- raw_data |>
  transmute(
    coupon,
    timestamp = with_tz(timestamp, "Europe/Rome"),
    hour = floor_date(timestamp, "hour"),
    day = as.Date(timestamp, tz = "Europe/Rome")
  ) |>
  filter(!is.na(coupon), !is.na(hour)) |>
  distinct(coupon, day, hour) |>
  group_by(coupon, day) |>
  summarise(
    daily_uptime = as.numeric(
      difftime(max(hour) + hours(1), min(hour), units = "hours")
    ),
    .groups = "drop"
  ) |>
  mutate(daily_uptime = pmin(pmax(daily_uptime, 1), 24))

saveRDS(uptime, here("02_Output", "Test", "uptime.rds"))
