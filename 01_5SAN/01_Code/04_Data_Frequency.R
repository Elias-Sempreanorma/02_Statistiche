library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(stringr)
library(here)

# carico i dati 
raw_data <- readRDS(here("02_Output", "raw_data.rds"))

# stima del tempo di accensione della macchina
uptime <- measurements |>
  filter(
    # name == "SystemInfo",
    timestamp > as.POSIXct("2026-07-27 00:00:00", tz = "Europe/Rome"))|>
  
  select(thing_id, timestamp) |>
  left_join(gateway, by = c("thing_id" = "external_id")) |>
  select(machine_id,, timestamp) |>
  left_join(machine, by = c("machine_id" = "id")) |>
  select(coupon = external_id, timestamp) |>
  mutate(hour = floor_date(timestamp, "hour"),
         day = as.Date(timestamp)) |>
  group_by(coupon, day) |>
  arrange(timestamp, .by_group = TRUE) |>
  summarise(up_time =  as.numeric(difftime(last(hour) + hours(1), first(hour), units = "hours")),
            .groups = "drop") |>
  mutate(daily_uptime = case_when(up_time == 0 ~ 1,
                             up_time > 24 ~ 24,
                             T ~ up_time)) |>
  select(- up_time)
