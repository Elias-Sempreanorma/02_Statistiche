library(dplyr)
library(lubridate)
library(here)

# carica i dati 
raw_data <- readRDS(here("02_Output", "raw_data.rds")) |>
  mutate(field = if_else(is.na(field) | trimws(field) == "", "(Non specificato)", field))
uptime <- readRDS(here("02_Output", "uptime.rds"))

# ---------------------------------------------------------------------------
# Rilevo i periodi in cui un sensore risulta aperto mentre il gateway
# continua a trasmettere.
#
# Un intervallo e' considerato "sensore aperto" quando due misure
# consecutive dello stesso sensore hanno:
# - status = 0 in entrambe le misure;
# - count invariato.
#
# La durata e' quindi almeno la differenza tra i timestamp ricevuti.
# Gli intervalli consecutivi vengono uniti nello stesso evento.
# ---------------------------------------------------------------------------
gruppi_sensore <- c(
  "company", "field", "project", "coupon", "machine_name",
  "gateway_name", "cds_name", "cds_description",
  "cds_brand", "cds_use", "cds_vds", "sensor_description"
)

open_intervals <- raw_data |>
  mutate(
    field = if_else(
      is.na(field) | trimws(field) == "",
      "(Non specificato)",
      field
    ),
    timestamp_local = with_tz(timestamp, "Europe/Rome")
  ) |>
  group_by(across(all_of(gruppi_sensore))) |>
  arrange(timestamp_local, .by_group = TRUE) |>
  mutate(
    previous_timestamp = lag(timestamp_local),
    previous_status = lag(status),
    previous_raw_count = lag(count),
    sensore_aperto_intervallo = (
      !is.na(previous_timestamp) &
      is.finite(status) &
      is.finite(previous_status) &
      status == 0 &
      previous_status == 0 &
      is.finite(count) &
      is.finite(previous_raw_count) &
      count == previous_raw_count
    ),
    nuovo_evento_aperto = (
      sensore_aperto_intervallo &
      !lag(sensore_aperto_intervallo, default = FALSE)
    ),
    open_event_id = cumsum(nuovo_evento_aperto)
  ) |>
  filter(sensore_aperto_intervallo) |>
  transmute(
    across(all_of(gruppi_sensore)),
    open_event_id,
    interval_start = previous_timestamp,
    interval_end = timestamp_local
  ) |>
  ungroup()

sensor_open_events <- open_intervals |>
  group_by(
    across(all_of(gruppi_sensore)),
    open_event_id
  ) |>
  summarise(
    open_start = min(interval_start, na.rm = TRUE),
    open_end = max(interval_end, na.rm = TRUE),
    open_hours = sum(
      as.numeric(
        difftime(
          interval_end,
          interval_start,
          units = "hours"
        )
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    open_date = as.Date(open_start, tz = "Europe/Rome")
  ) |>
  arrange(open_start)

# Suddivido gli intervalli anche per giorno, cosi' il dataset storico
# contiene le ore totali giornaliere in cui ogni sensore e' risultato aperto.
sensor_open_daily <- if (nrow(open_intervals) > 0) {
  open_intervals |>
    rowwise() |>
    mutate(
      open_day = list(
        seq.Date(
          as.Date(interval_start, tz = "Europe/Rome"),
          as.Date(interval_end, tz = "Europe/Rome"),
          by = "day"
        )
      )
    ) |>
    ungroup() |>
    tidyr::unnest(open_day) |>
    mutate(
      day = as.Date(open_day),
      day_start = as.POSIXct(day, tz = "Europe/Rome"),
      day_end = day_start + days(1),
      daily_open_piece_hours = pmax(
        0,
        as.numeric(
          difftime(
            pmin(interval_end, day_end),
            pmax(interval_start, day_start),
            units = "hours"
          )
        )
      )
    ) |>
    group_by(
      across(all_of(gruppi_sensore)),
      day
    ) |>
    summarise(
      daily_open_hours = sum(
        daily_open_piece_hours,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
} else {
  raw_data |>
    slice(0) |>
    transmute(
      across(all_of(gruppi_sensore)),
      day = as.Date(timestamp),
      daily_open_hours = numeric()
    )
}

saveRDS(
  sensor_open_events,
  here("02_Output", "sensor_open_events.rds")
)

# calcola gli incrementi dei conteggi per ogni sensore e li classifico
sensor_count_increment <- raw_data |>
  group_by(company, field, project, coupon, machine_name, gateway_name, cds_name, cds_description, 
           cds_brand, cds_use, cds_vds, sensor_description) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(count_type = case_when(row_number() == 1 ~ "first_count",
                                row_number() == 2 ~ "second_count",
                                TRUE ~ "not_first"),
         previous_count = if_else(row_number() == 1, count, lag(count)),
         increment = case_when(count_type == "first_count" ~ 0, 
                               count_type == "second_count" & previous_count == 0 ~ count - count, 
                               TRUE ~ count - previous_count),
         increment_type = case_when(increment > 0 ~ "increment",
                                    increment == 0 ~ "stable",
                                    increment < 0 ~ "decrease"),
         hour =  floor_date(timestamp, "hour"),
         day = as.Date(timestamp)) |>
  # unisce l'uptime gionaliero
  left_join(uptime, by = c("coupon", "day")) |>
  # storicizza le ore giornaliere in cui il sensore e' risultato aperto
  left_join(
    sensor_open_daily,
    by = c(
      "company", "field", "project", "coupon", "machine_name",
      "gateway_name", "cds_name", "cds_description",
      "cds_brand", "cds_use", "cds_vds",
      "sensor_description", "day"
    )
  ) |>
  mutate(
    daily_open_hours = coalesce(daily_open_hours, 0)
  ) |>
  ungroup()

# cds_colors <- sensor_count_increment |>
#   distinct(cds_name) |>
#   arrange(cds_name) |>
#   mutate(cds_color = hcl.colors(n = n(), palette = "Dark 3"))
# 
# # calcolo la somma oraria per ogni sensore
# sensor_hour_average <- sensor_count_increment |>
#   filter(increment_type != "decrease") |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, cds_name, cds_description, 
#            cds_brand, cds_use, cds_vds, sensor_description, hour) |>
#   summarise(increment_avg = sum(increment),
#             .groups = "drop_last") |>
#   left_join(cds_colors, by = "cds_name")
#   
# # calcolo la somma oraria per ogni macchina
# machine_hour_average <- sensor_count_increment |>
#   filter(increment_type != "decrease") |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, hour) |>
#   summarise(increment_avg = sum(increment),
#             .groups = "drop_last")  
# 
# # calcolo la media giornaliera per ogni sensore
# sensor_day_average <- sensor_count_increment |>
#   mutate(day =  floor_date(hour, "day")) |>
#   filter(increment_type != "decrease") |>
#   left_join(uptime, by = c("coupon", "day")) |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, cds_name, cds_description, 
#            cds_brand, cds_use, cds_vds, sensor_description, day, daily_uptime) |>
#   summarise(
#     increment = sum(increment),
#     increment_avg = round(increment/unique(daily_uptime),3),
#     .groups = "drop_last")
# 
# # calcolo la media giornaliera per ogni macchina
# machine_day_average <- sensor_count_increment |>
#   mutate(day =  floor_date(hour, "day")) |>
#   filter(increment_type != "decrease") |>
#   left_join(uptime, by = c("coupon", "day")) |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, day, daily_uptime) |>
#   summarise(increment_avg = round(sum(increment)/unique(daily_uptime),3),
#             .groups = "drop_last") 
# 
# # calcolo la media settimanale per ogni sensore
# sensor_week_average <- sensor_count_increment |>
#   mutate(week =  floor_date(hour, "week")) |>
#   filter(increment_type != "decrease") |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, cds_name, cds_description, 
#            cds_brand, cds_use, cds_vds, sensor_description, week) |>
#   summarise(increment_avg = round(mean(increment),3),
#             .groups = "drop_last")
# 
# # calcolo la media settimanale per ogni macchina
# machine_week_average <- sensor_count_increment |>
#   mutate(week =  floor_date(hour, "week")) |>
#   filter(increment_type != "decrease") |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, week) |>
#   summarise(increment_avg = round(mean(increment),3),
#             .groups = "drop_last") 
# 
# salvo i dati in formato R
saveRDS(sensor_count_increment, here("02_Output", "sensor_count_increment.rds"))
# saveRDS(sensor_hour_average, here("02_Output", "sensor_hour_average.rds"))
# # e in xlsx
# write.xlsx(sensor_hour_average, here("02_Output", "Attivazioni_Sensori.xlsx"))
# write.xlsx(sensor_day_average, here("02_Output", "Attivazioni_Sensori_giorno.xlsx"))
# write.xlsx(sensor_count_increment, here("02_Output", "Conteggio_Attivazioni_Sensori.xlsx"))
