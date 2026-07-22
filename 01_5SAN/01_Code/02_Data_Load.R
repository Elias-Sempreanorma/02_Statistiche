library(here)
library(dplyr)
library(readxl)
library(openxlsx)
library(RPostgres)
library(RMariaDB)
library(DBI)
library(purrr)
library(stringr)

# Connessione a DB 5SAN

db <- here("00_Data", "01_DB_Info", "5SAN_connection.Renviron")

con_app <- connetti_postgres(Sys.getenv("PG_DB_APP"))
con_iot <- connetti_postgres(Sys.getenv("PG_DB_IOT"))
con_stats <- connetti_postgres(Sys.getenv("PG_DB_STATS"))

# Carico tabelle
sensor <-  dbGetQuery(
  con_app,
  "SELECT * FROM public.sensor"
)

machine <-  dbGetQuery(
  con_app,
  "SELECT * FROM public.machine"
)

gateway <- dbGetQuery(
  con_app,
  "SELECT * FROM public.gateway"
)

measurements <- dbGetQuery(
  con_iot,
  "SELECT * FROM public.measurements"
)

progetti_componenti_b10d_san <- dbGetQuery(
  con_stats,
  "SELECT * FROM public.progetti_componenti_b10d_san"
)

# merge data da measurements, gateway, machine, sensor
data <- measurements |>
  select(timestamp, sensor_id = name, type, value_string, value_int, value_number, value_bool, thing_id) |>
  inner_join(gateway |> 
      left_join(machine |>
          select(coupon = external_id, id, machine_meta_data) |>
          left_join(sensor |>
              select(machine_id, sensor_description = description, sensor_name = name, sensor_type, sensor_id = external_id), by = c("id" = "machine_id")) |>
          select(coupon, id, machine_meta_data, sensor_name, sensor_description, sensor_type, sensor_id), by = c("machine_id" = "id")) |> 
      select(coupon, gateway_name = name, gateway_description = description, external_id, machine_meta_data, sensor_name, sensor_description, sensor_type, sensor_id), by = c("thing_id" = "external_id", "sensor_id")) |>
  select(coupon, sensor_id, gateway_name, gateway_description, sensor_name, sensor_description, sensor_type, machine_meta_data, timestamp, value_type = type, value_string, value_int, value_number, value_bool)|>
  # creazione dataset base 
  mutate(
    machine_serial_number = str_match(machine_meta_data, '"MachineSerialNumber"\\s*:\\s*"([^"]*)"')[, 2],
    machine_name = str_match(machine_meta_data,'"Name"\\s*:\\s*"([^"]*)"')[, 2]) |>
  select(coupon, machine_name, machine_serial_number, gateway_name, gateway_description, sensor_name, sensor_id, sensor_description, sensor_type, timestamp, value_type, value_string, value_int, value_number, value_bool) |>
  mutate(
    status = as.numeric(str_match(value_string, '"(?:value|status)"\\s*:\\s*(-?\\d+(?:\\.\\d+)?)')[, 2]),
    count  = as.numeric(str_match(value_string, '"count"\\s*:\\s*(-?\\d+(?:\\.\\d+)?)')[, 2]),
    offset = as.numeric(str_match(value_string, '"offset"\\s*:\\s*(-?\\d+(?:\\.\\d+)?)')[, 2])) |>
  select(-c(value_string, value_number, value_int, value_bool))

raw_data <- progetti_componenti_b10d_san |>
  inner_join(data, by = c("cds" = "sensor_name", "coupon"))
  
# salvo i dati in formato R
saveRDS(raw_data, here("02_Output", "raw_data.rds"))
