library(here)
library(dplyr)
library(readxl)
library(openxlsx)
library(RPostgres)
library(RMariaDB)
library(DBI)
library(purrr)
library(stringr)

# ---------------------------------------------------------------------------
# Connessione a DB 5SAN
# ---------------------------------------------------------------------------
db <- here("00_Data", "01_DB_Info", "5SAN_connection.Renviron")
con_app   <- connetti_postgres(Sys.getenv("PG_DB_APP"))
con_iot   <- connetti_postgres(Sys.getenv("PG_DB_IOT"))
con_stats <- connetti_postgres(Sys.getenv("PG_DB_STATS"))

# ---------------------------------------------------------------------------
# Determino da dove ripartire (caricamento incrementale)
# ---------------------------------------------------------------------------
raw_data_path <- here("02_Output", "raw_data.rds")

overlap <- as.difftime(1, units = "hours")
default_start <- as.POSIXct("2026-07-27 00:00:00", tz = "Europe/Rome")

if (file.exists(raw_data_path)) {
  existing_raw_data <- readRDS(raw_data_path)
  
  if (
    !is.null(existing_raw_data) &&
    nrow(existing_raw_data) > 0 &&
    "timestamp" %in% names(existing_raw_data) &&
    any(!is.na(existing_raw_data$timestamp))
  ) {
    start_ts <- max(existing_raw_data$timestamp, na.rm = TRUE) - overlap
  } else {
    existing_raw_data <- NULL
    start_ts <- default_start
  }
  
} else {
  existing_raw_data <- NULL
  start_ts <- default_start
}

# ---------------------------------------------------------------------------
# Carico tabelle anagrafiche (piccole, si possono scaricare intere)
# ---------------------------------------------------------------------------
sensor  <- dbGetQuery(con_app, "SELECT * FROM public.sensor")
machine <- dbGetQuery(con_app, "SELECT * FROM public.machine")
gateway <- dbGetQuery(con_app, "SELECT * FROM public.gateway")

progetti_componenti_b10d_san <- dbGetQuery(
  con_stats,
  "SELECT * FROM public.progetti_componenti_b10d_san"
)

# measurements: filtro nel DB e seleziono solo le colonne usate
# (value_int, value_number, value_bool non venivano mai usate a valle: escluse)
measurements <- dbGetQuery(
  con_iot,
  "
  WITH parsed AS (
    SELECT
      timestamp,
      name,
      type,
      thing_id,
      CASE
        WHEN value_string IS NOT NULL
         AND LEFT(BTRIM(value_string), 1) = '{'
         AND RIGHT(BTRIM(value_string), 1) = '}'
        THEN value_string::jsonb
        ELSE NULL::jsonb
      END AS value_json
    FROM public.measurements
    WHERE timestamp > $1
  )
  SELECT
    timestamp,
    name,
    type,
    thing_id,
    COALESCE(
      NULLIF(value_json ->> 'value', ''),
      NULLIF(value_json ->> 'status', '')
    )::double precision AS status,
    NULLIF(value_json ->> 'count', '')::double precision AS count,
    NULLIF(value_json ->> 'offset', '')::double precision AS offset,
    NULLIF(value_json ->> 'lifetime', '')::double precision AS lifetime
  FROM parsed
  ",
  params = list(start_ts)
)

# ---------------------------------------------------------------------------
# Estrazione machine_name / machine_serial_number UNA SOLA VOLTA
# fatta sulla tabella machine (poche righe), non dopo il join su measurements
# ---------------------------------------------------------------------------
machine <- machine |>
  mutate(
    machine_serial_number = str_match(machine_meta_data, '"MachineSerialNumber"\\s*:\\s*"([^"]*)"')[, 2],
    machine_name = str_match(machine_meta_data, '"Name"\\s*:\\s*"([^"]*)"')[, 2]
  )

# ---------------------------------------------------------------------------
# Controllo univocità delle chiavi di join
# ---------------------------------------------------------------------------
dup_gateway_sensor <- gateway |>
  left_join(machine |> select(id, machine_serial_number, machine_name, machine_meta_data), by = c("machine_id" = "id")) |>
  left_join(sensor |> select(machine_id, sensor_name = name, sensor_description = description, sensor_type, sensor_id = external_id),
            by = c("machine_id" = "machine_id")) |>
  count(external_id, sensor_id) |>
  filter(n > 1)

if (nrow(dup_gateway_sensor) > 0) {
  warning(sprintf(
    "Trovate %d combinazioni duplicate di (thing_id, sensor_id) nella tabella gateway/sensor: il join può moltiplicare righe.",
    nrow(dup_gateway_sensor)
  ))
}

dup_coupon_cds <- progetti_componenti_b10d_san |>
  count(coupon, cds) |>
  filter(n > 1)

if (nrow(dup_coupon_cds) > 0) {
  warning(sprintf(
    "Trovate %d combinazioni duplicate di (coupon, cds) in progetti_componenti_b10d_san: il join può moltiplicare righe.",
    nrow(dup_coupon_cds)
  ))
}

# ---------------------------------------------------------------------------
# Merge measurements + gateway + machine + sensor
# ---------------------------------------------------------------------------
gateway_lookup <- gateway |>
  left_join(
    machine |>
      select(coupon = external_id, id, machine_meta_data, machine_serial_number, machine_name) |>
      left_join(
        sensor |>
          select(machine_id, sensor_description = description, sensor_name = name, sensor_type, sensor_id = external_id),
        by = c("id" = "machine_id")
      ) |>
      select(coupon, id, machine_serial_number, machine_name, sensor_name, sensor_description, sensor_type, sensor_id),
    by = c("machine_id" = "id")
  ) |>
  select(coupon, gateway_name = name, gateway_description = description, external_id,
         machine_serial_number, machine_name, sensor_name, sensor_description, sensor_type, sensor_id)

new_data <- measurements |>
  rename(
    sensor_id = name,
    value_type = type
  ) |>
  inner_join(
    gateway_lookup,
    by = c("thing_id" = "external_id", "sensor_id")
  ) |>
  select(
    coupon, machine_name, machine_serial_number,
    gateway_name, gateway_description,
    sensor_name, sensor_id, sensor_description, sensor_type,
    timestamp, value_type, status, count, offset, lifetime
  )

# ---------------------------------------------------------------------------
# Unisco ai progetti/componenti per cds
# ---------------------------------------------------------------------------
new_raw_data <- progetti_componenti_b10d_san |>
  inner_join(new_data, by = c("cds" = "sensor_name", "coupon")) |>
  select(company = azienda, field = stabilimento, project = progetto, coupon, machine_name, machine_serial_number,
         gateway_name, cds_name = cds, cds_description = descrizione, cds_brand = marca,
         cds_code = codice, cds_use = utilizzo, cds_vds = b10dsan, cds_t10d = 'T10d (anni)',
         sensor_id, sensor_description, sensor_type,
         timestamp, value_type, status, count, offset, lifetime)

# Escludo i componenti di sicurezza non attivi
new_raw_data <- new_raw_data |>
  filter(
    !(project == "C14GR" & cds_name %in% c("MB4", "MB6")),
    !(project == "A3020" & cds_name == "FCM5"),
    !(project == "E11RI" & cds_name == "FCM8")
  )

# ---------------------------------------------------------------------------
# Unisco allo storico (deduplicando l'overlap) e salvo
# ---------------------------------------------------------------------------
if (!is.null(existing_raw_data)) {
  raw_data <- bind_rows(existing_raw_data, new_raw_data) |>
    distinct(coupon, sensor_id, timestamp, .keep_all = TRUE)
} else {
  raw_data <- new_raw_data
}

saveRDS(raw_data, raw_data_path)

# ---------------------------------------------------------------------------
# Disconnetto i DB
# ---------------------------------------------------------------------------
dbDisconnect(con_app)
dbDisconnect(con_iot)
dbDisconnect(con_stats)

# ---------------------------------------------------------------------------
# Nel caso serva aggiornare tutti i dati da 0 runnare
# ---------------------------------------------------------------------------
# saveRDS(NULL, raw_data_path)