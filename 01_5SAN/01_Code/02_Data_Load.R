library(here)
library(dplyr)
library(readxl)
library(openxlsx)
library(RPostgres)
# library(RMariaDB)
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
system_info_path <- here("02_Output", "system_info.rds")

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

# SystemInfo viene storicizzato separatamente. Al primo deploy lo ricarico
# dall'inizio dello storico, anche se raw_data.rds esiste gia'.
if (file.exists(system_info_path)) {
  existing_system_info <- readRDS(system_info_path)
  
  if (
    !is.null(existing_system_info) &&
    nrow(existing_system_info) > 0 &&
    "timestamp" %in% names(existing_system_info) &&
    any(!is.na(existing_system_info$timestamp))
  ) {
    system_info_start_ts <- max(existing_system_info$timestamp, na.rm = TRUE) - overlap
  } else {
    existing_system_info <- NULL
    system_info_start_ts <- default_start
  }
} else {
  existing_system_info <- NULL
  system_info_start_ts <- default_start
}

# ---------------------------------------------------------------------------
# Carico tabelle anagrafiche (piccole, si possono scaricare intere)
# ---------------------------------------------------------------------------
sensor  <- dbGetQuery(con_app, "SELECT * FROM public.sensor")
machine <- dbGetQuery(con_app, "SELECT * FROM public.machine")
gateway <- dbGetQuery(con_app, "SELECT * FROM public.gateway")
# measurements_check <- dbGetQuery(con_iot, "SELECT * FROM public.measurements")
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
      AND name <> 'SystemInfo'
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

# SystemInfo: tengo solo timestamp, gateway (thing_id) e uptime cumulativo.
system_info_measurements <- dbGetQuery(
  con_iot,
  "
  WITH parsed AS (
    SELECT
      timestamp,
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
      AND name = 'SystemInfo'
  )
  SELECT
    timestamp,
    thing_id,
    NULLIF(value_json ->> 'uptime', '')::double precision AS uptime
  FROM parsed
  ",
  params = list(system_info_start_ts)
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

# Chiave tecnica del gateway: gateway.external_id = measurements.thing_id.
gateway_info <- gateway |>
  left_join(
    machine |> select(id, coupon = external_id),
    by = c("machine_id" = "id")
  ) |>
  transmute(
    coupon,
    gateway_id = external_id,
    gateway_name = name
  ) |>
  distinct()

# Serve solo per migrare raw_data.rds creato prima dell'introduzione di
# gateway_id. Se il nome non e' univoco dentro il coupon, fermo l'ETL.
dup_gateway_name <- gateway_info |>
  count(coupon, gateway_name) |>
  filter(n > 1)

if (nrow(dup_gateway_name) > 0) {
  stop(
    "Gateway name non univoco nello stesso coupon: impossibile ricostruire gateway_id nello storico."
  )
}

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
  select(coupon, gateway_name = name, gateway_description = description, gateway_id = external_id,
         machine_serial_number, machine_name, sensor_name, sensor_description, sensor_type, sensor_id)

new_data <- measurements |>
  rename(
    sensor_id = name,
    value_type = type
  ) |>
  inner_join(
    gateway_lookup,
    by = c("thing_id" = "gateway_id", "sensor_id")
  ) |>
  select(
    coupon, machine_name, machine_serial_number,
    gateway_id = thing_id, gateway_name, gateway_description,
    sensor_name, sensor_id, sensor_description, sensor_type,
    timestamp, value_type, status, count, offset, lifetime
  )

# ---------------------------------------------------------------------------
# Unisco ai componenti tramite coupon + cds
# ---------------------------------------------------------------------------
new_raw_data <- progetti_componenti_b10d_san |>
  inner_join(new_data, by = c("cds" = "sensor_name", "coupon")) |>
  select(company = azienda, field = stabilimento, project = progetto, coupon, machine_name, machine_serial_number,
         gateway_id, gateway_name, cds_name = cds, cds_description = descrizione, cds_brand = marca,
         cds_code = codice, cds_use = utilizzo, cds_vds = b10dsan, cds_t10d = 'T10d (anni)',
         sensor_id, sensor_description, sensor_type,
         timestamp, value_type, status, count, offset, lifetime)

# Escludo i componenti di sicurezza non attivi
new_raw_data <- new_raw_data |>
  filter(
    !(coupon == "LIN-SEMP-4354-8784-4143-4591-177" & cds_name %in% c("MB4", "MB6")),
    !(coupon == "LIN-SEMP-3625-7560-4998-4816-219" & cds_name == "FCM5"),
    !(coupon == "LIN-SEMP-3106-8575-8743-0135-225" & cds_name == "FCM8")
  ) |>
  mutate(field = if_else(is.na(field) | trimws(field) == "", "(Non specificato)", field))

# ---------------------------------------------------------------------------
# Storico SystemInfo per singolo gateway
# ---------------------------------------------------------------------------
new_system_info <- system_info_measurements |>
  filter(
    !is.na(thing_id),
    !is.na(timestamp),
    is.finite(uptime),
    uptime >= 0
  ) |>
  inner_join(
    gateway_info,
    by = c("thing_id" = "gateway_id")
  ) |>
  transmute(
    coupon,
    gateway_id = thing_id,
    gateway_name,
    timestamp,
    uptime
  )

if (!is.null(existing_system_info)) {
  system_info <- bind_rows(existing_system_info, new_system_info) |>
    distinct(gateway_id, timestamp, .keep_all = TRUE) |>
    arrange(gateway_id, timestamp)
} else {
  system_info <- new_system_info |>
    distinct(gateway_id, timestamp, .keep_all = TRUE) |>
    arrange(gateway_id, timestamp)
}

saveRDS(system_info, system_info_path)

# ---------------------------------------------------------------------------
# Migrazione dello storico raw_data precedente: aggiungo gateway_id usando
# coupon + gateway_name. Per i nuovi dati gateway_id arriva direttamente
# da measurements.thing_id.
# ---------------------------------------------------------------------------
if (!is.null(existing_raw_data)) {
  if (!"gateway_id" %in% names(existing_raw_data)) {
    existing_raw_data <- existing_raw_data |>
      left_join(gateway_info, by = c("coupon", "gateway_name"))
  } else if (any(is.na(existing_raw_data$gateway_id))) {
    existing_raw_data <- existing_raw_data |>
      left_join(
        gateway_info |>
          rename(gateway_id_lookup = gateway_id),
        by = c("coupon", "gateway_name")
      ) |>
      mutate(gateway_id = coalesce(gateway_id, gateway_id_lookup)) |>
      select(-gateway_id_lookup)
  }
}

# ---------------------------------------------------------------------------
# Unisco allo storico (deduplicando l'overlap) e salvo
# ---------------------------------------------------------------------------
if (!is.null(existing_raw_data)) {
  raw_data <- bind_rows(existing_raw_data, new_raw_data) |>
    distinct(coupon, gateway_id, sensor_id, timestamp, .keep_all = TRUE)
} else {
  raw_data <- new_raw_data
}

# Correzione puntuale: per questo sensore il valore count = 462 e' noto
# come record non valido e viene rimosso a monte da tutto lo storico.
raw_data <- raw_data |>
  filter(
    !(
      coalesce(sensor_id == "Sensor0123000202B4", FALSE) &
      coalesce(count == 462, FALSE)
    )
  )

# Calcolo una sola eta' del componente per coupon + CdS, in anni.
# Il medesimo valore viene salvato per la barra T10d e usato nel nop.
# Se presente uso l'ultimo lifetime valido (secondi, incluso zero);
# altrimenti il tempo dalla prima osservazione fino a questo ETL.
life_reference_ts <- Sys.time()
seconds_per_year <- 365.25 * 24 * 60 * 60
if (!"lifetime" %in% names(raw_data)) {
  raw_data$lifetime <- NA_real_
}

vita_componenti <- raw_data |>
  filter(!is.na(coupon), !is.na(cds_name), !is.na(timestamp)) |>
  arrange(timestamp) |>
  group_by(coupon, cds_name) |>
  summarise(
    lifetime_seconds = last(
      lifetime[is.finite(lifetime) & lifetime >= 0],
      default = NA_real_
    ),
    first_seen = min(timestamp),
    .groups = "drop"
  ) |>
  mutate(
    cds_elapsed_years = coalesce(
      lifetime_seconds,
      pmax(0, as.numeric(difftime(life_reference_ts, first_seen, units = "secs")))
    ) / seconds_per_year
  ) |>
  select(coupon, cds_name, cds_elapsed_years)

# Sostituisco l'eta' salvata dall'ETL precedente, senza colonne duplicate.
raw_data <- raw_data |>
  select(-any_of("cds_elapsed_years")) |>
  left_join(vita_componenti, by = c("coupon", "cds_name"))

# B10dSAN da PFH: numeratore invariato (incrementi positivi del count).
# Il denominatore e' la stessa eta' mostrata nella barra T10d.
nop_sensori <- raw_data |>
  filter(is.finite(count), !is.na(timestamp)) |>
  group_by(coupon, gateway_id, sensor_id) |>
  arrange(timestamp, .by_group = TRUE) |>
  summarise(
    .nop_b10dsan = {
      anni_vita <- last(cds_elapsed_years, default = NA_real_)
      if (is.finite(anni_vita) && anni_vita > 0) {
        sum(pmax(diff(count), 0)) / anni_vita
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )

parametri_b10dsan <- progetti_componenti_b10d_san |>
  transmute(
    coupon,
    cds_name = cds,
    .pfh_b10dsan = as.numeric(
      sub(",", ".", as.character(`B10d/PFH\nProduttore`), fixed = TRUE)
    ),
    .base_b10dsan = as.numeric(
      sub(",", ".", as.character(b10dsan), fixed = TRUE)
    ),
    .divisore_b10dsan = case_when(
      utilizzo == "Normale" ~ 100,
      utilizzo == "Intensivo" ~ 1000,
      utilizzo == "Favorevole" ~ 10,
      TRUE ~ NA_real_
    )
  ) |>
  distinct(coupon, cds_name, .keep_all = TRUE)

raw_data <- raw_data |>
  left_join(parametri_b10dsan, by = c("coupon", "cds_name")) |>
  left_join(nop_sensori, by = c("coupon", "gateway_id", "sensor_id")) |>
  mutate(
    cds_vds = case_when(
      is.finite(.pfh_b10dsan) &
        .pfh_b10dsan > 0 & .pfh_b10dsan < 1 &
        is.finite(.nop_b10dsan) &
        !is.na(.divisore_b10dsan) ~
          (0.1 * .nop_b10dsan) /
            (.pfh_b10dsan * 8760 * .divisore_b10dsan),
      is.finite(.pfh_b10dsan) & .pfh_b10dsan > 0 & .pfh_b10dsan < 1 ~ NA_real_,
      TRUE ~ coalesce(.base_b10dsan, cds_vds)
    )
  ) |>
  select(
    -.pfh_b10dsan, -.base_b10dsan,
    -.divisore_b10dsan, -.nop_b10dsan
  )

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
# saveRDS(NULL, system_info_path)

# 