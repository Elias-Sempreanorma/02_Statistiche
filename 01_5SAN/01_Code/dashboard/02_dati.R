dati <- readRDS(here("02_Output", "sensor_count_increment.rds")) |>
  mutate(field = if_else(is.na(field) | trimws(field) == "", "(Non specificato)", field))

availability_path <- here("02_Output", "machine_availability_daily.rds")
machine_availability_daily <- if (file.exists(availability_path)) {
  readRDS(availability_path)
} else {
  tibble(coupon = character(), day = as.Date(character()),
         daily_available_hours = double(), daily_gateway_hours = double())
}

# Compatibilita' con dataset generati prima dell'introduzione delle ore aperte.
# Dopo il rerun completo dell'ETL la colonna e' presente realmente.
if (!"daily_open_hours" %in% names(dati)) {
  dati <- dati |>
    mutate(daily_open_hours = 0)
}

# Compatibilita' con dataset precedenti al flag di disconnessione gateway.
if (!"gateway_disconnect_affected" %in% names(dati)) {
  dati <- dati |>
    mutate(gateway_disconnect_affected = FALSE)
}

nota_disconnessione_gateway <-
  ". Il momento di registrazione delle manovre può risentire di un’interruzione delle comunicazioni."

raw_life_data <- readRDS(here("02_Output", "raw_data.rds")) |>
  mutate(timestamp = with_tz(timestamp, "Europe/Rome"))

# Uniforma anche i testi degli alert gia' salvati, senza attendere il prossimo ETL.
testo_manovre <- function(x) {
  # Uniforma anche i testi storici gia' persistiti, senza riscrivere il DB.
  x <- gsub("[[:space:]]*P5[[:space:]]*[-–][[:space:]]*P95", "", x)
  stringr::str_replace_all(x, c(
    "\\bAttivazioni\\b" = "Manovre",
    "\\battivazioni\\b" = "manovre",
    "\\bATTIVAZIONI\\b" = "MANOVRE",
    "\\bAttivazione\\b" = "Manovra",
    "\\battivazione\\b" = "manovra",
    "\\bSensori\\b" = "Componenti",
    "\\bsensori\\b" = "componenti",
    "\\bSENSORI\\b" = "COMPONENTI",
    "\\bSensore\\b" = "Componente",
    "\\bsensore\\b" = "componente",
    "\\bSENSORE\\b" = "COMPONENTE"
  ))
}

# Conserva date e orari per ordinare tutte le righe prima della paginazione.
# Il formato italiano viene applicato soltanto alla visualizzazione.
tabella_segnalazioni <- function(dati, ordine, righe = 15) {
  tabella <- DT::datatable(
    dati, rownames = FALSE,
    options = list(pageLength = righe, dom = "tip",
      order = list(list(match(ordine, names(dati)) - 1L, "desc")))
  )
  giorni <- names(dati)[vapply(dati, inherits, logical(1), what = "Date")]
  orari <- names(dati)[vapply(dati, inherits, logical(1), what = "POSIXt")]
  if (length(giorni)) {
    tabella <- DT::formatDate(tabella, giorni, "toLocaleDateString",
      params = list("it-IT", list(
        day = "2-digit", month = "2-digit", year = "numeric")))
  }
  if (length(orari)) {
    tabella <- DT::formatDate(tabella, orari, "toLocaleString",
      params = list("it-IT", list(timeZone = "Europe/Rome",
        day = "2-digit", month = "2-digit", year = "numeric",
        hour = "2-digit", minute = "2-digit", second = "2-digit", hour12 = FALSE)))
  }
  tabella
}

alerts_path <- here("02_Output", "alerts.rds")
leggi_alerts_dashboard <- function(path = alerts_path) {
  if (file.exists(path)) {
    saved <- readRDS(path)
    for (column in c("utilization_value", "utilization_lower", "utilization_upper", "utilization_references")) {
      if (!column %in% names(saved)) saved[[column]] <- NA_real_
    }
    saved |>
      filter(!alert_type %in% c("gateway_disconnection", "nok_daily")) |>
      mutate(
        period_start = with_tz(period_start, "Europe/Rome"),
        period_end = with_tz(period_end, "Europe/Rome"),
        generated_at = with_tz(generated_at, "Europe/Rome"),
        across(any_of(c("signal_description", "signal_data_text")), testo_manovre),
        # Compatibilita' con le descrizioni gia' salvate prima della migrazione.
        signal_data_text = gsub("[[:space:]]*\\[[^]\\r\\n]+/Sensor[^]\\r\\n]+\\]", "",
          signal_data_text, perl = TRUE)
      )
  } else {
    tibble(
      alert_id = character(),
      alert_type = character(),
      alert_scope = character(),
      period_start = as.POSIXct(character(), tz = "Europe/Rome"),
      period_end = as.POSIXct(character(), tz = "Europe/Rome"),
      generated_at = as.POSIXct(character(), tz = "Europe/Rome"),
      machine_code = character(),
      machine_serial_number = character(),
      machine_name = character(),
      coupon = character(),
      gateway_id = character(),
      gateway_name = character(),
      sensor_id = character(),
      sensor_code = character(),
      sensor_description = character(),
      functional_unit = character(),
      signal_description = character(),
      signal_data_text = character(),
      source = character()
    )
  }

}

# L'eta' mostrata nella barra T10d e' quella calcolata dall'ETL,
# identica al denominatore del nop usato per il B10dSAN da PFH.
# Dataset precedenti richiedono un'esecuzione dell'ETL: fino ad allora N/D.
if (!"cds_elapsed_years" %in% names(raw_life_data)) {
  raw_life_data$cds_elapsed_years <- NA_real_
}

life_data <- raw_life_data |>
  filter(
    !is.na(coupon),
    !is.na(cds_name),
    !is.na(timestamp)
  ) |>
  arrange(timestamp) |>
  group_by(coupon, cds_name) |>
  summarise(
    cds_vds = last(cds_vds[!is.na(cds_vds)], default = NA_real_),
    cds_t10d = last(cds_t10d[!is.na(cds_t10d)], default = NA_real_),
    count = if (all(is.na(count))) NA_real_ else max(count, na.rm = TRUE),
    offset = if (all(is.na(offset))) NA_real_ else max(offset, na.rm = TRUE),
    lifetime = last(
      cds_elapsed_years[is.finite(cds_elapsed_years) & cds_elapsed_years >= 0],
      default = NA_real_
    ),
    .groups = "drop"
  ) |>
  mutate(count = count + offset) |>
  select(coupon, cds_name, cds_vds, cds_t10d, count, lifetime)

# Anagrafica sensori (coupon + cds_name -> descrizione), usata per
# arricchire le etichette dei serbatoi
sensori_info <- dati |>
  distinct(coupon, cds_name, sensor_description)

