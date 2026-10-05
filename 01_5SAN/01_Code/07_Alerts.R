library(DBI)
library(RPostgres)
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(blastula)
library(jsonlite)

alerts_output_path <- here("02_Output", "alerts.rds")
disconnect_events_path <- here("02_Output", "gateway_disconnect_events.rds")
disconnect_sensor_events_path <- here(
  "02_Output",
  "gateway_disconnect_sensor_events.rds"
)

fmt_iso_utc <- function(x) {
  # Usata anche dentro mutate(): mantiene una stringa per ogni timestamp.
  if (length(x) == 0) return(character())
  result <- format(
    with_tz(as.POSIXct(x), "UTC"),
    "%Y-%m-%dT%H:%M:%OS6Z"
  )
  result[is.na(x)] <- NA_character_
  result
}

fmt_mail_ts <- function(x) {
  if (length(x) == 0 || is.na(x)) return("n.d.")
  format(
    with_tz(as.POSIXct(x), "Europe/Rome"),
    "%d/%m/%Y %H:%M:%S"
  )
}

fmt_num <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("n.d.")
  formatC(x, format = "f", digits = digits, decimal.mark = ",")
}

json_alert <- function(x) {
  # Una stringa ordinaria evita conflitti della classe json in bind_rows().
  as.character(jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  ))
}

inizio_giorno <- function(x) {
  as.POSIXct(as.Date(x), tz = "Europe/Rome")
}

fine_giorno <- function(x) {
  inizio_giorno(x) + days(1) - seconds(1)
}

# ---------------------------------------------------------------------------
# Una sola regola di maturita' per tutti gli alert.
# Il valore 30 NON viene salvato nel DB: e' una regola applicativa.
# ---------------------------------------------------------------------------
sensori_allarmabili <- calcola_sensori_allarmabili(sensor_count_increment)

dati_allarmabili <- sensor_count_increment |>
  semi_join(
    sensori_allarmabili |>
      select(coupon, gateway_id, sensor_id, cds_name, sensor_description),
    by = c(
      "coupon", "gateway_id", "sensor_id",
      "cds_name", "sensor_description"
    )
  )

raw_allarmabile <- raw_data |>
  semi_join(
    sensori_allarmabili |>
      select(coupon, gateway_id, sensor_id),
    by = c("coupon", "gateway_id", "sensor_id")
  )

# ---------------------------------------------------------------------------
# 1) Anomalie giornaliere delle attivazioni.
# ---------------------------------------------------------------------------
base_giornaliera <- prepara_valori_giornalieri_nok_data(dati_allarmabili)

alert_attivazioni_giornaliere <- base_giornaliera |>
  filter(
    outlier_lof,
    is.finite(daily_count)
  ) |>
  mutate(
    period_start = inizio_giorno(day),
    period_end = fine_giorno(day),
    generated_at = period_end,
    event_key = paste(
      "activation_daily",
      coupon,
      sensor_id,
      format(day, "%Y-%m-%d"),
      sep = "|"
    ),
    alert_type = "activation_daily",
    type_code = "ACTD",
    alert_scope = "sensor",
    functional_unit = paste(cds_name, sensor_description, sep = " - "),
    signal_description = if_else(
      daily_count == 0,
      "Zero attivazioni in una giornata con attivita' storica del sensore",
      "Anomalia statistica giornaliera delle attivazioni"
    ),
    signal_data_text = paste0(
      "Attivazioni giornaliere: ",
      round(daily_count),
      if_else(
        is.finite(lof_score),
        paste0("; LOF: ", round(lof_score, 3)),
        ""
      ),
      if_else(
        gateway_disconnect_affected,
        "; dato possibilmente alterato da una disconnessione del gateway",
        ""
      )
    )
  ) |>
  rowwise() |>
  mutate(
    signal_data_json = json_alert(list(
      daily_count = daily_count,
      lof_score = lof_score,
      gateway_disconnect_affected = gateway_disconnect_affected
    ))
  ) |>
  ungroup()

# ---------------------------------------------------------------------------
# 2) Anomalie orarie: >= 4 x mediana oraria positiva del sensore.
# ---------------------------------------------------------------------------
base_oraria <- dati_allarmabili |>
  mutate(hour_alert = floor_date(timestamp, "hour")) |>
  group_by(
    company, field, project, coupon, machine_name,
    machine_serial_number, gateway_id, gateway_name,
    sensor_id, cds_name, sensor_description, hour_alert
  ) |>
  summarise(
    hourly_count = if (any(is.finite(increment))) {
      sum(increment[is.finite(increment)], na.rm = TRUE)
    } else {
      NA_real_
    },
    gateway_disconnect_affected = any(
      gateway_disconnect_affected,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

mediane_orarie <- base_oraria |>
  filter(is.finite(hourly_count), hourly_count > 0) |>
  group_by(coupon, gateway_id, sensor_id, cds_name, sensor_description) |>
  summarise(
    hourly_median = median(hourly_count, na.rm = TRUE),
    .groups = "drop"
  )

alert_attivazioni_orarie <- base_oraria |>
  left_join(
    mediane_orarie,
    by = c(
      "coupon", "gateway_id", "sensor_id",
      "cds_name", "sensor_description"
    )
  ) |>
  filter(
    is.finite(hourly_count),
    is.finite(hourly_median),
    hourly_median > 0,
    hourly_count >= 4 * hourly_median
  ) |>
  mutate(
    period_start = hour_alert,
    period_end = hour_alert + hours(1),
    generated_at = period_end,
    event_key = paste(
      "activation_hourly",
      coupon,
      sensor_id,
      fmt_iso_utc(hour_alert),
      sep = "|"
    ),
    alert_type = "activation_hourly",
    type_code = "ACTH",
    alert_scope = "sensor",
    functional_unit = paste(cds_name, sensor_description, sep = " - "),
    signal_description =
      "Conteggio orario almeno 4 volte la mediana oraria del sensore",
    signal_data_text = paste0(
      "Attivazioni nell'ora: ",
      round(hourly_count),
      "; mediana oraria: ",
      round(hourly_median, 2),
      "; rapporto: ",
      round(hourly_count / hourly_median, 2),
      "x",
      if_else(
        gateway_disconnect_affected,
        "; dato possibilmente alterato da una disconnessione del gateway",
        ""
      )
    )
  ) |>
  rowwise() |>
  mutate(
    signal_data_json = json_alert(list(
      hourly_count = hourly_count,
      hourly_median = hourly_median,
      ratio_to_median = hourly_count / hourly_median,
      gateway_disconnect_affected = gateway_disconnect_affected
    ))
  ) |>
  ungroup()

# ---------------------------------------------------------------------------
# 3) Sensore aperto / assenza di contatto elettrico per oltre 1 ora.
# ---------------------------------------------------------------------------
eventi_aperti <- calcola_eventi_sensore_aperto(
  raw_data,
  sensori_allarmabili = sensori_allarmabili
)

alert_sensori_aperti <- eventi_aperti |>
  mutate(
    period_start = event_start,
    period_end = event_end,
    generated_at = event_end,
    event_key = paste(
      "sensor_open",
      coupon,
      sensor_id,
      fmt_iso_utc(event_start),
      fmt_iso_utc(event_end),
      sep = "|"
    ),
    alert_type = "sensor_open",
    type_code = "OPEN",
    alert_scope = "sensor",
    functional_unit = paste(cds_name, sensor_description, sep = " - "),
    signal_description =
      "Sensore aperto o senza contatto elettrico per oltre 1 ora consecutiva",
    signal_data_text = paste0(
      "Durata evento: ",
      round(open_hours, 2),
      " ore"
    )
  ) |>
  rowwise() |>
  mutate(
    signal_data_json = json_alert(list(
      open_hours = open_hours,
      event_start = fmt_iso_utc(event_start),
      event_end = fmt_iso_utc(event_end)
    ))
  ) |>
  ungroup()

# ---------------------------------------------------------------------------
# 4) NOK giornaliero fuori da media storica +/- 3 sigma.
#
# Per rendere l'alert persistente e indipendente dal periodo scelto in UI,
# ogni giornata viene confrontata con lo storico pulito dello stesso sensore
# escludendo la giornata candidata dal proprio riferimento.
# ---------------------------------------------------------------------------
base_nok <- base_giornaliera |>
  group_by(coupon, gateway_id, sensor_id, cds_name, sensor_description) |>
  mutate(
    clean_for_history = !outlier_lof & is.finite(daily_value),
    n_clean = sum(clean_for_history),
    sum_clean = sum(
      if_else(clean_for_history, daily_value, 0),
      na.rm = TRUE
    ),
    sumsq_clean = sum(
      if_else(clean_for_history, daily_value^2, 0),
      na.rm = TRUE
    ),
    exclude_self = clean_for_history,
    n_hist = n_clean - as.integer(exclude_self),
    sum_hist = sum_clean - if_else(exclude_self, daily_value, 0),
    sumsq_hist =
      sumsq_clean - if_else(exclude_self, daily_value^2, 0),
    NMN = if_else(
      n_hist > 0,
      sum_hist / n_hist,
      NA_real_
    ),
    var_daily_value = if_else(
      n_hist >= 2,
      pmax(
        (
          sumsq_hist - (sum_hist^2 / n_hist)
        ) / (n_hist - 1),
        0
      ),
      NA_real_
    ),
    NOK_giornaliero = if_else(
      is.finite(daily_value) &
        is.finite(NMN) &
        NMN > 0,
      daily_value / NMN,
      NA_real_
    ),
    sigma_nok = if_else(
      is.finite(var_daily_value) &
        is.finite(NMN) &
        NMN > 0,
      sqrt(var_daily_value) / NMN,
      NA_real_
    ),
    limite_nok_inf = pmax(0, 1 - 3 * sigma_nok),
    limite_nok_sup = 1 + 3 * sigma_nok,
    nok_fuori_range = (
      is.finite(NOK_giornaliero) &
      is.finite(limite_nok_inf) &
      is.finite(limite_nok_sup) &
      (
        NOK_giornaliero < limite_nok_inf |
        NOK_giornaliero > limite_nok_sup
      )
    )
  ) |>
  ungroup()

alert_nok <- base_nok |>
  filter(nok_fuori_range) |>
  mutate(
    period_start = inizio_giorno(day),
    period_end = fine_giorno(day),
    generated_at = period_end,
    event_key = paste(
      "nok_daily",
      coupon,
      sensor_id,
      format(day, "%Y-%m-%d"),
      sep = "|"
    ),
    alert_type = "nok_daily",
    type_code = "NOK",
    alert_scope = "sensor",
    functional_unit = paste(cds_name, sensor_description, sep = " - "),
    direction = if_else(
      NOK_giornaliero < limite_nok_inf,
      "sotto limite",
      "sopra limite"
    ),
    signal_description = paste(
      "NOK giornaliero",
      direction,
      "rispetto ai limiti storici +/- 3 sigma"
    ),
    signal_data_text = paste0(
      "NOK: ",
      round(NOK_giornaliero, 3),
      "; limite inferiore: ",
      round(limite_nok_inf, 3),
      "; limite superiore: ",
      round(limite_nok_sup, 3),
      if_else(
        gateway_disconnect_affected,
        "; dato possibilmente alterato da una disconnessione del gateway",
        ""
      )
    )
  ) |>
  rowwise() |>
  mutate(
    signal_data_json = json_alert(list(
      nok = NOK_giornaliero,
      nmn = NMN,
      daily_value = daily_value,
      lower_limit = limite_nok_inf,
      upper_limit = limite_nok_sup,
      direction = direction,
      gateway_disconnect_affected = gateway_disconnect_affected
    ))
  ) |>
  ungroup()

# ---------------------------------------------------------------------------
# 5) Disconnessioni gateway.
# Un unico alert di scope gateway. Almeno uno dei sensori che fornisce
# evidenza (incremento > 1) deve essere gia' allarmabile.
# ---------------------------------------------------------------------------
disconnect_events <- if (
  file.exists(disconnect_events_path)
) {
  readRDS(disconnect_events_path)
} else {
  tibble()
}

disconnect_sensor_events <- if (
  file.exists(disconnect_sensor_events_path)
) {
  readRDS(disconnect_sensor_events_path)
} else {
  tibble()
}

alert_gateway <- tibble()

if (
  nrow(disconnect_events) > 0 &&
  nrow(disconnect_sensor_events) > 0
) {
  evidenze_gateway <- disconnect_sensor_events |>
    left_join(
      sensori_allarmabili |>
        select(
          coupon, gateway_id, sensor_id,
          cds_name, sensor_description,
          n_osservazioni_conteggio
        ),
      by = c("coupon", "gateway_id", "sensor_id")
    ) |>
    mutate(
      qualifies = !is.na(n_osservazioni_conteggio)
    )

  righe_gateway <- lapply(
    seq_len(nrow(disconnect_events)),
    function(i) {
      ev <- disconnect_events[i, ]

      evidenze <- evidenze_gateway |>
        filter(
          coupon == ev$coupon,
          gateway_id == ev$gateway_id,
          systeminfo_b_ts == ev$systeminfo_b_ts
        )

      evidenze_qualificanti <- evidenze |>
        filter(qualifies)

      if (nrow(evidenze_qualificanti) == 0) {
        return(NULL)
      }

      meta <- raw_data |>
        filter(
          coupon == ev$coupon,
          gateway_id == ev$gateway_id
        ) |>
        arrange(timestamp) |>
        slice_tail(n = 1)

      machine_code <- if (nrow(meta) > 0) meta$project[[1]] else NA_character_
      machine_serial_number <- if (nrow(meta) > 0) {
        meta$machine_serial_number[[1]]
      } else NA_character_
      machine_name <- if (nrow(meta) > 0) meta$machine_name[[1]] else NA_character_

      sensori_gateway <- raw_data |>
        filter(
          coupon == ev$coupon,
          gateway_id == ev$gateway_id
        ) |>
        distinct(cds_name, sensor_description, sensor_id) |>
        arrange(cds_name, sensor_id) |>
        transmute(
          x = paste0(
            cds_name,
            if_else(
              !is.na(sensor_description) &
                nzchar(trimws(sensor_description)),
              paste0(" - ", sensor_description),
              ""
            ),
            " [", sensor_id, "]"
          )
        ) |>
        pull(x) |>
        paste(collapse = "; ")

      evidenze_text <- evidenze_qualificanti |>
        arrange(cds_name, sensor_id) |>
        transmute(
          x = paste0(
            coalesce(cds_name, sensor_id),
            if_else(
              !is.na(sensor_description) &
                nzchar(trimws(sensor_description)),
              paste0(" - ", sensor_description),
              ""
            ),
            ": incremento ",
            accumulated_increment
          )
        ) |>
        pull(x) |>
        paste(collapse = "; ")

      period_start <- if (
        !is.na(ev$gap_start_ts)
      ) ev$gap_start_ts else ev$systeminfo_a_ts

      period_end <- if (
        !is.na(ev$gap_end_ts)
      ) ev$gap_end_ts else ev$systeminfo_b_ts

      generated_at <- if (
        !is.na(ev$first_affected_ts)
      ) ev$first_affected_ts else ev$systeminfo_b_ts

      tibble(
        company = if (nrow(meta) > 0) meta$company[[1]] else NA_character_,
        field = if (nrow(meta) > 0) meta$field[[1]] else NA_character_,
        project = machine_code,
        coupon = ev$coupon,
        machine_name = machine_name,
        machine_serial_number = machine_serial_number,
        gateway_id = ev$gateway_id,
        gateway_name = ev$gateway_name,
        sensor_id = NA_character_,
        cds_name = NA_character_,
        sensor_description = NA_character_,
        period_start = period_start,
        period_end = period_end,
        generated_at = generated_at,
        event_key = paste(
          "gateway_disconnection",
          ev$gateway_id,
          fmt_iso_utc(ev$systeminfo_b_ts),
          sep = "|"
        ),
        alert_type = "gateway_disconnection",
        type_code = "GWAY",
        alert_scope = "gateway",
        functional_unit = NA_character_,
        signal_description =
          "Possibile disconnessione del gateway con conteggi accumulati durante l'assenza di comunicazione",
        signal_data_text = paste0(
          "Durata intervallo non osservato: ",
          round(ev$gap_minutes, 1),
          " min; sensori con evidenza: ",
          evidenze_text
        ),
        signal_data_json = json_alert(list(
          systeminfo_a_ts = fmt_iso_utc(ev$systeminfo_a_ts),
          systeminfo_b_ts = fmt_iso_utc(ev$systeminfo_b_ts),
          restart_ts = fmt_iso_utc(ev$restart_ts),
          gap_minutes = ev$gap_minutes,
          observed_sensors = ev$observed_sensors,
          evidence_sensors = ev$evidence_sensors,
          max_first_increment = ev$max_first_increment,
          raw_sensor_evidence = ev$sensor_evidence,
          qualifying_sensor_evidence = evidenze_text,
          all_gateway_sensors = sensori_gateway
        ))
      )
    }
  )

  alert_gateway <- bind_rows(righe_gateway)
}

# ---------------------------------------------------------------------------
# Dataset unico. Per gli alert di sensore recupero codice macchina, descrizione
# CdS e metadati gia' presenti nel dataset elaborato.
# ---------------------------------------------------------------------------
colonne_alert <- c(
  "company", "field", "project", "coupon",
  "machine_name", "machine_serial_number",
  "gateway_id", "gateway_name",
  "sensor_id", "cds_name", "sensor_description",
  "period_start", "period_end", "generated_at",
  "event_key", "alert_type", "type_code", "alert_scope",
  "functional_unit", "signal_description",
  "signal_data_text", "signal_data_json"
)

normalizza_alert <- function(df) {
  mancanti <- setdiff(colonne_alert, names(df))
  for (col in mancanti) {
    df[[col]] <- NA
  }
  df |>
    select(all_of(colonne_alert))
}

alerts_all <- bind_rows(
  normalizza_alert(alert_attivazioni_giornaliere),
  normalizza_alert(alert_attivazioni_orarie),
  normalizza_alert(alert_sensori_aperti),
  normalizza_alert(alert_nok),
  normalizza_alert(alert_gateway)
) |>
  distinct(event_key, .keep_all = TRUE) |>
  arrange(generated_at, event_key)

# Questo script viene eseguito tramite source(): on.exit() deve stare
# dentro una funzione, altrimenti puo\' chiudere la connessione nel contesto
# di valutazione. La chiusura esplicita resta alla fine dello script.
con_alerts <- connetti_postgres(Sys.getenv("PG_DB_STATS"))

# ---------------------------------------------------------------------------
# La tabella alerts e' un indice derivato dai dati: viene rigenerata ad ogni
# ETL con ID deterministici. Prima salvo solo lo stato operativo delle mail.
# ---------------------------------------------------------------------------
old_email_state <- tibble()

alerts_exists <- dbGetQuery(
  con_alerts,
  "SELECT to_regclass('public.alerts') IS NOT NULL AS exists"
)$exists[[1]]

if (isTRUE(alerts_exists)) {
  old_email_state <- tryCatch(
    dbGetQuery(
      con_alerts,
      "
      SELECT
        event_key,
        email_sent_at,
        email_error,
        email_suppressed
      FROM public.alerts
      "
    ),
    error = function(e) tibble()
  )
}

old_gateway_exists <- dbGetQuery(
  con_alerts,
  "SELECT to_regclass('public.gateway_disconnections') IS NOT NULL AS exists"
)$exists[[1]]

if (isTRUE(old_gateway_exists)) {
  old_gateway_state <- dbGetQuery(
    con_alerts,
    "
    SELECT
      gateway_id,
      systeminfo_b_ts,
      email_sent_at,
      email_error,
      email_suppressed
    FROM public.gateway_disconnections
    "
  ) |>
    mutate(
      event_key = paste(
        "gateway_disconnection",
        gateway_id,
        vapply(systeminfo_b_ts, fmt_iso_utc, character(1)),
        sep = "|"
      )
    ) |>
    select(
      event_key,
      email_sent_at,
      email_error,
      email_suppressed
    )

  old_email_state <- bind_rows(
    old_email_state,
    old_gateway_state
  ) |>
    arrange(event_key) |>
    group_by(event_key) |>
    summarise(
      email_sent_at = {
        x <- email_sent_at[!is.na(email_sent_at)]
        if (length(x) > 0) x[[1]] else as.POSIXct(NA)
      },
      email_error = {
        x <- email_error[!is.na(email_error) & nzchar(email_error)]
        if (length(x) > 0) x[[1]] else NA_character_
      },
      email_suppressed = any(email_suppressed, na.rm = TRUE),
      .groups = "drop"
    )
}

dbExecute(con_alerts, "DROP TABLE IF EXISTS public.alerts")
dbExecute(
  con_alerts,
  "DROP TABLE IF EXISTS public.gateway_disconnection_sensor_events"
)
dbExecute(
  con_alerts,
  "DROP TABLE IF EXISTS public.gateway_disconnections"
)

dbExecute(
  con_alerts,
  "
  CREATE TABLE public.alerts (
    alert_id VARCHAR(40) PRIMARY KEY,
    event_key TEXT NOT NULL UNIQUE,
    alert_type VARCHAR(80) NOT NULL,
    alert_scope VARCHAR(30) NOT NULL,

    period_start TIMESTAMPTZ NOT NULL,
    period_end TIMESTAMPTZ NOT NULL,
    generated_at TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    machine_code VARCHAR(100),
    machine_serial_number VARCHAR(255),
    machine_name VARCHAR(255),
    coupon VARCHAR(100) NOT NULL,

    gateway_id VARCHAR(100),
    gateway_name VARCHAR(255),

    sensor_id VARCHAR(255),
    sensor_code VARCHAR(100),
    sensor_description TEXT,
    functional_unit VARCHAR(255),

    signal_description TEXT NOT NULL,
    signal_data_text TEXT NOT NULL,
    signal_data JSONB NOT NULL DEFAULT '{}'::jsonb,

    source VARCHAR(100) NOT NULL DEFAULT 'MODELLO 5SAN',

    email_sent_at TIMESTAMPTZ,
    email_error TEXT,
    email_suppressed BOOLEAN NOT NULL DEFAULT FALSE,

    CONSTRAINT alerts_period_valid
      CHECK (period_end >= period_start)
  )
  "
)

dbExecute(
  con_alerts,
  "CREATE INDEX alerts_generated_at_idx ON public.alerts (generated_at)"
)
dbExecute(
  con_alerts,
  "CREATE INDEX alerts_coupon_idx ON public.alerts (coupon)"
)
dbExecute(
  con_alerts,
  "CREATE INDEX alerts_type_idx ON public.alerts (alert_type)"
)
dbExecute(
  con_alerts,
  "CREATE INDEX alerts_sensor_idx ON public.alerts (sensor_id)"
)

insert_alert_sql <- "
  INSERT INTO public.alerts (
    alert_id,
    event_key,
    alert_type,
    alert_scope,
    period_start,
    period_end,
    generated_at,
    machine_code,
    machine_serial_number,
    machine_name,
    coupon,
    gateway_id,
    gateway_name,
    sensor_id,
    sensor_code,
    sensor_description,
    functional_unit,
    signal_description,
    signal_data_text,
    signal_data,
    source
  )
  VALUES (
    (
      '5SAN-' || $1 || '-' ||
      to_char($2::timestamptz AT TIME ZONE 'Europe/Rome', 'YYYYMMDDHH24MISS') ||
      '-' || upper(substr(md5($3), 1, 8))
    ),
    $3,
    $4,
    $5,
    $6,
    $7,
    $2,
    $8,
    $9,
    $10,
    $11,
    $12,
    $13,
    $14,
    $15,
    $16,
    $17,
    $18,
    $19,
    $20::jsonb,
    'MODELLO 5SAN'
  )
"

if (nrow(alerts_all) > 0) {
  for (i in seq_len(nrow(alerts_all))) {
    a <- alerts_all[i, ]

    dbExecute(
      con_alerts,
      insert_alert_sql,
      params = list(
        a$type_code,
        a$generated_at,
        a$event_key,
        a$alert_type,
        a$alert_scope,
        a$period_start,
        a$period_end,
        a$project,
        a$machine_serial_number,
        a$machine_name,
        a$coupon,
        a$gateway_id,
        a$gateway_name,
        a$sensor_id,
        a$cds_name,
        a$sensor_description,
        a$functional_unit,
        a$signal_description,
        a$signal_data_text,
        a$signal_data_json
      )
    )
  }
}

# Ripristino stato mail sugli stessi eventi.
if (nrow(old_email_state) > 0) {
  for (i in seq_len(nrow(old_email_state))) {
    e <- old_email_state[i, ]

    dbExecute(
      con_alerts,
      "
      UPDATE public.alerts
      SET
        email_sent_at = $2,
        email_error = $3,
        email_suppressed = $4
      WHERE event_key = $1
      ",
      params = list(
        e$event_key,
        e$email_sent_at,
        e$email_error,
        e$email_suppressed
      )
    )
  }
}

# ---------------------------------------------------------------------------
# Mail per le sole disconnessioni gateway recenti.
# Gli altri alert sono persistiti e visibili in dashboard, ma per ora non
# generano mail automaticamente.
# ---------------------------------------------------------------------------
max_age_hours <- suppressWarnings(
  as.numeric(Sys.getenv("ALERT_EMAIL_MAX_AGE_HOURS", "24"))
)
if (!is.finite(max_age_hours) || max_age_hours <= 0) {
  max_age_hours <- 24
}

pending <- dbGetQuery(
  con_alerts,
  "
  SELECT
    alert_id,
    coupon,
    gateway_id,
    gateway_name,
    period_start,
    period_end,
    generated_at,
    signal_data_text
  FROM public.alerts
  WHERE alert_type = 'gateway_disconnection'
    AND email_sent_at IS NULL
    AND email_suppressed = FALSE
    AND generated_at >=
      NOW() - ($1::double precision * INTERVAL '1 hour')
  ORDER BY generated_at
  ",
  params = list(max_age_hours)
)

smtp_host <- Sys.getenv("SMTP_HOST")
smtp_port <- suppressWarnings(as.integer(Sys.getenv("SMTP_PORT", "587")))
smtp_user <- Sys.getenv("SMTP_USER")
smtp_password <- Sys.getenv("SMTP_PASSWORD")
smtp_use_ssl <- tolower(Sys.getenv("SMTP_USE_SSL", "true")) %in%
  c("1", "true", "yes", "y")

email_to <- Sys.getenv(
  "ALERT_EMAIL_TO",
  "elias.forma@sempreanorma.eu"
)
email_from <- Sys.getenv("ALERT_EMAIL_FROM", smtp_user)

smtp_ready <- all(
  nzchar(smtp_host),
  is.finite(smtp_port),
  nzchar(smtp_user),
  nzchar(smtp_password),
  nzchar(email_from)
)

if (nrow(pending) > 0 && !smtp_ready) {
  message(
    "Alert gateway presenti, ma SMTP non configurato: ",
    "impostare SMTP_HOST, SMTP_PORT, SMTP_USER e SMTP_PASSWORD nel file .env."
  )
}

if (nrow(pending) > 0 && smtp_ready) {
  smtp_credentials <- creds_envvar(
    user = smtp_user,
    pass_envvar = "SMTP_PASSWORD",
    host = smtp_host,
    port = smtp_port,
    use_ssl = smtp_use_ssl
  )

  for (i in seq_len(nrow(pending))) {
    ev <- pending[i, ]

    subject <- paste0(
      "[5SAN] Disconnessione gateway - ",
      ev$alert_id
    )

    body_text <- paste0(
      "Il sistema 5SAN ha generato una segnalazione di possibile ",
      "disconnessione del gateway.\n\n",
      "ID segnalazione: ", ev$alert_id, "\n",
      "Coupon: ", ev$coupon, "\n",
      "Gateway: ",
      ifelse(
        is.na(ev$gateway_name) || ev$gateway_name == "",
        ev$gateway_id,
        ev$gateway_name
      ),
      "\n",
      "Periodo: ",
      fmt_mail_ts(ev$period_start),
      " - ",
      fmt_mail_ts(ev$period_end),
      "\n",
      "Generato: ",
      fmt_mail_ts(ev$generated_at),
      "\n",
      "Dati: ",
      ev$signal_data_text,
      "\n\n",
      "Questa segnalazione e' generata automaticamente dal sistema 5SAN."
    )

    email <- compose_email(body = md(body_text))
    send_error <- NULL

    tryCatch(
      {
        smtp_send(
          email = email,
          to = email_to,
          from = email_from,
          subject = subject,
          credentials = smtp_credentials
        )

        dbExecute(
          con_alerts,
          "
          UPDATE public.alerts
          SET email_sent_at = NOW(),
              email_error = NULL
          WHERE alert_id = $1
          ",
          params = list(ev$alert_id)
        )
      },
      error = function(e) {
        send_error <<- conditionMessage(e)

        dbExecute(
          con_alerts,
          "
          UPDATE public.alerts
          SET email_error = $2
          WHERE alert_id = $1
          ",
          params = list(ev$alert_id, send_error)
        )
      }
    )

    if (!is.null(send_error)) {
      warning(
        "Invio email fallito per alerts.alert_id=",
        ev$alert_id,
        ": ",
        send_error
      )
    }
  }
}

# Mirror leggero per Shiny: la dashboard non interroga il DB ad ogni apertura.
alerts_dashboard <- dbGetQuery(
  con_alerts,
  "
  SELECT
    alert_id,
    alert_type,
    alert_scope,
    period_start,
    period_end,
    generated_at,
    machine_code,
    machine_serial_number,
    machine_name,
    coupon,
    gateway_id,
    gateway_name,
    sensor_id,
    sensor_code,
    sensor_description,
    functional_unit,
    signal_description,
    signal_data_text,
    source
  FROM public.alerts
  ORDER BY generated_at, alert_id
  "
)

saveRDS(alerts_dashboard, alerts_output_path)

dbDisconnect(con_alerts)
message(
  "Tabella public.alerts aggiornata: ",
  nrow(alerts_dashboard),
  " alert persistiti."
)
