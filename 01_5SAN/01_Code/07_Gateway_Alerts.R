library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(here)
library(blastula)

events_path <- here("02_Output", "gateway_disconnect_events.rds")
sensor_events_path <- here(
  "02_Output",
  "gateway_disconnect_sensor_events.rds"
)
raw_data_path <- here("02_Output", "raw_data.rds")

MIN_OSSERVAZIONI_ALLARME <- 30L

# ---------------------------------------------------------------------------
# Helper piccoli e deterministici.
# ---------------------------------------------------------------------------
first_non_blank <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) NA_character_ else x[[1]]
}

fmt_iso_utc <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  format(
    with_tz(as.POSIXct(x), "UTC"),
    "%Y-%m-%dT%H:%M:%OS6Z"
  )
}

fmt_ts_mail <- function(x) {
  if (length(x) == 0 || is.na(x)) return("n.d.")
  format(
    with_tz(as.POSIXct(x), "Europe/Rome"),
    "%d/%m/%Y %H:%M:%S"
  )
}

fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("n.d.")
  format(round(x, digits), trim = TRUE, scientific = FALSE)
}

if (!file.exists(events_path)) {
  message("Nessun file gateway_disconnect_events.rds: salto gestione alert gateway.")
} else if (!file.exists(raw_data_path)) {
  warning("raw_data.rds non disponibile: impossibile applicare la regola delle 30 osservazioni agli alert gateway.")
} else {
  gateway_disconnect_events <- readRDS(events_path)

  gateway_disconnect_sensor_events <- if (
    file.exists(sensor_events_path)
  ) {
    readRDS(sensor_events_path)
  } else {
    tibble()
  }

  raw_alerts <- readRDS(raw_data_path)

  # -------------------------------------------------------------------------
  # Regola generale di ammissibilita' all'allarme:
  # almeno 30 osservazioni finite di count per quello specifico sensore.
  # Si calcola una sola volta e viene poi riusata per tutte le disconnessioni.
  # -------------------------------------------------------------------------
  sensor_observation_counts <- raw_alerts |>
    filter(
      !is.na(coupon),
      !is.na(gateway_id),
      !is.na(sensor_id),
      is.finite(count)
    ) |>
    count(
      coupon,
      gateway_id,
      sensor_id,
      name = "n_count_observations"
    )

  sensor_metadata <- raw_alerts |>
    filter(
      !is.na(coupon),
      !is.na(gateway_id),
      !is.na(sensor_id)
    ) |>
    group_by(coupon, gateway_id, sensor_id) |>
    summarise(
      sensor_code = first_non_blank(cds_name),
      sensor_description = first_non_blank(sensor_description),
      .groups = "drop"
    )

  machine_metadata <- raw_alerts |>
    filter(!is.na(coupon)) |>
    group_by(coupon) |>
    summarise(
      machine_code = first_non_blank(project),
      machine_serial_number = first_non_blank(machine_serial_number),
      machine_name = first_non_blank(machine_name),
      .groups = "drop"
    )

  gateway_sensor_list <- raw_alerts |>
    filter(
      !is.na(coupon),
      !is.na(gateway_id),
      !is.na(sensor_id)
    ) |>
    distinct(
      coupon,
      gateway_id,
      sensor_id,
      cds_name,
      sensor_description
    ) |>
    arrange(coupon, gateway_id, cds_name, sensor_id) |>
    group_by(coupon, gateway_id) |>
    summarise(
      all_gateway_sensors = paste(
        paste0(
          coalesce(cds_name, sensor_id),
          if_else(
            !is.na(sensor_description) &
              nzchar(trimws(sensor_description)),
            paste0(" - ", sensor_description),
            ""
          ),
          " [", sensor_id, "]"
        ),
        collapse = "; "
      ),
      .groups = "drop"
    )

  sensor_event_details <- gateway_disconnect_sensor_events |>
    left_join(
      sensor_observation_counts,
      by = c("coupon", "gateway_id", "sensor_id")
    ) |>
    left_join(
      sensor_metadata,
      by = c("coupon", "gateway_id", "sensor_id")
    ) |>
    mutate(
      n_count_observations = coalesce(n_count_observations, 0L),
      qualifies_for_alert =
        n_count_observations >= MIN_OSSERVAZIONI_ALLARME
    )

  con_stats_alerts <- connetti_postgres(Sys.getenv("PG_DB_STATS"))
  on.exit(
    try(dbDisconnect(con_stats_alerts), silent = TRUE),
    add = TRUE
  )

  # -------------------------------------------------------------------------
  # Tabella generale degli alert.
  #
  # Le colonne comuni coprono i campi automatici della sezione A del modello:
  # - alert_id       -> A01
  # - period_*       -> A02
  # - generated_at   -> A03
  # - machine_code   -> A04
  # - functional_unit / sensore -> base per A05
  # - source          -> A07
  #
  # signal_data JSONB contiene invece i dati specifici del tipo di segnale,
  # evitando decine di colonne nullable per alert eterogenei.
  # -------------------------------------------------------------------------
  dbExecute(
    con_stats_alerts,
    "
    CREATE TABLE IF NOT EXISTS public.alerts (
      alert_id BIGSERIAL PRIMARY KEY,
      alert_type VARCHAR(80) NOT NULL,
      alert_scope VARCHAR(30) NOT NULL,
      event_key VARCHAR(300) NOT NULL UNIQUE,

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
      signal_data JSONB NOT NULL DEFAULT '{}'::jsonb,

      source VARCHAR(100) NOT NULL DEFAULT 'MODELLO 5SAN',
      min_observations_required INTEGER NOT NULL DEFAULT 30,
      qualifying_observations INTEGER,

      email_sent_at TIMESTAMPTZ,
      email_error TEXT,
      email_suppressed BOOLEAN NOT NULL DEFAULT FALSE,

      CONSTRAINT alerts_period_valid
        CHECK (period_end >= period_start)
    )
    "
  )

  dbExecute(
    con_stats_alerts,
    "
    CREATE INDEX IF NOT EXISTS alerts_generated_at_idx
      ON public.alerts (generated_at)
    "
  )
  dbExecute(
    con_stats_alerts,
    "
    CREATE INDEX IF NOT EXISTS alerts_coupon_idx
      ON public.alerts (coupon)
    "
  )
  dbExecute(
    con_stats_alerts,
    "
    CREATE INDEX IF NOT EXISTS alerts_type_idx
      ON public.alerts (alert_type)
    "
  )
  dbExecute(
    con_stats_alerts,
    "
    CREATE INDEX IF NOT EXISTS alerts_sensor_idx
      ON public.alerts (sensor_id)
    "
  )

  max_age_hours <- suppressWarnings(
    as.numeric(Sys.getenv("ALERT_EMAIL_MAX_AGE_HOURS", "24"))
  )
  if (!is.finite(max_age_hours) || max_age_hours <= 0) {
    max_age_hours <- 24
  }

  alert_cutoff <- Sys.time() - hours(max_age_hours)

  # -------------------------------------------------------------------------
  # Gateway disconnection -> UN alert di scope gateway.
  #
  # I campi sensore restano NULL perche' l'evento riguarda il gateway nel suo
  # insieme. Tutti i sensori associati e tutte le evidenze restano comunque
  # nel JSON signal_data.
  #
  # L'alert viene creato solo se almeno un sensore che fornisce evidenza
  # (incremento > 1) ha >= 30 osservazioni di count. Le evidenze dei sensori
  # con meno storico non vengono perse: restano in raw_sensor_evidence.
  # -------------------------------------------------------------------------
  if (
    !is.null(gateway_disconnect_events) &&
    nrow(gateway_disconnect_events) > 0
  ) {
    gateway_disconnect_events <- gateway_disconnect_events |>
      arrange(systeminfo_b_ts)

    for (i in seq_len(nrow(gateway_disconnect_events))) {
      ev <- gateway_disconnect_events[i, ]

      details <- sensor_event_details |>
        filter(
          coupon == ev$coupon,
          gateway_id == ev$gateway_id,
          systeminfo_b_ts == ev$systeminfo_b_ts
        )

      qualifying <- details |>
        filter(qualifies_for_alert)

      # Regola dei 30 conteggi: senza almeno un'evidenza matura non esiste alert.
      if (nrow(qualifying) == 0) next

      machine <- machine_metadata |>
        filter(coupon == ev$coupon) |>
        slice_head(n = 1)

      sensors_gateway <- gateway_sensor_list |>
        filter(
          coupon == ev$coupon,
          gateway_id == ev$gateway_id
        ) |>
        slice_head(n = 1)

      machine_code <- if (nrow(machine) > 0) {
        machine$machine_code[[1]]
      } else NA_character_

      machine_serial_number <- if (nrow(machine) > 0) {
        machine$machine_serial_number[[1]]
      } else NA_character_

      machine_name <- if (nrow(machine) > 0) {
        machine$machine_name[[1]]
      } else NA_character_

      all_gateway_sensors <- if (nrow(sensors_gateway) > 0) {
        sensors_gateway$all_gateway_sensors[[1]]
      } else NA_character_

      qualifying_sensor_evidence <- qualifying |>
        arrange(sensor_code, sensor_id) |>
        transmute(
          dettaglio = paste0(
            coalesce(sensor_code, sensor_id),
            if_else(
              !is.na(sensor_description) &
                nzchar(trimws(sensor_description)),
              paste0(" - ", sensor_description),
              ""
            ),
            " [", sensor_id, "]",
            ": incremento=", accumulated_increment,
            ", osservazioni=", n_count_observations,
            ", timestamp=",
            format(
              affected_ts,
              "%Y-%m-%d %H:%M:%S",
              tz = "Europe/Rome"
            )
          )
        ) |>
        pull(dettaglio) |>
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

      event_key <- paste(
        "gateway_disconnection",
        ev$gateway_id,
        fmt_iso_utc(ev$systeminfo_b_ts),
        sep = "|"
      )

      suppress_email <- generated_at < alert_cutoff

      qualifying_observations <- min(
        qualifying$n_count_observations,
        na.rm = TRUE
      )

      dbExecute(
        con_stats_alerts,
        "
        INSERT INTO public.alerts (
          alert_type,
          alert_scope,
          event_key,
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
          signal_data,
          source,
          min_observations_required,
          qualifying_observations,
          email_suppressed
        )
        VALUES (
          'gateway_disconnection',
          'gateway',
          $1,
          $2,
          $3,
          $4,
          $5,
          $6,
          $7,
          $8,
          $9,
          $10,
          NULL,
          NULL,
          NULL,
          NULL,
          $11,
          jsonb_build_object(
            'systeminfo_a_ts', $12::text,
            'systeminfo_b_ts', $13::text,
            'systeminfo_a_uptime_seconds', $14::double precision,
            'systeminfo_b_uptime_seconds', $15::double precision,
            'restart_ts', $16::text,
            'gap_minutes', $17::double precision,
            'first_affected_ts', $18::text,
            'last_affected_ts', $19::text,
            'observed_sensors', $20::integer,
            'evidence_sensors_raw', $21::integer,
            'evidence_sensors_qualified', $22::integer,
            'max_first_increment', $23::double precision,
            'raw_sensor_evidence', $24::text,
            'qualifying_sensor_evidence', $25::text,
            'all_gateway_sensors', $26::text,
            'rule_min_count_observations', $27::integer
          ),
          'MODELLO 5SAN',
          $27,
          $28,
          $29
        )
        ON CONFLICT (event_key)
        DO UPDATE SET
          period_start = EXCLUDED.period_start,
          period_end = EXCLUDED.period_end,
          generated_at = EXCLUDED.generated_at,
          machine_code = EXCLUDED.machine_code,
          machine_serial_number = EXCLUDED.machine_serial_number,
          machine_name = EXCLUDED.machine_name,
          coupon = EXCLUDED.coupon,
          gateway_id = EXCLUDED.gateway_id,
          gateway_name = EXCLUDED.gateway_name,
          signal_description = EXCLUDED.signal_description,
          signal_data = EXCLUDED.signal_data,
          min_observations_required = EXCLUDED.min_observations_required,
          qualifying_observations = EXCLUDED.qualifying_observations,
          email_suppressed =
            public.alerts.email_suppressed OR EXCLUDED.email_suppressed
        ",
        params = list(
          event_key,
          period_start,
          period_end,
          generated_at,
          machine_code,
          machine_serial_number,
          machine_name,
          ev$coupon,
          ev$gateway_id,
          ev$gateway_name,
          paste(
            "Possibile disconnessione del gateway con dati successivi ",
            "potenzialmente accumulati durante l'assenza di comunicazione."
          ),
          fmt_iso_utc(ev$systeminfo_a_ts),
          fmt_iso_utc(ev$systeminfo_b_ts),
          ev$systeminfo_a_uptime_seconds,
          ev$systeminfo_b_uptime_seconds,
          fmt_iso_utc(ev$restart_ts),
          ev$gap_minutes,
          fmt_iso_utc(ev$first_affected_ts),
          fmt_iso_utc(ev$last_affected_ts),
          as.integer(ev$observed_sensors),
          as.integer(ev$evidence_sensors),
          as.integer(nrow(qualifying)),
          ev$max_first_increment,
          ev$sensor_evidence,
          qualifying_sensor_evidence,
          all_gateway_sensors,
          MIN_OSSERVAZIONI_ALLARME,
          as.integer(qualifying_observations),
          suppress_email
        )
      )
    }
  }

  # -------------------------------------------------------------------------
  # Migrazione stato mail dalla vecchia tabella, poi rimozione definitiva.
  # In questo modo un alert recente gia' notificato non viene inviato due volte.
  # -------------------------------------------------------------------------
  old_gateway_table_exists <- dbGetQuery(
    con_stats_alerts,
    "
    SELECT to_regclass('public.gateway_disconnections') IS NOT NULL AS exists
    "
  )$exists[[1]]

  if (isTRUE(old_gateway_table_exists)) {
    dbExecute(
      con_stats_alerts,
      "
      UPDATE public.alerts AS a
      SET
        email_sent_at = COALESCE(a.email_sent_at, g.email_sent_at),
        email_error = COALESCE(a.email_error, g.email_error),
        email_suppressed = a.email_suppressed OR g.email_suppressed
      FROM public.gateway_disconnections AS g
      WHERE a.alert_type = 'gateway_disconnection'
        AND a.gateway_id = g.gateway_id
        AND (
          a.signal_data ->> 'systeminfo_b_ts'
        )::timestamptz = g.systeminfo_b_ts
      "
    )

    dbExecute(
      con_stats_alerts,
      "DROP TABLE public.gateway_disconnections"
    )
  }

  # Residuo della vecchia struttura, se presente.
  dbExecute(
    con_stats_alerts,
    "DROP TABLE IF EXISTS public.gateway_disconnection_sensor_events"
  )

  # -------------------------------------------------------------------------
  # Mail: ora viene gestita direttamente dalla tabella generale alerts.
  # Per il momento sono notificati soltanto gli alert gateway_disconnection.
  # -------------------------------------------------------------------------
  pending <- dbGetQuery(
    con_stats_alerts,
    "
    SELECT
      alert_id,
      coupon,
      gateway_id,
      gateway_name,
      period_start,
      period_end,
      generated_at,
      qualifying_observations,
      signal_data ->> 'restart_ts' AS restart_ts,
      (signal_data ->> 'gap_minutes')::double precision AS gap_minutes,
      (signal_data ->> 'observed_sensors')::integer AS observed_sensors,
      (signal_data ->> 'evidence_sensors_qualified')::integer
        AS evidence_sensors_qualified,
      (signal_data ->> 'max_first_increment')::double precision
        AS max_first_increment,
      signal_data ->> 'qualifying_sensor_evidence'
        AS qualifying_sensor_evidence
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
        "[5SAN] Disconnessione gateway rilevata - ",
        ev$coupon,
        " / ",
        ifelse(
          is.na(ev$gateway_name) || ev$gateway_name == "",
          ev$gateway_id,
          ev$gateway_name
        )
      )

      body_text <- paste0(
        "Il sistema 5SAN ha rilevato una possibile disconnessione del gateway ",
        "supportata da almeno un sensore con incremento dei conteggi superiore ",
        "a 1 e almeno ",
        MIN_OSSERVAZIONI_ALLARME,
        " osservazioni storiche di count.\n\n",
        "ID alert: ", ev$alert_id, "\n",
        "Macchina / coupon: ", ev$coupon, "\n",
        "Gateway: ",
        ifelse(
          is.na(ev$gateway_name) || ev$gateway_name == "",
          ev$gateway_id,
          ev$gateway_name
        ),
        "\n",
        "Gateway ID: ", ev$gateway_id, "\n\n",
        "Periodo non osservato: ",
        fmt_ts_mail(ev$period_start),
        " - ",
        fmt_ts_mail(ev$period_end),
        "\n",
        "Data generazione alert: ",
        fmt_ts_mail(ev$generated_at),
        "\n",
        "Riavvio gateway stimato: ",
        fmt_ts_mail(ev$restart_ts),
        "\n",
        "Durata intervallo non osservato: ",
        fmt_num(ev$gap_minutes),
        " minuti\n\n",
        "Sensori osservabili: ",
        ev$observed_sensors,
        "\n",
        "Sensori qualificanti per l'alert: ",
        ev$evidence_sensors_qualified,
        "\n",
        "Minimo numero di osservazioni tra i sensori qualificanti: ",
        ev$qualifying_observations,
        "\n",
        "Massimo incremento osservato: ",
        fmt_num(ev$max_first_increment, 0),
        "\n",
        "Evidenze: ",
        ifelse(
          is.na(ev$qualifying_sensor_evidence) ||
            ev$qualifying_sensor_evidence == "",
          "n.d.",
          ev$qualifying_sensor_evidence
        ),
        "\n\n",
        "Il tempo mancante non viene aggiunto all'uptime. I conteggi restano ",
        "invariati e i KPI vengono calcolati sui dati disponibili.\n\n",
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
            con_stats_alerts,
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
            con_stats_alerts,
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

  dbDisconnect(con_stats_alerts)
}
