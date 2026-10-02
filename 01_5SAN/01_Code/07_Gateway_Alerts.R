library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(here)
library(blastula)

events_path <- here("02_Output", "gateway_disconnect_events.rds")

if (!file.exists(events_path)) {
  message("Nessun file gateway_disconnect_events.rds: salto notifiche gateway.")
} else {
  gateway_disconnect_events <- readRDS(events_path)

  con_stats_alerts <- connetti_postgres(Sys.getenv("PG_DB_STATS"))

  # -------------------------------------------------------------------------
  # Tabella eventi. CREATE/ALTER sono idempotenti: lo script puo' girare
  # a ogni ETL senza duplicare strutture.
  # -------------------------------------------------------------------------
  dbExecute(
    con_stats_alerts,
    "
    CREATE TABLE IF NOT EXISTS public.gateway_disconnections (
      id BIGSERIAL PRIMARY KEY,
      coupon VARCHAR(100) NOT NULL,
      gateway_id VARCHAR(100) NOT NULL,
      gateway_name VARCHAR(255),
      systeminfo_a_ts TIMESTAMPTZ NOT NULL,
      systeminfo_b_ts TIMESTAMPTZ NOT NULL,
      systeminfo_a_uptime_seconds DOUBLE PRECISION,
      systeminfo_b_uptime_seconds DOUBLE PRECISION,
      restart_ts TIMESTAMPTZ NOT NULL,
      recovered_start_ts TIMESTAMPTZ,
      recovered_end_ts TIMESTAMPTZ,
      recovered_minutes DOUBLE PRECISION,
      observed_sensors INTEGER,
      evidence_sensors INTEGER,
      max_first_increment DOUBLE PRECISION,
      sensor_evidence TEXT,
      detected_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      email_sent_at TIMESTAMPTZ,
      email_error TEXT,
      email_suppressed BOOLEAN NOT NULL DEFAULT FALSE,
      CONSTRAINT gateway_disconnections_unique
        UNIQUE (gateway_id, systeminfo_b_ts)
    )
    "
  )

  dbExecute(
    con_stats_alerts,
    "ALTER TABLE public.gateway_disconnections ADD COLUMN IF NOT EXISTS recovered_minutes DOUBLE PRECISION"
  )
  dbExecute(
    con_stats_alerts,
    "ALTER TABLE public.gateway_disconnections ADD COLUMN IF NOT EXISTS email_error TEXT"
  )
  dbExecute(
    con_stats_alerts,
    "ALTER TABLE public.gateway_disconnections ADD COLUMN IF NOT EXISTS email_suppressed BOOLEAN NOT NULL DEFAULT FALSE"
  )

  dbExecute(
    con_stats_alerts,
    "
    CREATE UNIQUE INDEX IF NOT EXISTS gateway_disconnections_gateway_systeminfo_b_uidx
      ON public.gateway_disconnections (gateway_id, systeminfo_b_ts)
    "
  )

  # -------------------------------------------------------------------------
  # Inserisco solo gli eventi non ancora presenti.
  # Gli eventi storici vengono salvati nel DB ma non generano una valanga
  # di email al primo deploy.
  # -------------------------------------------------------------------------
  max_age_hours <- suppressWarnings(
    as.numeric(Sys.getenv("ALERT_EMAIL_MAX_AGE_HOURS", "24"))
  )
  if (!is.finite(max_age_hours) || max_age_hours <= 0) {
    max_age_hours <- 24
  }

  alert_cutoff <- Sys.time() - hours(max_age_hours)

  if (!is.null(gateway_disconnect_events) && nrow(gateway_disconnect_events) > 0) {
    gateway_disconnect_events <- gateway_disconnect_events |>
      arrange(systeminfo_b_ts)

    for (i in seq_len(nrow(gateway_disconnect_events))) {
      ev <- gateway_disconnect_events[i, ]

      suppress_email <- ev$systeminfo_b_ts < alert_cutoff

      dbGetQuery(
        con_stats_alerts,
        "
        INSERT INTO public.gateway_disconnections (
          coupon,
          gateway_id,
          gateway_name,
          systeminfo_a_ts,
          systeminfo_b_ts,
          systeminfo_a_uptime_seconds,
          systeminfo_b_uptime_seconds,
          restart_ts,
          recovered_start_ts,
          recovered_end_ts,
          recovered_minutes,
          observed_sensors,
          evidence_sensors,
          max_first_increment,
          sensor_evidence,
          email_suppressed
        )
        VALUES (
          $1, $2, $3, $4, $5, $6, $7, $8,
          $9, $10, $11, $12, $13, $14, $15, $16
        )
        ON CONFLICT (gateway_id, systeminfo_b_ts) DO NOTHING
        RETURNING id
        ",
        params = list(
          ev$coupon,
          ev$gateway_id,
          ev$gateway_name,
          ev$systeminfo_a_ts,
          ev$systeminfo_b_ts,
          ev$systeminfo_a_uptime_seconds,
          ev$systeminfo_b_uptime_seconds,
          ev$restart_ts,
          ev$recovered_start_ts,
          ev$recovered_end_ts,
          ev$recovered_minutes,
          ev$observed_sensors,
          ev$evidence_sensors,
          ev$max_first_increment,
          ev$sensor_evidence,
          suppress_email
        )
      )
    }
  }

  # -------------------------------------------------------------------------
  # Invio mail per eventi recenti non ancora notificati.
  # Se SMTP fallisce, l'evento resta pending e verra' ritentato al prossimo ETL.
  # -------------------------------------------------------------------------
  pending <- dbGetQuery(
    con_stats_alerts,
    "
    SELECT *
    FROM public.gateway_disconnections
    WHERE email_sent_at IS NULL
      AND email_suppressed = FALSE
      AND systeminfo_b_ts >= NOW() - ($1::double precision * INTERVAL '1 hour')
    ORDER BY systeminfo_b_ts
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

  fmt_ts <- function(x) {
    if (is.na(x)) return("n.d.")
    format(
      with_tz(as.POSIXct(x), "Europe/Rome"),
      "%d/%m/%Y %H:%M:%S"
    )
  }

  fmt_num <- function(x, digits = 1) {
    if (is.na(x) || !is.finite(x)) return("n.d.")
    format(round(x, digits), trim = TRUE, scientific = FALSE)
  }

  if (nrow(pending) > 0 && !smtp_ready) {
    message(
      "Gateway disconnections presenti, ma SMTP non configurato: ",
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
        "Il sistema 5SAN ha rilevato una probabile disconnessione del gateway ",
        "mentre la macchina risultava ancora operativa.\n\n",
        "Macchina / coupon: ", ev$coupon, "\n",
        "Gateway: ",
        ifelse(
          is.na(ev$gateway_name) || ev$gateway_name == "",
          ev$gateway_id,
          ev$gateway_name
        ),
        "\n",
        "Gateway ID: ", ev$gateway_id, "\n\n",
        "Ultimo SystemInfo precedente: ", fmt_ts(ev$systeminfo_a_ts), "\n",
        "Primo SystemInfo dopo il reset: ", fmt_ts(ev$systeminfo_b_ts), "\n",
        "Riavvio/riconnessione gateway stimato: ", fmt_ts(ev$restart_ts), "\n\n",
        "L'analisi dei conteggi dei sensori successivi alla riconnessione ",
        "indica che la macchina ha probabilmente continuato a lavorare durante ",
        "l'assenza di comunicazione del gateway.\n\n",
        "Intervallo di funzionamento recuperato: ",
        fmt_ts(ev$recovered_start_ts),
        " - ",
        fmt_ts(ev$recovered_end_ts),
        "\n",
        "Durata recuperata: ",
        fmt_num(ev$recovered_minutes),
        " minuti\n\n",
        "Sensori osservabili: ", ev$observed_sensors, "\n",
        "Sensori con evidenza di disconnessione: ", ev$evidence_sensors, "\n",
        "Massimo incremento osservato: ", fmt_num(ev$max_first_increment, 0), "\n",
        "Evidenze sensori: ",
        ifelse(
          is.na(ev$sensor_evidence) || ev$sensor_evidence == "",
          "n.d.",
          ev$sensor_evidence
        ),
        "\n\n",
        "L'intervallo recuperato e' stato incluso automaticamente nel calcolo ",
        "dell'uptime macchina.\n\n",
        "Questa segnalazione e' generata automaticamente dal sistema 5SAN."
      )

      email <- compose_email(
        body = md(body_text)
      )

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
            UPDATE public.gateway_disconnections
            SET email_sent_at = NOW(),
                email_error = NULL
            WHERE id = $1
            ",
            params = list(ev$id)
          )
        },
        error = function(e) {
          send_error <<- conditionMessage(e)

          dbExecute(
            con_stats_alerts,
            "
            UPDATE public.gateway_disconnections
            SET email_error = $2
            WHERE id = $1
            ",
            params = list(ev$id, send_error)
          )
        }
      )

      if (!is.null(send_error)) {
        warning(
          "Invio email fallito per gateway_disconnections.id=",
          ev$id,
          ": ",
          send_error
        )
      }
    }
  }

  dbDisconnect(con_stats_alerts)
}
