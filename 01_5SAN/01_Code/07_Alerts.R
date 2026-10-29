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
  as.POSIXct(as.character(as.Date(x)), tz = "Europe/Rome")
}

fine_giorno <- function(x) {
  inizio_giorno(x) + days(1) - seconds(1)
}

# Il comportamento e' indipendente dagli intensificatori del testo.
codice_comportamento <- function(type, description) {
  text <- tolower(description)
  case_when(
    type == "sensor_open" ~ "open",
    type %in% c("activation_daily", "activation_hourly", "nok_daily") &
      grepl("meno|inferior|sotto|dimin", text) ~ "decrease",
    type %in% c("activation_daily", "activation_hourly", "nok_daily") &
      grepl("più|piu|superior|sopra|aument", text) ~ "increase",
    TRUE ~ paste0("other:", description)
  )
}

descrizione_comportamento <- function(type, behavior, fallback) {
  case_when(
    type == "activation_hourly" & behavior == "increase" ~ "Più manovre del solito in un'ora",
    type == "activation_hourly" & behavior == "decrease" ~ "Meno manovre del solito in un'ora",
    type == "activation_daily" & behavior == "increase" ~ "Più manovre del solito nella giornata",
    type == "activation_daily" & behavior == "decrease" ~ "Meno manovre del solito nella giornata",
    type == "nok_daily" & behavior == "increase" ~ "NOK giornaliero superiore al valore atteso",
    type == "nok_daily" & behavior == "decrease" ~ "NOK giornaliero inferiore al valore atteso",
    TRUE ~ fallback
  )
}

# Riapre solo i dettagli persistiti: nessuna rivalutazione statistica dello storico.
espandi_segnalazioni_salvate <- function(alerts) {
  if (!nrow(alerts)) return(alerts)
  bind_rows(lapply(seq_len(nrow(alerts)), function(i) {
    a <- alerts[i, , drop = FALSE]
    data <- jsonlite::fromJSON(a$signal_data_json, simplifyVector = FALSE)
    if (is.null(data$sensors)) return(a)
    lines <- strsplit(a$signal_data_text, "\n", fixed = TRUE)[[1]]
    if (length(lines) != length(data$sensors)) {
      stop("Dettagli sensore incoerenti per la segnalazione ", a$alert_id)
    }
    bind_rows(lapply(seq_along(data$sensors), function(j) {
      member <- data$sensors[[j]]
      row <- a
      row$gateway_id <- member$gateway_id
      row$sensor_id <- member$sensor_id
      row$cds_name <- member$sensor_code
      row$sensor_description <- member$sensor_description
      row$functional_unit <- paste(row$cds_name, row$sensor_description, sep = " - ")
      row$alert_scope <- "sensor"
      # I gruppi v2 avevano il prefisso codice/descrizione [gateway/sensore].
      row$signal_data_text <- if (!is.null(member$text)) member$text else {
        sub("^.*?\\]:[[:space:]]*", "", lines[j], perl = TRUE)
      }
      row$signal_data_json <- json_alert(member$data)
      row
    }))
  }))
}

# Raggruppamento persistito: stessa macchina, periodo esatto e comportamento.
# Gateway e sensori restano distinti nel JSON, anche con codici ripetuti.
raggruppa_segnalazioni <- function(alerts) {
  if (!nrow(alerts)) return(alerts)
  sensors <- alerts |> filter(alert_scope %in% c("sensor", "machine"))
  gateways <- alerts |> filter(!alert_scope %in% c("sensor", "machine"))
  if (!nrow(sensors)) return(gateways)
  grouped <- sensors |>
    mutate(behavior_code = codice_comportamento(alert_type, signal_description)) |>
    group_by(coupon, alert_type, period_start, period_end, behavior_code) |>
    group_modify(function(.x, .y) {
      .x <- arrange(.x, gateway_id, sensor_id)
      a <- .x[1, , drop = FALSE]
      labels <- paste(.x$cds_name, .x$sensor_description, sep = " - ")
      a$signal_description <- descrizione_comportamento(
        .y$alert_type, .y$behavior_code, a$signal_description)
      a$event_key <- as.character(jsonlite::toJSON(list(
        version = 3L, coupon = .y$coupon, type = .y$alert_type,
        start = fmt_iso_utc(.y$period_start), end = fmt_iso_utc(.y$period_end),
        behavior = .y$behavior_code
      ), auto_unbox = TRUE))
      a$alert_scope <- if (nrow(.x) > 1L) "machine" else "sensor"
      a$gateway_id <- paste(sort(unique(.x$gateway_id)), collapse = "; ")
      a$gateway_name <- paste(sort(unique(.x$gateway_name)), collapse = "; ")
      a$sensor_id <- paste(unique(.x$sensor_id), collapse = "; ")
      a$cds_name <- paste(unique(.x$cds_name), collapse = "; ")
      a$sensor_description <- paste(unique(.x$sensor_description), collapse = "; ")
      # Evita limiti VARCHAR sui gruppi: la lista completa e' nel JSON/testo.
      a$functional_unit <- if (nrow(.x) > 1L) {
        paste(nrow(.x), "sensori coinvolti")
      } else substr(.x$functional_unit[[1]], 1L, 255L)
      a$signal_data_text <- paste(paste0(labels, ": ", .x$signal_data_text), collapse = "\n")
      a$signal_data_json <- json_alert(list(sensors = lapply(seq_len(nrow(.x)), function(i) {
        list(gateway_id = .x$gateway_id[i], sensor_id = .x$sensor_id[i],
             sensor_code = .x$cds_name[i], sensor_description = .x$sensor_description[i],
             text = .x$signal_data_text[i],
             original_event_key = .x$event_key[i],
             original_alert_id = if ("alert_id" %in% names(.x)) .x$alert_id[i] else NULL,
             data = jsonlite::fromJSON(.x$signal_data_json[i], simplifyVector = FALSE))
      })))
      a$generated_at <- min(.x$generated_at)
      if ("created_at" %in% names(.x)) a$created_at <- min(.x$created_at)
      if ("alert_id" %in% names(.x) && n_distinct(.x$alert_id) > 1L) a$alert_id <- NA_character_
      a
    }) |>
    ungroup()
  bind_rows(select(grouped, -behavior_code), gateways)
}

calcola_segnalazioni_giorno <- function(evaluation_day, incrementi, dati_raw) {
  cutoff <- inizio_giorno(evaluation_day + 1L)
  sensor_count_increment <- incrementi |> filter(timestamp < cutoff)
  raw_data <- dati_raw |> filter(timestamp < cutoff)
  # Nessun giorno successivo entra nel LOF, nella maturita' o nel riferimento.
  giornalieri <- prepara_valori_giornalieri_nok_data(incrementi) |>
    filter(day <= evaluation_day) |>
    group_by(coupon, gateway_id, sensor_id, cds_name, sensor_description) |>
    group_modify(function(.x, .y) {
      # Recupero giorni saltati: il modello corrente puo' essere piu' recente.
      if ("train_end" %in% names(.x) && any(.x$train_end > evaluation_day, na.rm = TRUE)) {
        model <- stima_modello_manovre(.x)
        .x$rate <- if (is.null(model)) NA_real_ else model$rate[[1]]
        .x$size <- if (is.null(model)) NA_real_ else model$size[[1]]
      }
      classifica_anomalie_giornaliere(.x)
    }) |>
    ungroup()
  attr(sensor_count_increment, "giornalieri_nok") <- giornalieri
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
  base_giornaliera <- prepara_valori_giornalieri_nok_data(sensor_count_increment) |>
    semi_join(sensori_allarmabili,
      by = c("coupon", "gateway_id", "sensor_id", "cds_name", "sensor_description")) |>
    group_by(coupon, gateway_id, sensor_id, cds_name, sensor_description) |>
    mutate(
      media_manovre_storica = mean(daily_value[is.finite(daily_value)])
    ) |>
    ungroup()

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
      signal_description = case_when(
        daily_count > limite_manovre_sup ~
          "Più manovre del solito nella giornata",
        daily_count < limite_manovre_inf ~
          "Meno manovre del solito nella giornata",
        TRUE ~ "Numero di manovre diverso dal solito nella giornata"
      ),
      signal_data_text = paste0(
        testo_manovre_giornaliere(
          daily_count, daily_uptime, limite_manovre_inf, limite_manovre_sup
        ),
        if_else(
          gateway_disconnect_affected,
          " Il momento di registrazione delle manovre può risentire di un'interruzione delle comunicazioni.",
          ""
        )
      )
    ) |>
    rowwise() |>
    mutate(
      signal_data_json = json_alert(list(
        daily_count = daily_count,
        daily_uptime = daily_uptime,
        maneuvers_per_hour = daily_value,
        historical_mean_per_hour = media_manovre_storica,
        lof_score = lof_score,
        probability_model = family,
        expected_count = rate * daily_uptime,
        lower_limit_95 = limite_manovre_inf,
        upper_limit_95 = limite_manovre_sup,
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
        "Molte più manovre del solito in un'ora",
      signal_data_text = paste0(
        round(hourly_count), " manovre in un'ora. Nelle ore con attività, il valore abituale è ",
        formatC(hourly_median, format = "f", digits = 1, decimal.mark = ","),
        " manovre.",
        if_else(
          gateway_disconnect_affected,
          " Il momento di registrazione delle manovre può risentire di un'interruzione delle comunicazioni.",
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
  # 3) Componente aperto / assenza di contatto elettrico per oltre 1 ora.
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
        "Componente aperto o senza contatto elettrico per oltre 1 ora consecutiva",
      signal_data_text = paste0(
        "Il componente è rimasto aperto o senza contatto elettrico per ",
        durata_in_testo(open_hours), "."
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
      signal_description = if_else(
        NOK_giornaliero < limite_nok_inf,
        "NOK giornaliero inferiore al valore atteso",
        "NOK giornaliero superiore al valore atteso"
      ),
      signal_data_text = paste0(
        if_else(
          NOK_giornaliero == 0,
          "Nessuna manovra registrata nelle ore di attività, a fronte di un valore atteso maggiore di zero.",
          paste0(
            "Il numero di manovre per ora di attività è pari a circa ",
            trimws(formatC(NOK_giornaliero, format = "fg", digits = 2,
              decimal.mark = ",")),
            " volte il valore atteso in base allo storico del componente."
          )
        ),
        if_else(
          gateway_disconnect_affected,
          " Il momento di registrazione delle manovre può risentire di un'interruzione delle comunicazioni.",
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
  # evidenza (sensore NC con incremento > 2) deve essere gia' allarmabile.
  # ---------------------------------------------------------------------------
  disconnect_events <- if (
    file.exists(disconnect_events_path)
  ) {
    readRDS(disconnect_events_path) |>
      filter(systeminfo_b_ts < cutoff)
  } else {
    tibble()
  }

  disconnect_sensor_events <- if (
    file.exists(disconnect_sensor_events_path)
  ) {
    readRDS(disconnect_sensor_events_path) |>
      filter(systeminfo_b_ts < cutoff)
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
            n_giorni_conteggio
          ),
        by = c("coupon", "gateway_id", "sensor_id")
      ) |>
      mutate(
        qualifies = !is.na(n_giorni_conteggio)
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
              ": ",
              accumulated_increment, " manovre registrate alla ripresa delle comunicazioni"
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

        # La disconnessione diventa rilevabile alla ricezione del messaggio B.
        generated_at <- ev$systeminfo_b_ts

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
            "Possibile interruzione delle comunicazioni con la macchina",
          signal_data_text = paste0(
            ifelse(
              is.finite(ev$gap_minutes),
              paste0("Dati mancanti per ", durata_in_testo(ev$gap_minutes / 60), ". "),
              "Durata dell'interruzione non disponibile. "
            ),
            "Alla ripresa delle comunicazioni: ",
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
    date_columns <- c("period_start", "period_end", "generated_at")
    for (col in mancanti) {
      df[[col]] <- if (col %in% date_columns) {
        rep(as.POSIXct(NA), nrow(df))
      } else rep(NA_character_, nrow(df))
    }
    df |>
      mutate(across(all_of(setdiff(colonne_alert, date_columns)), as.character)) |>
      select(all_of(colonne_alert))
  }

  alerts_all <- bind_rows(
    normalizza_alert(alert_attivazioni_giornaliere),
    normalizza_alert(alert_attivazioni_orarie),
    normalizza_alert(alert_sensori_aperti),
    normalizza_alert(alert_nok),
    normalizza_alert(alert_gateway)
  ) |>
    filter(
      case_when(
        alert_type %in% c("activation_daily", "activation_hourly", "nok_daily") ~
          as.Date(period_start, tz = "Europe/Rome"),
        alert_type == "gateway_disconnection" ~ as.Date(generated_at, tz = "Europe/Rome"),
        TRUE ~ as.Date(period_end, tz = "Europe/Rome")
      ) == evaluation_day
    ) |>
    raggruppa_segnalazioni() |>
    mutate(generated_at = Sys.time()) |>
    arrange(period_start, event_key)

  alerts_all


}

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
    source,
    created_at
  )
  VALUES (
    COALESCE($21::varchar, (
      '5SAN-' || $1 || '-' ||
      to_char($6::timestamptz AT TIME ZONE 'Europe/Rome', 'YYYYMMDDHH24MISS') ||
      '-' || upper(substr(md5($3), 1, 8))
    )),
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
    'MODELLO 5SAN',
    COALESCE($22::timestamptz, NOW())
  )
  ON CONFLICT (event_key) DO NOTHING
"

inserisci_segnalazioni <- function(con_alerts, alerts_all) {
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
          a$signal_data_json,
          if ("alert_id" %in% names(a)) a$alert_id else NA_character_,
          if ("created_at" %in% names(a)) a$created_at else as.POSIXct(NA)
        )
      )
    }
  }

}

aggiorna_segnalazioni <- function() {
  con_alerts <- connetti_postgres(Sys.getenv("PG_DB_STATS"))
  on.exit(dbDisconnect(con_alerts), add = TRUE)
  # Un solo aggiornamento alla volta, anche con ETL sovrapposti.
  locked <- dbGetQuery(con_alerts,
    "SELECT pg_try_advisory_lock(55401, 7) AS locked")$locked[[1]]
  if (!isTRUE(locked)) stop("Aggiornamento segnalazioni gia' in corso; riprovare al prossimo ETL.")
  on.exit(dbExecute(con_alerts, "SELECT pg_advisory_unlock(55401, 7)"), add = TRUE, after = FALSE)
dbExecute(
  con_alerts,
  "
  CREATE TABLE IF NOT EXISTS public.alerts (
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
  "CREATE INDEX IF NOT EXISTS alerts_generated_at_idx ON public.alerts (generated_at)"
)
dbExecute(
  con_alerts,
  "CREATE INDEX IF NOT EXISTS alerts_coupon_idx ON public.alerts (coupon)"
)
dbExecute(
  con_alerts,
  "CREATE INDEX IF NOT EXISTS alerts_type_idx ON public.alerts (alert_type)"
)
dbExecute(
  con_alerts,
  "CREATE INDEX IF NOT EXISTS alerts_sensor_idx ON public.alerts (sensor_id)"
)

dbExecute(con_alerts, "CREATE TABLE IF NOT EXISTS public.alert_evaluation_days (
  evaluation_day DATE PRIMARY KEY, completed_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
)")
dbExecute(con_alerts, "CREATE TABLE IF NOT EXISTS public.alert_migrations (
  version INTEGER PRIMARY KEY, completed_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
)")

  # Migrazione una tantum dei soli alert sensore esistenti.
  # Le disconnessioni e il relativo stato mail restano invariati.
  dbWithTransaction(con_alerts, {
    migrated <- dbGetQuery(con_alerts,
      "SELECT 1 FROM public.alert_migrations WHERE version = 3")
    if (!nrow(migrated)) {
for (column in c("gateway_id", "gateway_name", "sensor_id", "sensor_code")) {
  dbExecute(con_alerts, paste0("ALTER TABLE public.alerts ALTER COLUMN ", column, " TYPE TEXT"))
}

      legacy <- dbGetQuery(con_alerts, "SELECT
        alert_id, created_at,
        machine_code AS project, coupon, machine_name, machine_serial_number,
        gateway_id, gateway_name, sensor_id, sensor_code AS cds_name,
        sensor_description, period_start, period_end, generated_at, event_key,
        alert_type, CASE alert_type
          WHEN 'activation_daily' THEN 'ACTD' WHEN 'activation_hourly' THEN 'ACTH'
          WHEN 'sensor_open' THEN 'OPEN' WHEN 'nok_daily' THEN 'NOK'
        END AS type_code,
        alert_scope, functional_unit, signal_description, signal_data_text,
        signal_data::text AS signal_data_json
        FROM public.alerts WHERE alert_scope IN ('sensor', 'machine')
          AND period_end <= date_trunc('day', NOW() AT TIME ZONE 'Europe/Rome') AT TIME ZONE 'Europe/Rome'")
      grouped <- raggruppa_segnalazioni(espandi_segnalazioni_salvate(legacy))
      dbExecute(con_alerts, "DELETE FROM public.alerts WHERE alert_scope IN ('sensor', 'machine')")
      inserisci_segnalazioni(con_alerts, grouped)
      dbExecute(con_alerts, "INSERT INTO public.alert_migrations(version) VALUES (3)")
    }
  })

  # La prima esecuzione parte da ieri; poi recupera anche i giorni saltati.
  # Il checkpoint registra anche giornate senza segnalazioni.
  yesterday <- as.Date(Sys.time(), tz = "Europe/Rome") - 1L
  last <- dbGetQuery(con_alerts,
    "SELECT MAX(evaluation_day) AS day FROM public.alert_evaluation_days")$day[[1]]
  first <- if (is.na(last)) yesterday else as.Date(last) + 1L
  morning_hour <- suppressWarnings(as.integer(Sys.getenv("ALERT_EVALUATION_HOUR", "7")))
  if (is.na(morning_hour) || morning_hour < 0L || morning_hour > 23L) {
    stop("ALERT_EVALUATION_HOUR deve essere un'ora tra 0 e 23.")
  }
  if (first <= yesterday && hour(with_tz(Sys.time(), "Europe/Rome")) >= morning_hour) {
    for (day_number in seq.int(as.integer(first), as.integer(yesterday))) {
      day <- as.Date(day_number, origin = "1970-01-01")
      dbWithTransaction(con_alerts, {
        alerts <- calcola_segnalazioni_giorno(day, sensor_count_increment, raw_data)
        inserisci_segnalazioni(con_alerts, alerts)
        dbExecute(con_alerts,
          "INSERT INTO public.alert_evaluation_days(evaluation_day) VALUES ($1) ON CONFLICT DO NOTHING",
          params = list(day))
      })
      message("Segnalazioni consolidate per il ", day, ": ", nrow(alerts))
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
    AND period_end >= NOW() - ($1::double precision * INTERVAL '1 hour')
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
      "[5SAN] Possibile interruzione delle comunicazioni - ",
      ev$alert_id
    )

    body_text <- paste0(
      "La macchina potrebbe aver smesso temporaneamente di comunicare con 5SAN.\n\n",
      "ID segnalazione: ", ev$alert_id, "\n",
      "Codice macchina: ", ev$coupon, "\n",
      "Dispositivo di collegamento: ",
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
      "Segnalazione riferita al: ",
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

# Sostituzione atomica: Shiny non puo' leggere un RDS scritto a meta'.
tmp <- tempfile("alerts-", tmpdir = dirname(alerts_output_path))
on.exit(unlink(tmp), add = TRUE)
saveRDS(alerts_dashboard, tmp)
if (!file.rename(tmp, alerts_output_path)) stop("Impossibile aggiornare alerts.rds")

message(
  "Tabella public.alerts aggiornata: ",
  nrow(alerts_dashboard),
  " alert persistiti."
)



}

aggiorna_segnalazioni()
