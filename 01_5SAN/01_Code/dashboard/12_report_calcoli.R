  # ---------------------------------------------------------------------
  # Report anomalie settimanali/mensili.
  # "Settimana" usa quattro fasce mensili (1-7, 8-14, 15-21, 22-fine mese),
  # cosi' un mese completo genera sempre quattro report.
  # ---------------------------------------------------------------------
  periodi_report <- reactive({
    date_corrente <- date_modal_corrente()
    req(length(date_corrente) == 2, all(!is.na(date_corrente)))
    
    inizio <- as.Date(date_corrente[1])
    fine <- as.Date(date_corrente[2])
    granularita <- if (
      is.null(input$report_granularita) ||
      !nzchar(input$report_granularita)
    ) {
      "Settimana"
    } else {
      input$report_granularita
    }
    
    mesi <- seq.Date(
      lubridate::floor_date(inizio, "month"),
      lubridate::floor_date(fine, "month"),
      by = "month"
    )
    
    periodi <- lapply(mesi, function(mese) {
      fine_mese <- as.Date(lubridate::ceiling_date(mese, "month") - lubridate::days(1))
      
      if (identical(granularita, "Mese")) {
        p_start <- max(inizio, mese)
        p_end <- min(fine, fine_mese)
        if (p_start <= p_end) {
          return(tibble(
            start = p_start,
            end = p_end,
            label = format(mese, "%B %Y")
          ))
        }
        return(NULL)
      }
      
      limiti <- list(
        c(1L, 7L),
        c(8L, 14L),
        c(15L, 21L),
        c(22L, lubridate::days_in_month(mese))
      )
      
      bind_rows(lapply(seq_along(limiti), function(i) {
        p_start <- mese + lubridate::days(limiti[[i]][1] - 1L)
        p_end <- mese + lubridate::days(limiti[[i]][2] - 1L)
        p_start <- max(inizio, as.Date(p_start))
        p_end <- min(fine, as.Date(p_end))
        
        if (p_start > p_end) return(NULL)
        
        tibble(
          start = p_start,
          end = p_end,
          label = paste0(
            "Sett. ", i, " - ",
            format(p_start, "%d/%m"),
            " - ",
            format(p_end, "%d/%m")
          )
        )
      }))
    })
    
    bind_rows(periodi)
  })
  
  output$report_period_buttons <- renderUI({
    periodi <- periodi_report()
    
    if (nrow(periodi) == 0) {
      return(div(class = "alarm-empty", "Nessun periodo disponibile."))
    }
    
    div(
      class = "report-buttons",
      lapply(seq_len(nrow(periodi)), function(i) {
        p <- periodi[i, ]
        tags$button(
          type = "button",
          class = "report-period-btn",
          onclick = sprintf(
            "Shiny.setInputValue('report_periodo_click',{start:'%s',end:'%s',label:'%s',nonce:Date.now()},{priority:'event'});",
            format(p$start, "%Y-%m-%d"),
            format(p$end, "%Y-%m-%d"),
            gsub("'", "\\'", p$label)
          ),
          p$label
        )
      })
    )
  })
  
  calcola_report_periodo <- function(inizio, fine, macchina, sensori) {
    inizio <- as.Date(inizio)
    fine <- as.Date(fine)
    durata <- as.integer(fine - inizio) + 1L
    prev_fine <- inizio - 1L
    prev_inizio <- inizio - durata
    
    macchina_info <- macchine_lookup |>
      filter(coupon == macchina) |>
      slice_head(n = 1)
    
    nome_macchina <- if (nrow(macchina_info) > 0) {
      as.character(macchina_info$machine_name)
    } else {
      macchina
    }
    
    base_giornaliera <- prepara_valori_giornalieri_nok(macchina, sensori)
    
    sensori_allarmabili_report <- sensori_allarmabili |>
      filter(
        coupon == macchina,
        cds_name %in% sensori
      ) |>
      select(cds_name, sensor_description)
    
    # NMN e limiti storici sono calcolati escludendo il periodo del report.
    storico_pulito <- base_giornaliera |>
      filter(
        !outlier_lof,
        day < inizio | day > fine
      )
    
    nmn <- storico_pulito |>
      group_by(cds_name, sensor_description) |>
      summarise(
        NMN = mean(daily_value, na.rm = TRUE),
        .groups = "drop"
      )
    
    storico_nok <- storico_pulito |>
      left_join(nmn, by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK_giornaliero = if_else(
          is.finite(daily_value) & is.finite(NMN) & NMN != 0,
          daily_value / NMN,
          NA_real_
        )
      ) |>
      filter(is.finite(NOK_giornaliero))
    
    limiti_nok <- storico_nok |>
      group_by(cds_name, sensor_description) |>
      summarise(
        media_nok_storico = mean(NOK_giornaliero, na.rm = TRUE),
        sigma_nok_storico = if (n() >= 2) sd(NOK_giornaliero, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) |>
      mutate(
        limite_inf = pmax(0, media_nok_storico - 3 * sigma_nok_storico),
        limite_sup = media_nok_storico + 3 * sigma_nok_storico
      )
    
    metriche_periodo <- function(data_start, data_end) {
      periodo <- base_giornaliera |>
        filter(day >= data_start, day <= data_end)
      
      att <- periodo |>
        group_by(cds_name, sensor_description) |>
        summarise(
          attivazioni_medie = mean(daily_count, na.rm = TRUE),
          gateway_disconnect_affected = any(
            gateway_disconnect_affected,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      nok <- periodo |>
        group_by(cds_name, sensor_description) |>
        summarise(
          NMM = mean(daily_value, na.rm = TRUE),
          .groups = "drop"
        ) |>
        left_join(nmn, by = c("cds_name", "sensor_description")) |>
        mutate(
          NOK = if_else(
            is.finite(NMM) & is.finite(NMN) & NMN != 0,
            NMM / NMN,
            NA_real_
          )
        ) |>
        select(cds_name, sensor_description, NOK)
      
      att |>
        full_join(nok, by = c("cds_name", "sensor_description"))
    }
    
    correnti <- metriche_periodo(inizio, fine) |>
      left_join(
        limiti_nok |>
          select(cds_name, sensor_description, limite_inf, limite_sup),
        by = c("cds_name", "sensor_description")
      )
    
    precedenti <- metriche_periodo(prev_inizio, prev_fine) |>
      rename(
        attivazioni_medie_precedenti = attivazioni_medie,
        NOK_precedente = NOK,
        gateway_disconnect_affected_precedente =
          gateway_disconnect_affected
      )
    
    confronto <- correnti |>
      left_join(
        precedenti,
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        variazione_attivazioni_pct = case_when(
          is.finite(attivazioni_medie_precedenti) &
            attivazioni_medie_precedenti != 0 ~
              100 * (attivazioni_medie - attivazioni_medie_precedenti) /
              abs(attivazioni_medie_precedenti),
          TRUE ~ NA_real_
        ),
        variazione_nok_pct = case_when(
          is.finite(NOK_precedente) & NOK_precedente != 0 ~
            100 * (NOK - NOK_precedente) / abs(NOK_precedente),
          TRUE ~ NA_real_
        ),
        nok_fuori_range = (
          is.finite(NOK) &
          is.finite(limite_inf) &
          is.finite(limite_sup) &
          (NOK < limite_inf | NOK > limite_sup)
        )
      ) |>
      ordina_naturale()
    
    # Metriche macchina.
    att_macchina <- sum(confronto$attivazioni_medie, na.rm = TRUE)
    att_macchina_prev <- sum(confronto$attivazioni_medie_precedenti, na.rm = TRUE)
    att_macchina_pct <- if (
      is.finite(att_macchina_prev) && att_macchina_prev != 0
    ) {
      100 * (att_macchina - att_macchina_prev) / abs(att_macchina_prev)
    } else {
      NA_real_
    }
    
    nok_macchina <- mean(confronto$NOK[is.finite(confronto$NOK)], na.rm = TRUE)
    nok_macchina_prev <- mean(
      confronto$NOK_precedente[is.finite(confronto$NOK_precedente)],
      na.rm = TRUE
    )
    if (!is.finite(nok_macchina)) nok_macchina <- NA_real_
    if (!is.finite(nok_macchina_prev)) nok_macchina_prev <- NA_real_
    
    nok_macchina_pct <- if (
      is.finite(nok_macchina_prev) && nok_macchina_prev != 0
    ) {
      100 * (nok_macchina - nok_macchina_prev) / abs(nok_macchina_prev)
    } else {
      NA_real_
    }
    
    storico_macchina <- storico_nok |>
      group_by(day) |>
      summarise(
        NOK_macchina = mean(NOK_giornaliero, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(is.finite(NOK_macchina))
    
    nok_macchina_media_storica <- if (nrow(storico_macchina) > 0) {
      mean(storico_macchina$NOK_macchina, na.rm = TRUE)
    } else NA_real_
    nok_macchina_sigma <- if (nrow(storico_macchina) >= 2) {
      sd(storico_macchina$NOK_macchina, na.rm = TRUE)
    } else NA_real_
    nok_macchina_inf <- if (is.finite(nok_macchina_media_storica) && is.finite(nok_macchina_sigma)) {
      max(0, nok_macchina_media_storica - 3 * nok_macchina_sigma)
    } else NA_real_
    nok_macchina_sup <- if (is.finite(nok_macchina_media_storica) && is.finite(nok_macchina_sigma)) {
      nok_macchina_media_storica + 3 * nok_macchina_sigma
    } else NA_real_
    nok_macchina_fuori <- (
      is.finite(nok_macchina) &&
      is.finite(nok_macchina_inf) &&
      is.finite(nok_macchina_sup) &&
      (nok_macchina < nok_macchina_inf || nok_macchina > nok_macchina_sup)
    )
    
    calcola_utilizzo <- function(data_start, data_end) {
      dati_u <- base_giornaliera |>
        filter(day >= data_start, day <= data_end) |>
        left_join(nmn, by = c("cds_name", "sensor_description")) |>
        mutate(
          NOK_giornaliero = if_else(
            is.finite(daily_value) & is.finite(NMN) & NMN != 0,
            daily_value / NMN,
            NA_real_
          )
        ) |>
        filter(is.finite(NOK_giornaliero)) |>
        group_by(cds_name, sensor_description) |>
        summarise(
          media_nok = mean(NOK_giornaliero, na.rm = TRUE),
          varianza_nok = if (n() >= 2) var(NOK_giornaliero, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) |>
        mutate(
          U_sensore = sqrt(media_nok^2 + varianza_nok)
        ) |>
        filter(is.finite(U_sensore))
      
      if (nrow(dati_u) == 0) return(NA_real_)
      mean(dati_u$U_sensore, na.rm = TRUE)
    }
    
    utilizzo_corrente <- calcola_utilizzo(inizio, fine)
    utilizzo_precedente <- calcola_utilizzo(prev_inizio, prev_fine)
    utilizzo_pct <- if (
      is.finite(utilizzo_corrente) &&
      is.finite(utilizzo_precedente) &&
      utilizzo_precedente != 0
    ) {
      100 * (utilizzo_corrente - utilizzo_precedente) /
        abs(utilizzo_precedente)
    } else NA_real_
    
    prima_data <- min(base_giornaliera$day, na.rm = TRUE)
    ultima_start <- max(base_giornaliera$day, na.rm = TRUE) - durata + 1L
    
    partenze <- if (
      is.finite(as.numeric(prima_data)) &&
      is.finite(as.numeric(ultima_start)) &&
      ultima_start >= prima_data
    ) {
      seq.Date(prima_data, ultima_start, by = "7 days")
    } else {
      as.Date(character(0))
    }
    
    valori_u_storici <- vapply(
      partenze,
      function(x) calcola_utilizzo(x, x + durata - 1L),
      numeric(1)
    )
    valori_u_storici <- valori_u_storici[is.finite(valori_u_storici)]
    
    shared_assessment <- valuta_utilizzo_periodo(
      prepara_valori_giornalieri_nok_data(dati, macchina), inizio, fine,
      if (durata > 7L) "Mese" else "Settimana")
    p10_u <- p90_u <- NA_real_
    if (nrow(shared_assessment)) {
      utilizzo_corrente <- shared_assessment$utilization[1]
      p10_u <- shared_assessment$lower_limit[1]
      p90_u <- shared_assessment$upper_limit[1]
    }
    snapshot <- leggi_alerts_dashboard() |> filter(coupon == macchina,
      alert_type %in% c("utilization_weekly", "utilization_monthly"),
      as.Date(period_start, tz = "Europe/Rome") == inizio,
      as.Date(period_end, tz = "Europe/Rome") == fine)
    if (nrow(snapshot) && is.finite(snapshot$utilization_value[1])) {
      utilizzo_corrente <- snapshot$utilization_value[1]
      p10_u <- snapshot$utilization_lower[1]
      p90_u <- snapshot$utilization_upper[1]
    }
    utilizzo_stato <- case_when(
      fine >= as.Date(Sys.time(), tz = "Europe/Rome") ~ "In corso",
      !is.finite(utilizzo_corrente) | !is.finite(p10_u) | !is.finite(p90_u) ~ "N/D",
      utilizzo_corrente < p10_u ~ "Basso",
      utilizzo_corrente > p90_u ~ "Elevato", TRUE ~ "Normale")

    # Anomalie attivazioni nel periodo.
    anomalie_giornaliere <- base_giornaliera |>
      group_by(cds_name, sensor_description) |>
      mutate(
        media_manovre_storica = mean(daily_value[is.finite(daily_value)])
      ) |>
      ungroup() |>
      semi_join(
        sensori_allarmabili_report,
        by = c("cds_name", "sensor_description")
      ) |>
      filter(
        outlier_lof,
        day >= inizio,
        day <= fine,
        is.finite(daily_count)
      ) |>
      transmute(
        Macchina = nome_macchina,
        Coupon = macchina,
        Componente = paste(cds_name, sensor_description, sep = " - "),
        Data = day,
        Ora = "-",
        Conteggio = round(daily_count),
        Descrizione = paste0(
          case_when(
            daily_count > limite_manovre_sup ~
              "Più manovre del solito nella giornata",
            daily_count < limite_manovre_inf ~
              "Meno manovre del solito nella giornata",
            TRUE ~ "Numero di manovre diverso dal solito nella giornata"
          ),
          if_else(
            gateway_disconnect_affected,
            nota_disconnessione_gateway,
            ""
          )
        )
      )
    
    anomalie_orarie <- prepara_manovre_orarie(dati) |>
      semi_join(sensori_allarmabili,
        by = c("coupon", "gateway_id", "sensor_id", "cds_name", "sensor_description")) |>
      filter(coupon == macchina, cds_name %in% sensori,
        day >= inizio, day <= fine, anomalia_oraria) |>
      transmute(Macchina = nome_macchina, Coupon = macchina,
        Componente = paste(cds_name, sensor_description, sep = " - "), Data = day,
        Ora = format(hour, "%H:00", tz = "Europe/Rome"), Conteggio = round(hourly_count),
        Descrizione = paste0(if_else(hourly_count > limite_orario_sup,
          "Più manovre del solito in un'ora", "Meno manovre del solito in un'ora"),
          ". LOF e distribuzione dei conteggi orari.",
          if_else(gateway_disconnect_affected, nota_disconnessione_gateway, "")))

    tab_att_anomale <- bind_rows(
      anomalie_giornaliere,
      anomalie_orarie
    ) |>
      distinct() |>
      arrange(Data, Ora, Componente)
    
    tab_nok_anomalo <- tibble()
    tab_utilizzo_anomalo <- leggi_alerts_dashboard() |>
      filter(coupon == macchina, alert_type %in% c("utilization_weekly", "utilization_monthly"),
        as.Date(period_start, tz = "Europe/Rome") <= fine,
        as.Date(period_end, tz = "Europe/Rome") >= inizio) |>
      transmute(Macchina = nome_macchina, Coupon = coupon,
        Inizio = as.Date(period_start, tz = "Europe/Rome"),
        Fine = as.Date(period_end, tz = "Europe/Rome"),
        Descrizione = paste0(signal_description, ". ", signal_data_text))

    list(
      inizio = inizio,
      fine = fine,
      precedente_inizio = prev_inizio,
      precedente_fine = prev_fine,
      macchina = nome_macchina,
      coupon = macchina,
      confronto = confronto,
      att_macchina = att_macchina,
      att_macchina_pct = att_macchina_pct,
      nok_macchina = nok_macchina,
      nok_macchina_pct = nok_macchina_pct,
      nok_macchina_inf = nok_macchina_inf,
      nok_macchina_sup = nok_macchina_sup,
      nok_macchina_fuori = nok_macchina_fuori,
      utilizzo = utilizzo_corrente,
      utilizzo_pct = utilizzo_pct,
      utilizzo_stato = utilizzo_stato,
      p10_utilizzo = p10_u,
      p90_utilizzo = p90_u,
      attivazioni_anomale = tab_att_anomale,
      nok_anomalo = tab_nok_anomalo,
      utilizzo_anomalo = tab_utilizzo_anomalo
    )
  }
  
  report_dati <- reactive({
    periodo <- report_periodo_corrente()
    req(!is.null(periodo))
    
    calcola_report_periodo(
      periodo$start,
      periodo$end,
      macchina_attiva(),
      sensori_modal_correnti()
    )
  })
  
