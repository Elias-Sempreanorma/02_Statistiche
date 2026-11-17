  # Storico del NOK per sensore: i valori giornalieri vengono aggregati
  # nel periodo scelto e confrontati con la media storica (NMN) dello
  # stesso sensore, calcolata su tutto lo storico disponibile.
  kpi_nok_storico_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    valori_giornalieri_modal_nok() |>
      filter(
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      mutate(
        periodo = as.Date(periodo_bucket(day, filtri$granularita))
      ) |>
      group_by(periodo, cds_name, sensor_description) |>
      summarise(
        valore_periodo = mean(daily_value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      left_join(nmn_storico_modal_nok(), by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK_periodo = case_when(
          is.na(valore_periodo) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ valore_periodo / NMN
        ),
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")
      ) |>
      arrange(periodo)
  }) |>
    bindCache(filtri_nok_modal())
  
  # ---------------------------------------------------------------------
  # Utilizzo macchina nel tempo.
  # Il raggruppamento vale solo per questo grafico.
  # ---------------------------------------------------------------------
  nok_temporale_confronto_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    durata_giorni <- as.integer(
      filtri$date[2] - filtri$date[1]
    ) + 1L
    
    finestra_confronto <- periodo_precedente_grafici(
      filtri$date, valori_giornalieri_modal_nok()$day
    )
    periodo_precedente_fine <- finestra_confronto$fine
    periodo_precedente_inizio <- finestra_confronto$inizio
    
    base <- valori_giornalieri_modal_nok() |>

      left_join(
        nmn_storico_modal_nok(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK_giornaliero = case_when(
          is.na(daily_value) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ daily_value / NMN
        ),
        confronto = case_when(
          day >= periodo_precedente_inizio &
            day <= periodo_precedente_fine ~ "Periodo precedente",
          day >= filtri$date[1] &
            day <= filtri$date[2] ~ "Periodo selezionato",
          TRUE ~ NA_character_
        )
      ) |>
      filter(
        !is.na(confronto),
        is.finite(NOK_giornaliero)
      ) |>
      mutate(
        confronto = factor(
          confronto,
          levels = c("Periodo precedente", "Periodo selezionato")
        ),
        periodo = as.Date(
          periodo_bucket(day, filtri$granularita)
        )
      )
    
    per_sensore <- base |>
      group_by(
        confronto,
        periodo,
        cds_name,
        sensor_description
      ) |>
      summarise(
        NOK = mean(NOK_giornaliero, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(is.finite(NOK)) |>
      mutate(
        etichetta_sensore = paste(
          cds_name,
          sensor_description,
          sep = " - "
        ),
        periodo_label = formatta_periodo_label(
          periodo,
          filtri$granularita
        )
      )
    
    media_macchina <- per_sensore |>
      group_by(confronto, periodo) |>
      summarise(
        NOK = mean(NOK, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        etichetta_sensore = "Media macchina",
        periodo_label = formatta_periodo_label(
          periodo,
          filtri$granularita
        )
      )
    
    list(
      sensori = per_sensore,
      media_macchina = media_macchina,
      etichette_confronto = finestra_confronto$etichette
    )
  }) |>
    bindCache(
      filtri_nok_modal()$macchina,
      filtri_nok_modal()$date,
      filtri_nok_modal()$sensori,
      filtri_nok_modal()$granularita
    )
  
