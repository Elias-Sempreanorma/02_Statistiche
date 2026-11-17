  valori_giornalieri_modal_nok <- reactive({
    
    filtri <- filtri_nok_modal()
    req(length(filtri$sensori) > 0)
    
    prepara_valori_giornalieri_nok(
      filtri$macchina,
      filtri$sensori
    )
  }) |>
    bindCache(filtri_nok_modal()$macchina, filtri_nok_modal()$sensori)
  
  nmn_storico_modal_nok <- reactive({
    filtri <- filtri_nok_modal()
    
    valori_giornalieri_modal_nok() |>
      filter(
        !outlier_lof,
        day < filtri$date[1] | day > filtri$date[2]
      ) |>
      group_by(cds_name, sensor_description) |>
      summarise(NMN = mean(daily_value, na.rm = TRUE), .groups = "drop")
  })
  
  # Statistiche storiche del NOK giornaliero per sensore.
  # Limiti di riferimento: media storica +/- 3 sigma.
  statistiche_nok_storiche_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    valori_giornalieri_modal_nok() |>
      filter(
        !outlier_lof,
        day < filtri$date[1] | day > filtri$date[2]
      ) |>
      left_join(
        nmn_storico_modal_nok(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK_giornaliero = case_when(
          is.na(daily_value) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ daily_value / NMN
        )
      ) |>
      filter(is.finite(NOK_giornaliero)) |>
      group_by(cds_name, sensor_description) |>
      summarise(
        media_nok_storico = mean(NOK_giornaliero, na.rm = TRUE),
        sigma_nok_storico = if (n() >= 2) {
          sd(NOK_giornaliero, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        limite_nok_inf = pmax(0, media_nok_storico - 3 * sigma_nok_storico),
        limite_nok_sup = media_nok_storico + 3 * sigma_nok_storico
      )
  })
  
  kpi_nok_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    nmm_per_sensore <- valori_giornalieri_modal_nok() |>
      filter(
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      group_by(cds_name, sensor_description) |>
      summarise(NMM = mean(daily_value, na.rm = TRUE), .groups = "drop")
    
    nmn_storico_modal_nok() |>
      left_join(nmm_per_sensore, by = c("cds_name", "sensor_description")) |>
      left_join(
        statistiche_nok_storiche_modal(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK = case_when(
          is.na(NMN) | is.na(NMM) | NMM == 0 ~ NA_real_,
          TRUE ~ NMM / NMN
        ),
        stato_nok = case_when(
          !is.finite(NOK) |
            !is.finite(limite_nok_inf) |
            !is.finite(limite_nok_sup) ~ "N/D",
          NOK < limite_nok_inf | NOK > limite_nok_sup ~ "Anomalo",
          TRUE ~ "Normale"
        )
      ) |>
      ordina_naturale()
  })
  
  output$modal_nok_table <- renderTable({
    
    kpi <- kpi_nok_modal()
    
    validate(
      need(nrow(kpi) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    kpi |>
      transmute(
        Componente = paste(cds_name, sensor_description, sep = " - "),
        NOK = case_when(
          is.na(NOK) | is.infinite(NOK) ~
            "<span style='color:#718096;font-weight:800;'>N/D</span>",
          stato_nok == "Anomalo" ~
            paste0(
              "<span style='color:#B42318;font-weight:800;'>",
              format(round(NOK, 3), nsmall = 3),
              "</span>"
            ),
          TRUE ~
            paste0(
              "<span style='color:#19764A;font-weight:800;'>",
              format(round(NOK, 3), nsmall = 3),
              "</span>"
            )
        )
      ) |>
      pivot_wider(names_from = Componente, values_from = NOK)
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  spacing = "s",
  align = "c",
  sanitize.text.function = function(x) x)
  
