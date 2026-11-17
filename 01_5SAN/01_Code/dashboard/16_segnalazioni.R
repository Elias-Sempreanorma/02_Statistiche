  # ---------------------------------------------------------------------
  # Allarmi automatici nel periodo selezionato.
  # 1) anomalie sulle attivazioni: giornate escluse dal LOF;
  # 2) anomalie NOK: NOK giornaliero fuori da media storica +/- 3 sigma.
  # ---------------------------------------------------------------------
  # ---------------------------------------------------------------------
  # Gli alert mostrati in dashboard sono gli stessi persistiti nel DB.
  # In questo modo ID, soglia dei 7 giorni e logica di rilevazione
  # hanno un'unica sorgente e non vengono ricalcolati ad ogni apertura.
  # ---------------------------------------------------------------------
  alerts_correnti <- reactiveFileReader(
    intervalMillis = 60000, session = session,
    filePath = alerts_path, readFunc = leggi_alerts_dashboard
  )
  alerts_filtrati_modal <- reactive({
    filtri <- filtri_nok_modal()

    alerts_correnti() |>
      filter(
        coupon == filtri$macchina,
        as.Date(period_end, tz = "Europe/Rome") >= filtri$date[1],
        as.Date(period_start, tz = "Europe/Rome") <= filtri$date[2]
      )
  })

  allarmi_attivazioni_modal <- reactive({
    filtri <- filtri_nok_modal()

    alerts_filtrati_modal() |>
      filter(
        alert_type %in% c("activation_daily", "activation_hourly"),
        vapply(strsplit(coalesce(sensor_code, ""), "; ", fixed = TRUE),
          function(codes) any(codes %in% filtri$sensori), logical(1))
      ) |>
      arrange(period_start, sensor_code, alert_id) |>
      transmute(
        `ID segnalazione` = alert_id,
        Componente = if_else(alert_scope == "machine", functional_unit,
          paste(sensor_code, sensor_description, sep = " - ")),
        Data = as.Date(period_start, tz = "Europe/Rome"),
        Ora = if_else(
          alert_type == "activation_hourly",
          format(period_start, "%H:00", tz = "Europe/Rome"),
          "-"
        ),
        Manovre = signal_data_text,
        Motivo = signal_description
      )
  })

  allarmi_sensori_aperti_modal <- reactive({
    filtri <- filtri_nok_modal()

    alerts_filtrati_modal() |>
      filter(
        alert_type == "sensor_open",
        vapply(strsplit(coalesce(sensor_code, ""), "; ", fixed = TRUE),
          function(codes) any(codes %in% filtri$sensori), logical(1))
      ) |>
      arrange(period_start, sensor_code, alert_id) |>
      transmute(
        `ID segnalazione` = alert_id,
        Componente = if_else(alert_scope == "machine", functional_unit,
          paste(sensor_code, sensor_description, sep = " - ")),
        Data = as.Date(period_start, tz = "Europe/Rome"),
        Inizio = period_start,
        Fine = period_end,
        Dati = signal_data_text
      )
  })

  allarmi_nok_modal <- reactive({
    alerts_filtrati_modal() |>
      filter(alert_type %in% c("utilization_weekly", "utilization_monthly")) |>
      arrange(period_start, alert_id) |>
      transmute(`ID segnalazione` = alert_id,
        Macchina = coalesce(machine_name, coupon),
        Periodo = if_else(alert_type == "utilization_monthly", "Mese", "Settimana"),
        Inizio = as.Date(period_start, tz = "Europe/Rome"),
        Fine = as.Date(period_end, tz = "Europe/Rome"),
        Descrizione = paste0(signal_description, ". ", signal_data_text))
  })

  allarmi_gateway_modal <- reactive({
    alerts_filtrati_modal() |>
      filter(alert_type == "gateway_disconnection") |>
      arrange(period_start, alert_id) |>
      transmute(
        `ID segnalazione` = alert_id,
        `Dispositivo di collegamento` = if_else(
          is.na(gateway_name) | gateway_name == "",
          gateway_id,
          gateway_name
        ),
        Inizio = period_start,
        Fine = period_end,
        Descrizione = signal_description,
        Dati = signal_data_text
      )
  })

  output$modal_allarmi_attivazioni_panel <- renderUI({
    tabella <- allarmi_attivazioni_modal()
    
    if (nrow(tabella) == 0) {
      return(
        div(
          class = "alarm-empty",
          "Nessuna anomalia nelle manovre nel periodo selezionato."
        )
      )
    }
    
    DTOutput("modal_allarmi_attivazioni")
  })
  
  output$modal_allarmi_sensori_aperti_panel <- renderUI({
    tabella <- allarmi_sensori_aperti_modal()
    
    if (nrow(tabella) == 0) {
      return(
        div(
          class = "alarm-empty",
          "Nessun componente aperto o senza contatto elettrico per oltre 1 ora consecutiva nel periodo selezionato."
        )
      )
    }
    
    DTOutput("modal_allarmi_sensori_aperti")
  })
  
  output$modal_allarmi_nok_panel <- renderUI({
    tabella <- allarmi_nok_modal()
    
    if (nrow(tabella) == 0) {
      return(
        div(
          class = "alarm-empty",
          "Nessuna anomalia di utilizzo nel periodo. Il confronto richiede almeno 7 giornate storiche valide e non anomale e un periodo precedente confrontabile."
        )
      )
    }
    
    DTOutput("modal_allarmi_nok")
  })

  output$modal_allarmi_gateway_panel <- renderUI({
    tabella <- allarmi_gateway_modal()

    if (nrow(tabella) == 0) {
      return(
        div(
          class = "alarm-empty",
          "Nessuna interruzione delle comunicazioni segnalata nel periodo selezionato."
        )
      )
    }

    DTOutput("modal_allarmi_gateway")
  })
  
  output$modal_allarmi_attivazioni <- renderDT({
    tabella_segnalazioni(allarmi_attivazioni_modal(), "Data")
  }, server = TRUE)

  output$modal_allarmi_sensori_aperti <- renderDT({
    tabella_segnalazioni(allarmi_sensori_aperti_modal(), "Inizio")
  }, server = TRUE)

  output$modal_allarmi_nok <- renderDT({
    tabella_segnalazioni(allarmi_nok_modal(), "Inizio")
  }, server = TRUE)

  output$modal_allarmi_gateway <- renderDT({
    tabella_segnalazioni(allarmi_gateway_modal(), "Inizio")
  }, server = TRUE)

