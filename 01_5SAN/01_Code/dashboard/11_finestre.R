  # ---------------------------------------------------------------------
  # Finestre complete aperte dalla Home. Periodo e sensori partono dai
  # valori generali; le modifiche vengono riportate alla Home solo dopo
  # la pausa definita da ritardo_filtri_ms.
  # ---------------------------------------------------------------------
  # Modal unico con navigazione interna tramite radioGroupButtons.
  # apri_modal() viene chiamata dai tre bottoni Home: imposta la vista
  # iniziale e mostra il dialogo. Il contenuto cambia senza chiuderlo.
  # ---------------------------------------------------------------------
  apri_modal <- function(
    vista_iniziale,
    sensori_iniziali = NULL,
    date_iniziali = NULL
  ) {
    if (is.null(sensori_iniziali)) {
      sensori_iniziali <- sensori_lookup |>
        filter(coupon == isolate(macchina_attiva())) |>
        pull(cds_name)
    }
    
    if (is.null(date_iniziali)) {
      date_iniziali <- isolate(as.Date(input$date))
    }
    
    sensori_modal_correnti(normalizza_sensori(sensori_iniziali))
    date_modal_corrente(as.Date(date_iniziali))
    
    macchina_corrente <- macchine_lookup |>
      filter(coupon == isolate(macchina_attiva())) |>
      slice_head(n = 1)
    
    riferimento_macchina <- if (nrow(macchina_corrente) > 0) {
      paste(
        macchina_corrente$project,
        macchina_corrente$machine_name,
        sep = " – "
      )
    } else {
      as.character(isolate(macchina_attiva()))
    }
    
    granularita_attivazioni_corrente("Giorno")
    granularita_nok_corrente("Giorno")
    granularita_utilizzo_corrente("Settimana")
    
    modal_apertura_id(isolate(modal_apertura_id()) + 1)
    
    mostra_dati_attivazioni(FALSE)
    mostra_dati_trend(FALSE)
    mostra_dati_tank(FALSE)
    mostra_dati_nok(FALSE)
    mostra_dati_profilo_nok(FALSE)
    mostra_storico_utilizzo_nok(FALSE)
    
    layout_macchina <- immagini_macchine |>
      filter(coupon == isolate(macchina_attiva())) |>
      distinct(file_name)
    layout_macchina <- layout_macchina[
      file.exists(file.path(cds_images_dir, layout_macchina$file_name)), , drop = FALSE
    ]
    miniature_layout <- if (nrow(layout_macchina) > 0) {
      div(
        class = "machine-layout-thumbs",
        `aria-label` = "Layout macchina",
        lapply(seq_len(nrow(layout_macchina)), function(i) {
          nome_file <- layout_macchina$file_name[i]
          versione <- unname(tools::md5sum(file.path(cds_images_dir, nome_file)))
          tags$button(
            type = "button", class = "machine-layout-thumb",
            title = "Ingrandisci layout", `aria-haspopup` = "dialog",
            `aria-controls` = "machine-layout-dialog",
            tags$img(
              src = paste0("cds-images/", nome_file, "?v=", versione),
              alt = paste("Layout", riferimento_macchina, i)
            )
          )
        })
      )
    }

    showModal(modalDialog(
      title = NULL,
      size = "xl",
      easyClose = TRUE,
      fade = TRUE,
      footer = NULL,
      
      div(
        class = "modal-machine-header",
        div(
          paste0("Macchina: ", riferimento_macchina),
          class = "modal-machine-title"
        ),
        tags$button(
          type = "button",
          class = "modal-close-x",
          `data-dismiss` = "modal",
          `aria-label` = "Chiudi",
          title = "Chiudi (Esc)",
          HTML("&times;")
        )
      ),
      
      div(
        class = "modal-machine-toolbar",
        div(
          class = "modal-nav-bar",
          radioGroupButtons(
            "vista_selezionata",
            label = NULL,
            choices = c(
              "Conteggio manovre" = "attivazioni",
              "Utilizzo storico" = "nok",
              "Vita componenti"          = "vita",
              "Allarmi e segnalazioni"   = "allarmi"
            ),
            selected = vista_iniziale,
            status   = "primary",
            size     = "normal",
            width    = "auto"
          )
        ),
        tags$button(
          type = "button", class = "machine-guide-open btn btn-default",
          `aria-haspopup` = "dialog", `aria-controls` = "machine-guide-dialog",
          icon("circle-question"), " Guida alla dashboard"
        )
      ),
      guida_online_modal(nested = TRUE),
      if (nrow(layout_macchina) > 0) tags$dialog(
        id = "machine-layout-dialog",
        `aria-labelledby` = "machine-layout-title",
        div(
          class = "machine-layout-heading",
          tags$strong("Layout macchina", id = "machine-layout-title"),
          tags$button(type = "button", class = "machine-layout-close", "Chiudi layout", autofocus = NA)
        ),
        tags$img(alt = "Layout macchina")
      ),

      div(
        class = "machine-details-content",
        miniature_layout,
        uiOutput("modal_contenuto")
      )
    ))
  }
  
  observeEvent(input$home_attivazioni, {
    req(macchina_attiva(), input$date)
    apri_modal("attivazioni")
  })
  
  observeEvent(input$home_vita, {
    req(macchina_attiva())
    apri_modal("vita")
  })
  
  observeEvent(input$home_nok, {
    req(macchina_attiva(), input$date)
    apri_modal("nok")
  })
  
  observeEvent(input$home_allarmi, {
    req(macchina_attiva())
    apri_modal("allarmi")
  })
  
  # Click su un CdS nello schema: seleziona quel sensore e apre la tenda
  # direttamente sulla vista Conteggio attivazioni.
  observeEvent(input$schema_sensor_click, {
    req(macchina_attiva(), input$date, input$schema_sensor_click$sensor)
    
    sensore_cliccato <- input$schema_sensor_click$sensor
    sensori_validi <- sensori_lookup |>
      filter(coupon == macchina_attiva()) |>
      pull(cds_name)
    
    req(sensore_cliccato %in% sensori_validi)
    
    apri_modal("attivazioni", sensore_cliccato)
  })
  
  # Quando si cambia vista, azzera la visibilita' delle tabelle dati
  observeEvent(input$vista_selezionata, {
    mostra_dati_attivazioni(FALSE)
    mostra_dati_trend(FALSE)
    mostra_dati_tank(FALSE)
    mostra_dati_nok(FALSE)
    mostra_dati_profilo_nok(FALSE)
    mostra_storico_utilizzo_nok(FALSE)
    mostra_outlier_nok(FALSE)
    
  }, ignoreInit = TRUE)
  
  # Contenuto del modal: si aggiorna al cambio di vista senza chiudere il dialogo
  output$modal_contenuto <- renderUI({
    req(input$vista_selezionata, macchina_attiva())
    modal_apertura_id()
    
    vista <- input$vista_selezionata
    
    sensori_macchina <- sensori_lookup |>
      filter(coupon == macchina_attiva())
    
    sensori_selezionati <- intersect(
      isolate(sensori_modal_correnti()),
      sensori_macchina$cds_name
    )
    
    date_selezionate <- isolate(date_modal_corrente())
    req(length(date_selezionate) == 2, all(!is.na(date_selezionate)))
    
    scelte_sensori <- setNames(
      sensori_macchina$cds_name,
      paste(sensori_macchina$cds_name, sensori_macchina$sensor_description, sep = " - ")
    )
    
    picker_opts <- pickerOptions(
      actionsBox = TRUE,
      liveSearch = TRUE,
      selectedTextFormat = "count > 3",
      countSelectedText = "{0} componenti selezionati"
    )
    
    if (vista == "attivazioni") {
      tagList(
        div(
          class = "modal-filters",
          fluidRow(
            column(4,
                   dateRangeInput(
                     "modal_date", "Periodo:",
                     start = date_selezionate[1], end = date_selezionate[2],
                     min = data_min, max = data_max,
                     format = "dd-mm-yyyy", separator = " a ", language = "it", weekstart = 1
                   )
            ),
            column(8,
                   pickerInput(
                     "modal_sensori", "Componenti:",
                     choices = scelte_sensori, selected = sensori_selezionati,
                     multiple = TRUE, options = picker_opts
                   )
            )
          ),
          radioButtons(
            "modal_granularita_attivazioni", "Raggruppamento:",
            choices = c("Ora", "Giorno", "Settimana", "Mese", "Trimestre", "Anno"),
            selected = isolate(granularita_attivazioni_corrente()), inline = TRUE
          )
        ),
        h4("Conteggio manovre", class = "titolo-sezione"),
        div(
          class = "activation-scroll",
          uiOutput("modal_activationPlot_container")
        ),
        div(
          class = "modal-data-button",
          actionButton("modal_btn_dati_attivazioni", "Mostra dati", icon = icon("table"), class = "btn-sm btn-default")
        ),
        uiOutput("modal_panel_dati_attivazioni"),
        h4("Andamento per componente", class = "titolo-sezione"),
        br(),
        div(
          class = "activation-scroll",
          uiOutput("modal_activationTrendPlot_container")
        ),
        div(
          class = "modal-data-button",
          actionButton("modal_btn_dati_trend", "Mostra dati", icon = icon("table"), class = "btn-sm btn-default")
        ),
        uiOutput("modal_panel_dati_trend")
      )
      
    } else if (vista == "vita") {
      tagList(
        div(
          class = "modal-filters",
          pickerInput(
            "modal_sensori", "Componenti:",
            choices = scelte_sensori, selected = sensori_selezionati,
            multiple = TRUE, options = picker_opts
          )
        ),
        div(
          class = "life-header",
          div(
            h4("Vita componenti", class = "life-header-title"),
            div(
              "Manovre effettuate e anni di utilizzo rispetto ai valori di riferimento",
              class = "life-header-subtitle"
            )
          )
        ),
        uiOutput("modal_tankCards"),
        div(
          class = "modal-data-button",
          actionButton("modal_btn_dati_tank", "Mostra dati", icon = icon("table"), class = "btn-sm btn-default")
        ),
        uiOutput("modal_panel_dati_tank")
      )
      
    } else if (vista == "nok") {
      tagList(
        div(
          class = "modal-filters",
          fluidRow(
            column(4,
                   dateRangeInput(
                     "modal_date", "Periodo:",
                     start = date_selezionate[1], end = date_selezionate[2],
                     min = data_min, max = data_max,
                     format = "dd-mm-yyyy", separator = " a ", language = "it", weekstart = 1
                   )
            ),
            column(8,
                   pickerInput(
                     "modal_sensori", "Componenti:",
                     choices = scelte_sensori, selected = sensori_selezionati,
                     multiple = TRUE, options = picker_opts
                   )
            )
          )
        ),
        div(
          style = paste0(
            "display:flex;",
            "align-items:center;",
            "justify-content:space-between;",
            "gap:16px;",
            "margin-top:10px;"
          ),
          h4(
            "Profilo utilizzo componenti di sicurezza",
            class = "titolo-sezione",
            style = "margin:0;"
          ),
          actionButton(
            "modal_btn_outlier_nok",
            "Manovre anomale",
            class = "btn-sm btn-default"
          )
        ),
        uiOutput("modal_panel_outlier_nok"),
        div(
          style = paste0(
            "margin:14px 0 20px 0;",
            "padding:15px 18px;",
            "border:1px solid #DDE4EA;",
            "border-radius:9px;",
            "background:#FAFBFC;"
          ),
          div(
            style = paste0(
              "display:flex;",
              "align-items:center;",
              "gap:18px;",
              "margin-bottom:8px;",
              "flex-wrap:wrap;"
            ),
            div(
              "Utilizzo componenti di sicurezza nel periodo",
              style = paste0(
                "font-size:16px;",
                "font-weight:700;",
                "color:#4A4A4A;"
              )
            ),
            div(
              style = "margin-bottom:-15px;",
              radioButtons(
                "modal_granularita_utilizzo",
                label = NULL,
                choices = c("Settimana", "Mese"),
                selected = isolate(
                  granularita_utilizzo_corrente()
                ),
                inline = TRUE
              )
            )
          ),
          uiOutput("modal_utilizzo_macchina")
        ),
        uiOutput("modal_panel_storico_utilizzo_nok"),
        girafeOutput("modal_utilizzoNokPlot", height = "380px"),
        div(
          class = "modal-data-button",
          actionButton(
            "modal_btn_dati_profilo_nok",
            "Mostra dati",
            icon = icon("table"),
            class = "btn-sm btn-default"
          )
        ),
        uiOutput("modal_panel_dati_profilo_nok"),
        h4("NOK", class = "titolo-sezione"),
        p("Rapporto tra il numero di manovre medie nel periodo e quello storico. Le medie sono rapportate alle ore di attività giornaliere. Il valore 1 indica un numero di manovre per ora in linea con il passato. Il valore 2 indica il doppio; 0,5 indica la metà.", class = "help-block"),
        radioButtons(
          "modal_granularita_nok",
          "Raggruppamento:",
          choices = c("Giorno", "Settimana", "Mese", "Trimestre", "Anno"),
          selected = isolate(granularita_nok_corrente()),
          inline = TRUE
        ),
        girafeOutput("modal_utilizzoTempoPlot", height = "380px"),
        div(
          class = "modal-data-button",
          actionButton(
            "modal_btn_dati_nok",
            "Mostra dati",
            icon = icon("table"),
            class = "btn-sm btn-default"
          )
        ),
        uiOutput("modal_panel_dati_nok"),
        h4("Disponibilità della macchina", class = "titolo-sezione"),
        p(
          "La percentuale indica quanta parte del tempo di accensione la macchina era disponibile al lavoro, con tutti i componenti monitorati nella condizione prevista. Nel dettaglio sono riportate le ore disponibili rispetto alle ore di accensione dell’intero periodo.",
          class = "help-block"
        ),
        girafeOutput("modal_disponibilitaPlot", height = "380px")
      )
      
    } else if (vista == "allarmi") {
      tagList(
        div(
          class = "modal-filters",
          fluidRow(
            column(
              3,
              dateRangeInput(
                "modal_date", "Periodo:",
                start = date_selezionate[1], end = date_selezionate[2],
                min = data_min, max = data_max,
                format = "dd-mm-yyyy", separator = " a ", language = "it", weekstart = 1
              )
            ),
            column(
              6,
              pickerInput(
                "modal_sensori", "Componenti:",
                choices = scelte_sensori, selected = sensori_selezionati,
                multiple = TRUE, options = picker_opts
              )
            ),
            column(
              3,
              if (length(documenti_files) > 0) {
                div(
                  class = "modal-documenti",
                  div(
                    class = "modal-documenti-title",
                    icon("file-word"),
                    span("Documenti")
                  ),
                  div(
                    class = "documenti-links",
                    lapply(seq_along(documenti_files), function(i) {
                      downloadLink(
                        outputId = paste0("download_documento_", i),
                        label = tools::file_path_sans_ext(
                          basename(documenti_files[[i]])
                        ),
                        class = "documento-download"
                      )
                    })
                  )
                )
              }
            )
          )
        ),
        div(
          class = "report-panel",
          h4("Report anomalie", class = "titolo-sezione"),
          radioButtons(
            "report_granularita",
            "Periodo del rapporto:",
            choices = c("Settimana", "Mese"),
            selected = "Settimana",
            inline = TRUE
          ),
          uiOutput("report_period_buttons")
        ),
        div(
          class = "alarm-section",
          h4("Manovre anomale", class = "titolo-sezione"),
          p(
            "Giornate o singole ore con un numero di manovre molto diverso dal comportamento abituale del componente."
          ),
          uiOutput("modal_allarmi_attivazioni_panel")
        ),
        div(
          class = "alarm-section",
          h4("Utilizzo della macchina", class = "titolo-sezione"),
          p(
            "Settimane o mesi conclusi con utilizzo complessivo fuori dall’intervallo storico abituale."
          ),
          uiOutput("modal_allarmi_nok_panel")
        ),
        div(
          class = "alarm-section",
          h4("Componenti aperti o senza contatto elettrico", class = "titolo-sezione"),
          p(
            "Periodi di oltre un'ora in cui il componente è rimasto aperto o senza contatto elettrico mentre la macchina continuava a inviare dati."
          ),
          uiOutput("modal_allarmi_sensori_aperti_panel")
        )
      )
    }
  })
  
  observeEvent(input$modal_btn_dati_attivazioni, {
    mostra_dati_attivazioni(!mostra_dati_attivazioni())
    updateActionButton(
      session, "modal_btn_dati_attivazioni",
      label = if (mostra_dati_attivazioni()) "Nascondi dati" else "Mostra dati"
    )
  })
  
  observeEvent(input$modal_btn_dati_trend, {
    mostra_dati_trend(!mostra_dati_trend())
    updateActionButton(
      session, "modal_btn_dati_trend",
      label = if (mostra_dati_trend()) "Nascondi dati" else "Mostra dati"
    )
  })
  
  observeEvent(input$modal_btn_dati_tank, {
    mostra_dati_tank(!mostra_dati_tank())
    updateActionButton(
      session, "modal_btn_dati_tank",
      label = if (mostra_dati_tank()) "Nascondi dati" else "Mostra dati"
    )
  })
  
  observeEvent(input$modal_btn_dati_nok, {
    mostra_dati_nok(!mostra_dati_nok())
    updateActionButton(
      session, "modal_btn_dati_nok",
      label = if (mostra_dati_nok()) "Nascondi dati" else "Mostra dati"
    )
  })
  
  observeEvent(input$modal_btn_dati_profilo_nok, {
    mostra_dati_profilo_nok(!mostra_dati_profilo_nok())
    updateActionButton(
      session, "modal_btn_dati_profilo_nok",
      label = if (mostra_dati_profilo_nok()) "Nascondi dati" else "Mostra dati"
    )
  })
  
  observeEvent(input$modal_btn_storico_utilizzo_nok, {
    mostra_storico_utilizzo_nok(
      !mostra_storico_utilizzo_nok()
    )
  })
  
  observeEvent(input$modal_btn_outlier_nok, {
    mostra_outlier_nok(!mostra_outlier_nok())
  })
  
  # ---------------------------------------------------------------------
  # Coda ottimizzata dei filtri modal.
  # Il debounce aspetta 800 ms dall'ultima modifica. Data e sensori del
  # modal vengono mantenuti in reactiveVal dedicati: il modal aggiorna
  # eventualmente la Home, ma la Home non riscrive gli input del modal.
  # Questo elimina la sincronizzazione circolare che generava i loop.
  # ---------------------------------------------------------------------
  sensori_modal_input <- eventReactive(input$modal_sensori, {
    list(
      apertura_id = isolate(modal_apertura_id()),
      sensori = normalizza_sensori(input$modal_sensori)
    )
  }, ignoreNULL = TRUE) |>
    debounce(millis = ritardo_filtri_ms)
  
  observeEvent(sensori_modal_input(), {
    filtri <- sensori_modal_input()
    
    if (identical(filtri$apertura_id, isolate(modal_apertura_id()))) {
      sensori_modal_correnti(filtri$sensori)
    }
  }, ignoreInit = TRUE)
  
  periodo_modal_input <- eventReactive(input$modal_date, {
    list(
      apertura_id = isolate(modal_apertura_id()),
      date = as.Date(input$modal_date)
    )
  }, ignoreNULL = TRUE) |>
    debounce(millis = ritardo_filtri_ms)
  
  observeEvent(periodo_modal_input(), {
    filtri <- periodo_modal_input()
    
    if (!identical(
      filtri$apertura_id,
      isolate(modal_apertura_id())
    )) {
      return()
    }
    
    # Unica sorgente dati del modal mentre e' aperto.
    # L'aggiornamento avviene solo dopo il debounce.
    if (!identical(
      isolate(date_modal_corrente()),
      filtri$date
    )) {
      date_modal_corrente(filtri$date)
    }
    
    # Il modal puo' riportare la scelta alla Home, ma la Home non
    # riscrive mai modal_date: in questo modo non esiste piu' il ciclo
    # input$date -> modal_date -> input$date.
    if (!identical(
      isolate(as.Date(input$date)),
      filtri$date
    )) {
      updateDateRangeInput(
        session,
        "date",
        start = filtri$date[1],
        end = filtri$date[2]
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$modal_granularita_attivazioni, {
    req(input$modal_granularita_attivazioni)
    granularita_attivazioni_corrente(input$modal_granularita_attivazioni)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$modal_granularita_nok, {
    req(input$modal_granularita_nok)
    granularita_nok_corrente(input$modal_granularita_nok)
  }, ignoreNULL = TRUE)
  
  # Il selettore Settimana/Mese vive fuori dal renderUI dinamico della card:
  # cosi' il cambio valore non distrugge e ricrea lo stesso input.
  observeEvent(input$modal_granularita_utilizzo, {
    req(input$modal_granularita_utilizzo)
    granularita_utilizzo_corrente(input$modal_granularita_utilizzo)
  }, ignoreNULL = TRUE)
  
  filtri_attivazioni_modal <- reactive({
    req(macchina_attiva())
    
    date_corrente <- date_modal_corrente()
    req(
      length(date_corrente) == 2,
      all(!is.na(date_corrente))
    )
    
    list(
      macchina = macchina_attiva(),
      date = date_corrente,
      sensori = sensori_modal_correnti(),
      granularita = granularita_attivazioni_corrente()
    )
  })
  
  filtri_vita_modal <- reactive({
    req(macchina_attiva())
    
    list(
      macchina = macchina_attiva(),
      sensori = sensori_modal_correnti()
    )
  })
  
  filtri_nok_modal <- reactive({
    req(macchina_attiva())
    
    date_corrente <- date_modal_corrente()
    req(
      length(date_corrente) == 2,
      all(!is.na(date_corrente))
    )
    
    list(
      macchina = macchina_attiva(),
      date = date_corrente,
      sensori = sensori_modal_correnti(),
      granularita = granularita_nok_corrente()
    )
  })

  
