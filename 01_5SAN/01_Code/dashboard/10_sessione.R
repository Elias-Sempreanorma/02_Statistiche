  
  # Parametri della querystring, usati soltanto in modalita' web.
  web_query <- reactive({
    search <- session$clientData$url_search
    if (is.null(search) || !nzchar(search)) {
      return(list())
    }
    parseQueryString(search)
  })
  
  # Verifica nuovamente l'accesso lato server. La UI ha gia' bloccato
  # le richieste non autorizzate con HTTP 403/503.
  web_access <- reactive({
    if (!IS_WEB_MODE) {
      return(list(ok = TRUE, coupon = NULL, reason = NULL, status = 200L))
    }

    valida_accesso_web(web_query())
  })
  
  output$web_gate <- renderUI({
    if (!IS_WEB_MODE) return(NULL)

    access <- web_access()

    if (!isTRUE(access$ok)) {
      return(
        div(
          style = "min-height:100vh;display:flex;align-items:center;justify-content:center;padding:24px;background:#F5F6F7;",
          div(
            style = "max-width:560px;width:100%;background:#fff;border:1px solid #DDE4EA;border-radius:10px;padding:34px;box-sizing:border-box;box-shadow:0 4px 18px rgba(0,0,0,.06);",
            h2(
              if (identical(as.integer(access$status), 503L)) "Servizio non disponibile" else "Accesso non consentito",
              style = "margin-top:0;color:#234A66;"
            ),
            p(access$reason, style = "color:#5F6B76;font-size:16px;margin-bottom:0;")
          )
        )
      )
    }

    dashboard_ui
  })
  
  # In modalita' web la guida richiede l'accesso autorizzato.
  observeEvent(input$home_guida, {
    if (IS_WEB_MODE) req(isTRUE(web_access()$ok))
    showModal(guida_online_modal())
  }, ignoreInit = TRUE)

  # Sorgente unica della macchina per tutta la dashboard.
  # Internal -> selectInput; Web -> coupon validato dalla querystring.
  macchina_attiva <- reactive({
    if (!IS_WEB_MODE) {
      req(input$azienda, input$stabilimento, input$macchina)
      # Durante l'aggiornamento dei selectInput il browser puo' inviare
      # ancora il coupon precedente: non applicarlo alla nuova azienda.
      macchina_valida <- macchine_lookup |>
        filter(
          company == input$azienda,
          field == input$stabilimento,
          coupon == input$macchina
        )
      req(nrow(macchina_valida) > 0)
      return(as.character(input$macchina))
    }
    
    access <- web_access()
    req(isTRUE(access$ok), nzchar(access$coupon))
    access$coupon
  })
  
  # Download dei DOCX presenti in 00_Data/03_Documenti.
  if (length(documenti_files) > 0) {
    for (i in seq_along(documenti_files)) {
      local({
        file_corrente <- documenti_files[[i]]
        output_id <- paste0("download_documento_", i)
        
        output[[output_id]] <- downloadHandler(
          filename = function() {
            basename(file_corrente)
          },
          content = function(file) {
            file.copy(
              file_corrente,
              file,
              overwrite = TRUE
            )
          },
          contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
      })
    }
  }
  
  # I filtri a cascata esistono solo nella dashboard interna.
  if (!IS_WEB_MODE) {
    # Aggiorno insieme stabilimenti e macchine: aziende diverse possono
    # condividere lo stesso stabilimento (es. "(Non specificato)").
    observeEvent(list(input$azienda, input$stabilimento), {
      req(input$azienda)

      stabilimenti <- stabilimenti_lookup |>
        filter(company == input$azienda) |>
        pull(field)

      stabilimento_corrente <- if (
        length(input$stabilimento) == 1 &&
        input$stabilimento %in% stabilimenti
      ) {
        input$stabilimento
      } else {
        head(stabilimenti, 1)
      }

      updateSelectInput(
        session,
        "stabilimento",
        choices = stabilimenti,
        selected = stabilimento_corrente
      )

      macchine_filtrate <- macchine_lookup |>
        filter(
          company == input$azienda,
          field %in% stabilimento_corrente
        ) |>
        distinct(coupon, .keep_all = TRUE)

      macchina_corrente <- if (
        length(input$macchina) == 1 &&
        input$macchina %in% macchine_filtrate$coupon
      ) {
        input$macchina
      } else {
        head(macchine_filtrate$coupon, 1)
      }

      freezeReactiveValue(input, "macchina")
      updateSelectInput(
        session,
        "macchina",
        choices = setNames(
          macchine_filtrate$coupon,
          paste(macchine_filtrate$project, macchine_filtrate$machine_name, sep = " - ")
        ),
        selected = macchina_corrente
      )
    }, priority = 100)
  }

  # 3. Quando cambia la macchina, aggiorno i sensori disponibili
  
  # Tabelle dati mostrate direttamente all'interno dei modal.
  mostra_dati_attivazioni <- reactiveVal(FALSE)
  mostra_dati_trend <- reactiveVal(FALSE)
  mostra_dati_tank <- reactiveVal(FALSE)
  mostra_dati_nok <- reactiveVal(FALSE)
  mostra_dati_profilo_nok <- reactiveVal(FALSE)
  mostra_storico_utilizzo_nok <- reactiveVal(FALSE)
  mostra_outlier_nok <- reactiveVal(FALSE)
  
  # I filtri vengono applicati solo dopo una breve pausa dall'ultima scelta.
  # In questo modo una selezione multipla genera un solo aggiornamento.
  ritardo_filtri_ms <- 800
  
  normalizza_sensori <- function(x) {
    if (is.null(x)) character(0) else sort(unique(as.character(x)))
  }
  
  sensori_modal_correnti <- reactiveVal(character(0))
  date_modal_corrente <- reactiveVal(NULL)
  modal_apertura_id <- reactiveVal(0)
  granularita_attivazioni_corrente <- reactiveVal("Giorno")
  granularita_nok_corrente <- reactiveVal("Giorno")
  granularita_utilizzo_corrente <- reactiveVal("Settimana")
  report_periodo_corrente <- reactiveVal(NULL)
  
  filtri_principali <- reactive({
    req(macchina_attiva(), input$date)
    
    list(
      macchina = macchina_attiva(),
      date = as.Date(input$date),
      sensori = sensori_lookup |>
        filter(coupon == macchina_attiva()) |>
        pull(cds_name) |>
        normalizza_sensori()
    )
  }) |>
    debounce(millis = ritardo_filtri_ms)
  
