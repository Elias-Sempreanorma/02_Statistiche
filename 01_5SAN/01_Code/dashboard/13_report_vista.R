  formatta_variazione_report <- function(x) {
    if (!is.finite(x)) return("N/D")
    paste0(
      ifelse(x > 0, "+", ""),
      formatC(x, format = "f", digits = 1, decimal.mark = ","),
      "%"
    )
  }
  
  testo_andamento_report <- function(x, nome) {
    if (!is.finite(x)) return(paste(nome, "non confrontabile con il periodo precedente"))
    if (abs(x) < 0.05) return(paste(nome, "sostanzialmente stabile"))
    paste(
      nome,
      ifelse(x > 0, "in aumento del", "in diminuzione del"),
      paste0(formatC(abs(x), format = "f", digits = 1, decimal.mark = ","), "%")
    )
  }

  
  testo_report_esteso <- function(r) {
    n_sensori <- nrow(r$confronto)
    n_nok_fuori <- nrow(r$nok_anomalo)
    n_att_anomale <- nrow(r$attivazioni_anomale)
    
    sensori_att_anomali <- if (n_att_anomale > 0) {
      dplyr::n_distinct(r$attivazioni_anomale$Componente)
    } else {
      0L
    }
    
    utilizzo_range <- if (
      is.finite(r$p10_utilizzo) &&
      is.finite(r$p90_utilizzo)
    ) {
      paste0(
        "intervallo abituale da ",
        formatC(r$p10_utilizzo, format = "f", digits = 3, decimal.mark = ","),
        " a ",
        formatC(r$p90_utilizzo, format = "f", digits = 3, decimal.mark = ",")
      )
    } else {
      "intervallo storico non disponibile"
    }
    
    nok_range <- if (
      is.finite(r$nok_macchina_inf) &&
      is.finite(r$nok_macchina_sup)
    ) {
      paste0(
        "valori abituali da ",
        formatC(r$nok_macchina_inf, format = "f", digits = 3, decimal.mark = ","),
        " a ",
        formatC(r$nok_macchina_sup, format = "f", digits = 3, decimal.mark = ",")
      )
    } else {
      "dati insufficienti per il confronto con lo storico"
    }
    
    stato_utilizzo <- if (identical(r$utilizzo_stato, "Normale")) {
      "all'interno del comportamento storico atteso"
    } else if (identical(r$utilizzo_stato, "Elevato")) {
      "sopra il livello storico di riferimento"
    } else if (identical(r$utilizzo_stato, "Basso")) {
      "sotto il livello storico di riferimento"
    } else {
      "non classificabile rispetto allo storico disponibile"
    }
    
    stato_nok <- if (isTRUE(r$nok_macchina_fuori)) {
      "fuori dal proprio intervallo storico"
    } else {
      "all'interno del proprio intervallo storico"
    }
    
    p1 <- paste0(
      "Questo rapporto descrive come è stata utilizzata la macchina ",
      r$macchina,
      " (coupon ",
      r$coupon,
      ") nel periodo ",
      format(r$inizio, "%d/%m/%Y"),
      " - ",
      format(r$fine, "%d/%m/%Y"),
      ". Il confronto riguarda il periodo precedente della stessa durata, ",
      format(r$precedente_inizio, "%d/%m/%Y"),
      " - ",
      format(r$precedente_fine, "%d/%m/%Y"),
      ". I risultati sono confrontati anche con il comportamento abituale della macchina."
    )
    
    p2 <- paste0(
      "Per quanto riguarda l’utilizzo, ",
      testo_andamento_report(r$utilizzo_pct, "l'utilizzo complessivo"),
      ". Il valore del periodo è ",
      ifelse(
        is.finite(r$utilizzo),
        formatC(r$utilizzo, format = "f", digits = 3, decimal.mark = ","),
        "N/D"
      ),
      " e risulta ",
      stato_utilizzo,
      "; riferimento: ",
      utilizzo_range,
      ". Un aumento rispetto al periodo precedente non indica, da solo, un problema: conta anche quanto il valore si discosta dall’uso abituale."
    )
    
    p3 <- paste0(
      "L’utilizzo medio rispetto allo storico è ",
      ifelse(
        is.finite(r$nok_macchina),
        formatC(r$nok_macchina, format = "f", digits = 3, decimal.mark = ","),
        "N/D"
      ),
      "; ",
      testo_andamento_report(r$nok_macchina_pct, "rispetto al periodo precedente"),
      ". Un valore di 1 corrisponde all’utilizzo abituale. Nel complesso, il risultato è ",
      stato_nok,
      " (",
      nok_range,
      "). Il NOK resta un indicatore descrittivo; le segnalazioni riguardano manovre e utilizzo complessivo della macchina."
    )
    
    p4 <- if (n_att_anomale > 0) {
      paste0(
        "Sono inoltre presenti ",
        n_att_anomale,
        " segnalazioni di manovre anomale distribuite su ",
        sensori_att_anomali,
        " componenti. Le tabelle successive permettono di risalire al componente, alla data e, quando disponibile, all'ora dell'evento."
      )
    } else {
      paste0(
        "Nel periodo non sono state rilevate manovre da segnalare."
      )
    }
    
    c(
      paste(p1, p2),
      paste(p3, p4)
    )
  }
  
  observeEvent(input$report_periodo_click, {
    req(input$report_periodo_click$start, input$report_periodo_click$end)
    
    report_periodo_corrente(list(
      start = as.Date(input$report_periodo_click$start),
      end = as.Date(input$report_periodo_click$end),
      label = input$report_periodo_click$label
    ))
    
    showModal(modalDialog(
      title = NULL,
      size = "xl",
      easyClose = TRUE,
      fade = TRUE,
      footer = NULL,
      uiOutput("report_modal_content")
    ))
  })

  
  observeEvent(input$report_back_to_alarms, {
    sensori_ripristino <- isolate(sensori_modal_correnti())
    date_ripristino <- isolate(date_modal_corrente())
    
    removeModal()
    
    apri_modal(
      "allarmi",
      sensori_iniziali = sensori_ripristino,
      date_iniziali = date_ripristino
    )
  }, ignoreInit = TRUE)
  
  output$report_modal_content <- renderUI({
    r <- report_dati()
    narrativa <- testo_report_esteso(r)
    
    sensori <- r$confronto |>
      mutate(
        sensore_label = paste(cds_name, sensor_description, sep = " - ")
      )
    
    cella_nok <- function(valore, fuori) {
      classe <- if (!is.finite(valore)) {
        "report-nd"
      } else if (isTRUE(fuori)) {
        "report-bad"
      } else {
        "report-ok"
      }
      
      stato <- if (!is.finite(valore)) {
        "N/D"
      } else if (isTRUE(fuori)) {
        "Diverso dal solito"
      } else {
        "Abituale"
      }
      
      tagList(
        tags$span(
          class = classe,
          if (is.finite(valore)) {
            formatC(
              valore,
              format = "f",
              digits = 3,
              decimal.mark = ","
            )
          } else {
            "N/D"
          }
        ),
        tags$span(
          class = paste(
            "report-status-pill",
            if (isTRUE(fuori)) "report-status-bad" else "report-status-ok"
          ),
          stato
        )
      )
    }
    
    colonne <- c(
      lapply(seq_len(nrow(sensori)), function(i) {
        srow <- sensori[i, ]
        list(
          nome = srow$sensore_label,
          att = paste0(
            formatC(
              srow$attivazioni_medie,
              format = "f",
              digits = 1,
              decimal.mark = ","
            ),
            " (",
            formatta_variazione_report(
              srow$variazione_attivazioni_pct
            ),
            ")"
          ),
          nok = cella_nok(
            srow$NOK,
            srow$nok_fuori_range
          )
        )
      }),
      list(list(
        nome = "Macchina",
        att = paste0(
          formatC(
            r$att_macchina,
            format = "f",
            digits = 1,
            decimal.mark = ","
          ),
          " (",
          formatta_variazione_report(r$att_macchina_pct),
          ")"
        ),
        nok = cella_nok(
          r$nok_macchina,
          r$nok_macchina_fuori
        )
      ))
    )
    
    tabella_sintesi <- div(
      class = "report-table-wrap",
      tags$table(
        class = "report-table",
        tags$thead(
          tags$tr(
            tags$th("Metrica"),
            lapply(colonne, function(x) tags$th(x$nome))
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Manovre medie")),
            lapply(colonne, function(x) tags$td(x$att))
          ),
          tags$tr(
            tags$td(tags$strong("Utilizzo / storico")),
            lapply(colonne, function(x) tags$td(x$nok))
          )
        )
      )
    )
    
    tabella_html <- function(df, vuoto) {
      if (nrow(df) == 0) {
        return(div(class = "alarm-empty", vuoto))
      }
      
      div(
        class = "report-table-wrap",
        tags$table(
          class = "report-table",
          tags$thead(
            tags$tr(lapply(names(df), tags$th))
          ),
          tags$tbody(
            lapply(seq_len(nrow(df)), function(i) {
              tags$tr(
                lapply(
                  df[i, , drop = FALSE],
                  function(x) tags$td(as.character(x))
                )
              )
            })
          )
        )
      )
    }
    
    utilizzo_val <- if (is.finite(r$utilizzo)) {
      formatC(
        r$utilizzo,
        format = "f",
        digits = 3,
        decimal.mark = ","
      )
    } else {
      "N/D"
    }
    
    nok_val <- if (is.finite(r$nok_macchina)) {
      formatC(
        r$nok_macchina,
        format = "f",
        digits = 3,
        decimal.mark = ","
      )
    } else {
      "N/D"
    }
    
    n_att <- nrow(r$attivazioni_anomale)
    n_nok <- nrow(r$nok_anomalo)
    
    sezione <- function(numero, titolo, nota, contenuto) {
      div(
        class = "report-section",
        div(
          class = "report-section-heading",
          span(numero, class = "report-section-number"),
          h4(titolo, class = "report-section-title")
        ),
        div(nota, class = "report-section-note"),
        contenuto
      )
    }
    
    div(
      class = "report-modal",
      div(
        class = "report-hero",
        div(
          class = "report-modal-header",
          div(
            class = "report-heading-left",
            actionButton(
              "report_back_to_alarms",
              label = NULL,
              icon = icon("arrow-left"),
              class = "report-back-btn",
              title = "Torna agli allarmi",
              `aria-label` = "Torna agli allarmi"
            ),
            div(
              h2("Rapporto anomalie e utilizzo", class = "report-title"),
              div(
                paste0(
                  r$macchina,
                  " · Coupon ",
                  r$coupon
                ),
                class = "report-subtitle"
              )
            )
          ),
          downloadButton(
            "download_report_pdf",
            "Scarica PDF",
            icon = icon("file-pdf"),
            class = "report-download"
          )
        ),
        div(
          class = "report-meta",
          span(
            icon("calendar"),
            paste(
              format(r$inizio, "%d/%m/%Y"),
              format(r$fine, "%d/%m/%Y"),
              sep = " - "
            ),
            class = "report-meta-chip"
          ),
          span(
            icon("clock"),
            paste0(
              "Confronto: ",
              format(r$precedente_inizio, "%d/%m/%Y"),
              " - ",
              format(r$precedente_fine, "%d/%m/%Y")
            ),
            class = "report-meta-chip"
          )
        )
      ),
      
      div(
        class = "report-kpi-grid",
        div(
          class = "report-kpi-card",
          div("Indice utilizzo", class = "report-kpi-label"),
          div(utilizzo_val, class = "report-kpi-value"),
          div(
            paste0(
              r$utilizzo_stato,
              " · ",
              formatta_variazione_report(r$utilizzo_pct)
            ),
            class = "report-kpi-note"
          )
        ),
        div(
          class = "report-kpi-card",
          div("Utilizzo medio rispetto allo storico", class = "report-kpi-label"),
          div(nok_val, class = "report-kpi-value"),
          div(
            paste0(
              ifelse(
                isTRUE(r$nok_macchina_fuori),
                "Diverso dal solito",
                "Abituale"
              ),
              " · ",
              formatta_variazione_report(r$nok_macchina_pct)
            ),
            class = "report-kpi-note"
          )
        ),
        div(
          class = "report-kpi-card",
          div("Anomalie manovre", class = "report-kpi-label"),
          div(n_att, class = "report-kpi-value"),
          div("Eventi rilevanti nel periodo", class = "report-kpi-note")
        ),
        div(
          class = "report-kpi-card",
          div("Periodi con utilizzo insolito", class = "report-kpi-label"),
          div(nrow(r$utilizzo_anomalo), class = "report-kpi-value"),
          div(
            "Segnalazioni di utilizzo",
            class = "report-kpi-note"
          )
        )
      ),
      
      div(
        class = "report-summary",
        div("Lettura del periodo", class = "report-summary-title"),
        lapply(narrativa, function(x) tags$p(x))
      ),
      
      sezione(
        "1",
        "Sintesi per componente e macchina",
        "Le manovre sono la media giornaliera del periodo. Tra parentesi è indicato il cambiamento rispetto al periodo precedente. Il confronto con lo storico distingue l’utilizzo abituale da quello insolito.",
        tabella_sintesi
      ),
      
      sezione(
        "2",
        "Manovre anomale rilevanti",
        "Dettaglio degli eventi di manovra anomali rilevati nel periodo.",
        tabella_html(
          r$attivazioni_anomale |>
            mutate(Data = format(Data, "%d/%m/%Y")),
          "Nessuna manovra anomala rilevante nel periodo."
        )
      ),
      
      sezione(
        "3",
        "Utilizzo della macchina diverso dal solito",
        "Questa sezione mostra i periodi in cui l’utilizzo complessivo della macchina è più alto o più basso del suo intervallo abituale.",
        tabella_html(
          r$utilizzo_anomalo,
          "L’utilizzo complessivo della macchina è in linea con il suo storico nel periodo."
        )
      )
    )
  })
  
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      r <- report_dati()
      paste0(
        "report_anomalie_",
        r$coupon,
        "_",
        format(r$inizio, "%Y%m%d"),
        "_",
        format(r$fine, "%Y%m%d"),
        ".pdf"
      )
    },
    content = function(file) {
      r <- report_dati()
      narrativa <- testo_report_esteso(r)
      
      grDevices::pdf(
        file,
        width = 11.69,
        height = 8.27,
        onefile = TRUE
      )
      on.exit(grDevices::dev.off(), add = TRUE)
      
      navy <- "#234A66"
      slate <- "#334155"
      muted <- "#718096"
      beige <- "#F1F3F4"
      beige_dark <- "#EBBD55"
      border <- "#DDE4EA"
      green <- "#19764A"
      red <- "#B42318"
      
      titolo_base <- "Rapporto anomalie e utilizzo"
      sottotitolo <- paste0(
        r$macchina,
        " · Coupon ",
        r$coupon,
        " · ",
        format(r$inizio, "%d/%m/%Y"),
        " - ",
        format(r$fine, "%d/%m/%Y")
      )
      
      footer <- function() {
        grid::grid.text(
          paste0(
            "5SAN · Report generato il ",
            format(Sys.time(), "%d/%m/%Y %H:%M")
          ),
          x = 0.03,
          y = 0.025,
          just = c("left", "bottom"),
          gp = grid::gpar(
            fontsize = 7.5,
            col = muted
          )
        )
      }
      
      header_pagina <- function(titolo_sezione = NULL) {
        grid::grid.rect(
          x = 0.5,
          y = 0.955,
          width = 1,
          height = 0.09,
          gp = grid::gpar(
            fill = navy,
            col = NA
          )
        )
        grid::grid.text(
          "5SAN · MONITORAGGIO OPERATIVO",
          x = 0.035,
          y = 0.972,
          just = c("left", "top"),
          gp = grid::gpar(
            fontsize = 7.5,
            fontface = "bold",
            col = "#DDD6BD"
          )
        )
        grid::grid.text(
          if (is.null(titolo_sezione)) titolo_base else titolo_sezione,
          x = 0.035,
          y = 0.945,
          just = c("left", "top"),
          gp = grid::gpar(
            fontsize = 15,
            fontface = "bold",
            col = "#FFFFFF"
          )
        )
        footer()
      }
      
      kpi_card <- function(x, label, value, note, status = "normal") {
        status_col <- if (identical(status, "bad")) red else if (
          identical(status, "good")
        ) green else navy
        
        grid::grid.roundrect(
          x = x,
          y = 0.685,
          width = 0.215,
          height = 0.145,
          r = grid::unit(0.035, "snpc"),
          gp = grid::gpar(
            fill = "#FFFFFF",
            col = border,
            lwd = 1
          )
        )
        grid::grid.text(
          label,
          x = x - 0.092,
          y = 0.73,
          just = c("left", "center"),
          gp = grid::gpar(
            fontsize = 7.5,
            fontface = "bold",
            col = muted
          )
        )
        grid::grid.text(
          value,
          x = x - 0.092,
          y = 0.688,
          just = c("left", "center"),
          gp = grid::gpar(
            fontsize = 16,
            fontface = "bold",
            col = status_col
          )
        )
        grid::grid.text(
          note,
          x = x - 0.092,
          y = 0.647,
          just = c("left", "center"),
          gp = grid::gpar(
            fontsize = 7.3,
            col = muted
          )
        )
      }
      
      # Pagina 1 - Executive summary.
      grid::grid.newpage()
      grid::grid.rect(
        x = 0.5,
        y = 0.90,
        width = 1,
        height = 0.20,
        gp = grid::gpar(fill = navy, col = NA)
      )
      grid::grid.text(
        "5SAN · MONITORAGGIO OPERATIVO",
        x = 0.04,
        y = 0.965,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 8,
          fontface = "bold",
          col = "#DDD6BD"
        )
      )
      grid::grid.text(
        titolo_base,
        x = 0.04,
        y = 0.925,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 23,
          fontface = "bold",
          col = "#FFFFFF"
        )
      )
      grid::grid.text(
        sottotitolo,
        x = 0.04,
        y = 0.865,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 10,
          col = "#DDE5EC"
        )
      )
      grid::grid.text(
        paste0(
          "Confronto: ",
          format(r$precedente_inizio, "%d/%m/%Y"),
          " - ",
          format(r$precedente_fine, "%d/%m/%Y"),
          ""
        ),
        x = 0.04,
        y = 0.832,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 8,
          col = "#DDE5EC"
        )
      )
      
      utilizzo_val <- if (is.finite(r$utilizzo)) {
        formatC(r$utilizzo, format = "f", digits = 3, decimal.mark = ",")
      } else "N/D"
      nok_val <- if (is.finite(r$nok_macchina)) {
        formatC(r$nok_macchina, format = "f", digits = 3, decimal.mark = ",")
      } else "N/D"
      
      kpi_card(
        0.145,
        "INDICE UTILIZZO",
        utilizzo_val,
        paste0(r$utilizzo_stato, " · ", formatta_variazione_report(r$utilizzo_pct)),
        if (r$utilizzo_stato %in% c("Elevato", "Basso")) "bad" else "good"
      )
      kpi_card(
        0.385,
        "UTILIZZO / STORICO",
        nok_val,
        paste0(
          ifelse(isTRUE(r$nok_macchina_fuori), "Diverso dal solito", "Abituale"),
          " · ",
          formatta_variazione_report(r$nok_macchina_pct)
        ),
        if (isTRUE(r$nok_macchina_fuori)) "bad" else "good"
      )
      kpi_card(
        0.625,
        "ANOMALIE MANOVRE",
        as.character(nrow(r$attivazioni_anomale)),
        "Eventi rilevanti",
        if (nrow(r$attivazioni_anomale) > 0) "bad" else "good"
      )
      kpi_card(
        0.865,
        "PERIODI CON USO INSOLITO",
        as.character(nrow(r$utilizzo_anomalo)),
        "Segnalazioni di utilizzo",
        if (nrow(r$utilizzo_anomalo) > 0) "bad" else "good"
      )
      
      grid::grid.text(
        "Lettura del periodo",
        x = 0.04,
        y = 0.57,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 13,
          fontface = "bold",
          col = navy
        )
      )
      grid::grid.rect(
        x = 0.5,
        y = 0.36,
        width = 0.92,
        height = 0.38,
        gp = grid::gpar(
          fill = beige,
          col = "#D9DDE0"
        )
      )
      
      testo_exec <- paste(
        vapply(
          narrativa,
          function(x) paste(strwrap(x, width = 155), collapse = "\n"),
          character(1)
        ),
        collapse = "\n\n"
      )
      grid::grid.text(
        testo_exec,
        x = 0.06,
        y = 0.53,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 9,
          col = slate,
          lineheight = 1.25
        )
      )
      footer()
      
      # Sintesi per sensore: più pagine, 7 sensori alla volta + macchina.
      sensori_summary <- r$confronto |>
        transmute(
          Componente = cds_name,
          Manovre = paste0(
            formatC(attivazioni_medie, format = "f", digits = 1, decimal.mark = ","),
            " (",
            vapply(
              variazione_attivazioni_pct,
              formatta_variazione_report,
              character(1)
            ),
            ")"
          ),
          NOK = ifelse(
            is.finite(NOK),
            formatC(NOK, format = "f", digits = 3, decimal.mark = ","),
            "N/D"
          ),
          Stato = ifelse(nok_fuori_range, "INSOLITO", "ABITUALE")
        )
      
      idx_chunks <- split(
        seq_len(nrow(sensori_summary)),
        ceiling(seq_len(nrow(sensori_summary)) / 7)
      )
      
      if (length(idx_chunks) == 0) idx_chunks <- list(integer(0))
      
      for (chunk_i in seq_along(idx_chunks)) {
        idx <- idx_chunks[[chunk_i]]
        chunk <- sensori_summary[idx, , drop = FALSE]
        
        metriche <- c("Manovre medie", "Utilizzo / storico")
        tab <- data.frame(Metrica = metriche, check.names = FALSE)
        
        if (nrow(chunk) > 0) {
          for (i in seq_len(nrow(chunk))) {
            tab[[chunk$Componente[i]]] <- c(
              chunk$Manovre[i],
              paste0(chunk$NOK[i], " · ", chunk$Stato[i])
            )
          }
        }
        
        if (chunk_i == length(idx_chunks)) {
          tab[["Macchina"]] <- c(
            paste0(
              formatC(r$att_macchina, format = "f", digits = 1, decimal.mark = ","),
              " (",
              formatta_variazione_report(r$att_macchina_pct),
              ")"
            ),
            paste0(
              nok_val,
              " · ",
              ifelse(isTRUE(r$nok_macchina_fuori), "FUORI", "OK")
            )
          )
        }
        
        grid::grid.newpage()
        header_pagina("Sintesi per componente e macchina")
        grid::grid.text(
          paste0(
            "Valori medi del periodo; tra parentesi la variazione rispetto al periodo precedente. ",
            "Il confronto con lo storico distingue l’utilizzo abituale da quello insolito."
          ),
          x = 0.04,
          y = 0.84,
          just = c("left", "top"),
          gp = grid::gpar(fontsize = 8.5, col = muted)
        )
        
        tg <- gridExtra::tableGrob(
          tab,
          rows = NULL,
          theme = gridExtra::ttheme_minimal(
            base_size = 8,
            core = list(
              fg_params = list(col = slate),
              bg_params = list(fill = c("#FFFFFF", "#FAFBFC"))
            ),
            colhead = list(
              fg_params = list(fontface = "bold", col = navy),
              bg_params = list(fill = "#EDF1F4")
            )
          )
        )
        grid::pushViewport(
          grid::viewport(
            x = 0.04,
            y = 0.79,
            width = 0.92,
            height = 0.60,
            just = c("left", "top")
          )
        )
        grid::grid.draw(tg)
        grid::popViewport()
      }
      
      disegna_tabella_paginata <- function(titolo_sezione, df, empty_text) {
        if (nrow(df) == 0) {
          grid::grid.newpage()
          header_pagina(titolo_sezione)
          grid::grid.roundrect(
            x = 0.5,
            y = 0.60,
            width = 0.88,
            height = 0.18,
            r = grid::unit(0.03, "snpc"),
            gp = grid::gpar(fill = beige, col = "#D9DDE0")
          )
          grid::grid.text(
            empty_text,
            x = 0.5,
            y = 0.60,
            gp = grid::gpar(
              fontsize = 10,
              col = slate
            )
          )
          return(invisible(NULL))
        }
        
        chunks <- split(
          seq_len(nrow(df)),
          ceiling(seq_len(nrow(df)) / 12)
        )
        
        for (idx in chunks) {
          grid::grid.newpage()
          header_pagina(titolo_sezione)
          
          tg <- gridExtra::tableGrob(
            as.data.frame(df[idx, , drop = FALSE]),
            rows = NULL,
            theme = gridExtra::ttheme_minimal(
              base_size = 7.4,
              core = list(
                fg_params = list(col = slate, hjust = 0, x = 0.03),
                bg_params = list(fill = rep(c("#FFFFFF", "#FAFBFC"), length.out = length(idx)))
              ),
              colhead = list(
                fg_params = list(fontface = "bold", col = navy),
                bg_params = list(fill = "#EDF1F4")
              )
            )
          )
          
          grid::pushViewport(
            grid::viewport(
              x = 0.035,
              y = 0.84,
              width = 0.93,
              height = 0.70,
              just = c("left", "top")
            )
          )
          grid::grid.draw(tg)
          grid::popViewport()
        }
      }
      
      att_pdf <- r$attivazioni_anomale |>
        mutate(Data = format(Data, "%d/%m/%Y"))
      
      disegna_tabella_paginata(
        "Manovre anomale rilevanti",
        att_pdf,
        "Nessuna manovra anomala rilevante nel periodo."
      )
      
      disegna_tabella_paginata(
        "Utilizzo della macchina diverso dal solito",
        r$utilizzo_anomalo,
        "L’utilizzo complessivo della macchina è in linea con il suo storico."
      )
    },
    contentType = "application/pdf"
  )
  
  output$modal_panel_dati_attivazioni <- renderUI({
    if (!mostra_dati_attivazioni()) return(NULL)
    div(class = "modal-data-panel", DTOutput("modal_tabella_dati_attivazioni"))
  })
  
  output$modal_panel_dati_trend <- renderUI({
    if (!mostra_dati_trend()) return(NULL)
    div(class = "modal-data-panel", DTOutput("modal_tabella_dati_trend"))
  })
  
  output$modal_panel_dati_tank <- renderUI({
    if (!mostra_dati_tank()) return(NULL)
    div(class = "modal-data-panel", DTOutput("modal_tabella_dati_tank"))
  })
  
  output$modal_panel_dati_nok <- renderUI({
    if (!mostra_dati_nok()) return(NULL)
    div(class = "modal-data-panel", DTOutput("modal_tabella_dati_nok"))
  })
  
  output$modal_panel_dati_profilo_nok <- renderUI({
    if (!mostra_dati_profilo_nok()) return(NULL)
    div(
      class = "modal-data-panel",
      DTOutput("modal_tabella_dati_profilo_nok")
    )
  })
  
  output$modal_panel_storico_utilizzo_nok <- renderUI({
    if (!mostra_storico_utilizzo_nok()) return(NULL)
    
    div(
      class = "modal-data-panel",
      girafeOutput(
        "modal_storicoUtilizzoPlot",
        height = "300px"
      )
    )
  })
  
  output$modal_panel_outlier_nok <- renderUI({
    if (!mostra_outlier_nok()) return(NULL)
    
    div(
      class = "modal-data-panel",
      p(
        "Manovre anomale rilevate nel periodo selezionato. ",
        "La tabella riunisce le segnalazioni giornaliere e quelle riferite a una singola ora. Le giornate con anomalie confermate non vengono usate per definire l’utilizzo abituale del componente."
      ),
      DTOutput("modal_tabella_outlier_nok")
    )
  })
  
