  # ---------------------------------------------------------------------
  # Utilizzo macchina dal NOK.
  # Usa gli stessi dati giornalieri gia' filtrati per il calcolo del NOK.
  #
  # U_sensore  = sqrt(media(NOK)^2 + varianza(NOK))
  # U_macchina = media(U_sensore)
  # ---------------------------------------------------------------------
  giorno_utilizzo <- reactive({
    invalidateLater(60000, session)
    as.Date(Sys.time(), tz = "Europe/Rome")
  })

  storico_utilizzo_macchina <- reactive({
    granularita_utilizzo <- granularita_utilizzo_corrente()
    machine_daily <- prepara_valori_giornalieri_nok_data(dati, macchina_attiva())
    valid_days <- machine_daily$day[!is.na(machine_daily$day)]
    starts <- if (length(valid_days)) seq.Date(
      as.Date(floor_date(min(valid_days), if (granularita_utilizzo == "Mese") "month" else "week", week_start = 1)),
      as.Date(floor_date(max(valid_days), if (granularita_utilizzo == "Mese") "month" else "week", week_start = 1)),
      by = if (granularita_utilizzo == "Mese") "month" else "7 days") else as.Date(character())
    saved <- alerts_correnti() |>
      filter(coupon == macchina_attiva(), alert_type %in% c("utilization_weekly", "utilization_monthly"))
    storico_utilizzo_grafico <- lapply(seq_along(starts), function(i) {
      first <- starts[i]
      last <- if (granularita_utilizzo == "Mese") first %m+% months(1) - days(1) else first + 6L
      assessment <- valuta_utilizzo_periodo(machine_daily, first, last, granularita_utilizzo)
      value <- lower <- upper <- NA_real_
      references <- 0L
      if (nrow(assessment)) {
        value <- assessment$utilization[1]
        lower <- assessment$lower_limit[1]
        upper <- assessment$upper_limit[1]
        references <- assessment$reference_weeks[1]
      }
      snapshot <- saved |> filter(
        as.Date(period_start, tz = "Europe/Rome") == first,
        as.Date(period_end, tz = "Europe/Rome") == last)
      if (nrow(snapshot) && is.finite(snapshot$utilization_value[1])) {
        value <- snapshot$utilization_value[1]
        lower <- snapshot$utilization_lower[1]
        upper <- snapshot$utilization_upper[1]
        references <- snapshot$utilization_references[1]
      }
      state <- case_when(
        last >= as.Date(Sys.time(), tz = "Europe/Rome") ~ "In corso",
        !is.finite(value) | !is.finite(lower) | !is.finite(upper) ~ "N/D",
        value < lower ~ "Basso", value > upper ~ "Elevato", TRUE ~ "Normale")
      tibble(inizio_finestra = first, fine_finestra = last, U_macchina = value,
        limite_inf = lower, limite_sup = upper, n_riferimenti = references,
        tipo = "Riferimento",
        stato = state)
    }) |> bind_rows()
    if (!nrow(storico_utilizzo_grafico)) storico_utilizzo_grafico <- tibble(
      inizio_finestra = as.Date(character()), fine_finestra = as.Date(character()),
      U_macchina = double(), limite_inf = double(), limite_sup = double(),
      n_riferimenti = integer(), tipo = character(), stato = character())
    storico_utilizzo_grafico
  }) |> bindCache(
    macchina_attiva(), granularita_utilizzo_corrente(), alerts_correnti(),
    giorno_utilizzo(), cache = "session"
  )

  utilizzo_nok_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    base_completa <- valori_giornalieri_modal_nok() |>

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
      filter(is.finite(NOK_giornaliero))
    
    # Utilizzo del periodo selezionato: invariato.
    base <- base_completa |>
      filter(
        day >= filtri$date[1],
        day <= filtri$date[2]
      )
    
    per_sensore <- base |>
      group_by(cds_name, sensor_description) |>
      summarise(
        n_osservazioni = n(),
        media_nok = mean(NOK_giornaliero, na.rm = TRUE),
        varianza_nok = if (n() >= 2) {
          var(NOK_giornaliero, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        U_sensore = sqrt(media_nok^2 + varianza_nok),
        sd_nok = sqrt(varianza_nok),
        banda_min = pmax(0, media_nok - sd_nok),
        banda_max = media_nok + sd_nok,
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")
      ) |>
      ordina_naturale()
    
    durata_giorni <- as.integer(
      filtri$date[2] - filtri$date[1]
    ) + 1L
    
    finestra_confronto <- periodo_precedente_grafici(
      filtri$date, valori_giornalieri_modal_nok()$day
    )
    periodo_precedente_fine <- finestra_confronto$fine
    periodo_precedente_inizio <- finestra_confronto$inizio
    
    per_sensore_confronto <- base_completa |>
      mutate(
        confronto = case_when(
          day >= periodo_precedente_inizio &
            day <= periodo_precedente_fine ~ "Periodo precedente",
          day >= filtri$date[1] &
            day <= filtri$date[2] ~ "Periodo selezionato",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(confronto)) |>
      mutate(
        confronto = factor(
          confronto,
          levels = c("Periodo precedente", "Periodo selezionato")
        )
      ) |>
      group_by(
        confronto,
        cds_name,
        sensor_description
      ) |>
      summarise(
        n_osservazioni = n(),
        media_nok = mean(NOK_giornaliero, na.rm = TRUE),
        varianza_nok = if (n() >= 2) {
          var(NOK_giornaliero, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        U_sensore = sqrt(media_nok^2 + varianza_nok),
        sd_nok = sqrt(varianza_nok),
        banda_min = pmax(0, media_nok - sd_nok),
        banda_max = media_nok + sd_nok,
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")
      ) |>
      ordina_naturale()
    
    valori_u <- per_sensore$U_sensore[
      is.finite(per_sensore$U_sensore)
    ]
    
    U_macchina <- if (length(valori_u) > 0) {
      mean(valori_u)
    } else {
      NA_real_
    }
    
    # Identico riferimento di utilizzo in grafici e segnalazioni:
    # tutti i componenti della macchina, periodi di calendario completi,
    # solo periodi precedenti e P5-P95 secondo la soglia configurata.
    granularita_utilizzo <- granularita_utilizzo_corrente()
    storico_utilizzo_grafico <- storico_utilizzo_macchina() |>
      mutate(tipo = if_else(
        fine_finestra >= filtri$date[1] & inizio_finestra <= filtri$date[2],
        "Periodo selezionato", "Riferimento"))
    selected <- storico_utilizzo_grafico |> filter(tipo == "Periodo selezionato")
    # Campi legacy: contengono ora esclusivamente limiti P5-P95.
    P10_periodo <- if (nrow(selected) == 1L) selected$limite_inf[1] else NA_real_
    P90_periodo <- if (nrow(selected) == 1L) selected$limite_sup[1] else NA_real_
    P10_utilizzo <- P10_periodo
    P90_utilizzo <- P90_periodo
    stato_utilizzo <- if (nrow(selected) == 1L) selected$stato[1] else "N/D"
    U_macchina <- if (nrow(selected) == 1L) selected$U_macchina[1] else NA_real_
    U_storici <- storico_utilizzo_grafico$U_macchina[storico_utilizzo_grafico$tipo == "Riferimento"]
    quota_storico_selezionata <- NA_real_
    manca_finestra_precedente <- !length(U_storici)
    includi_periodo_nella_soglia <- FALSE

    list(
      per_sensore = per_sensore,
      per_sensore_confronto = per_sensore_confronto,
      etichette_confronto = finestra_confronto$etichette,
      confronto_inizio = finestra_confronto$inizio,
      confronto_fine = finestra_confronto$fine,
      U_macchina = U_macchina,
      P10_utilizzo = P10_utilizzo,
      P90_utilizzo = P90_utilizzo,
      stato_utilizzo = stato_utilizzo,
      storico_utilizzo_grafico = storico_utilizzo_grafico,
      granularita_utilizzo = granularita_utilizzo,
      P10_periodo = P10_periodo,
      P90_periodo = P90_periodo,
      n_finestre_storiche = length(U_storici),
      n_settimane_storiche = length(U_storici),
      durata_finestra_giorni = durata_giorni,
      quota_storico_selezionata = quota_storico_selezionata,
      manca_finestra_precedente = manca_finestra_precedente,
      includi_periodo_nella_soglia = includi_periodo_nella_soglia
    )
  }) |>
    bindCache(
      filtri_nok_modal()$macchina,
      filtri_nok_modal()$date,
      filtri_nok_modal()$sensori,
      granularita_utilizzo_corrente(),
      storico_utilizzo_macchina(), cache = "session"
    )
  
  output$modal_utilizzo_macchina <- renderUI({
    
    utilizzo <- utilizzo_nok_modal()
    granularita <- utilizzo$granularita_utilizzo
    
    periodi <- utilizzo$storico_utilizzo_grafico |>
      filter(tipo == "Periodo selezionato") |>
      arrange(inizio_finestra)
    
    soglia_bassa <- if (
      length(utilizzo$P10_periodo) == 0 ||
      !is.finite(utilizzo$P10_periodo)
    ) {
      "N/D"
    } else {
      formatC(
        utilizzo$P10_periodo,
        format = "f",
        digits = 2,
        decimal.mark = ","
      )
    }
    
    soglia_alta <- if (
      length(utilizzo$P90_periodo) == 0 ||
      !is.finite(utilizzo$P90_periodo)
    ) {
      "N/D"
    } else {
      formatC(
        utilizzo$P90_periodo,
        format = "f",
        digits = 2,
        decimal.mark = ","
      )
    }
    
    cards_periodo <- lapply(seq_len(nrow(periodi)), function(i) {
      riga <- periodi[i, ]
      
      colore <- switch(
        as.character(riga$stato),
        "Normale" = "#19764A",
        "Elevato" = "#A65A52",
        "Basso" = "#58758F",
        "#5F6F7F"
      )
      
      sfondo <- switch(
        as.character(riga$stato),
        "Normale" = "#F2FBF6",
        "Elevato" = "#FBF3F2",
        "Basso" = "#F2F6FA",
        "#F4F8FB"
      )
      
      # Mostra le date effettive anche quando la settimana o il mese e' spezzato.
      etichetta_periodo <- paste0(
        if (identical(granularita, "Mese")) "Mese " else "Sett. ",
        format(riga$inizio_finestra, "%d-%m"),
        " / ",
        format(riga$fine_finestra, "%d-%m")
      )
      
      div(
        style = paste0(
          "min-width:105px;",
          "padding:7px 9px;",
          "border:1px solid ", colore, ";",
          "border-radius:8px;",
          "background:", sfondo, ";",
          "text-align:center;"
        ),
        div(
          etichetta_periodo,
          style = "font-size:11px;font-weight:700;color:#4A4A4A;"
        ),
        div(
          if (is.finite(riga$U_macchina)) formatC(
            riga$U_macchina, format = "f", digits = 2, decimal.mark = ",") else "N/D",
          style = paste0(
            "font-size:22px;",
            "font-weight:800;",
            "line-height:1.2;",
            "color:", colore, ";"
          )
        ),
        div(
          paste0(riga$stato,
            if (is.finite(riga$limite_inf) && is.finite(riga$limite_sup))
              paste0(" · ", formatC(riga$limite_inf, digits = 2, format = "f"),
                "–", formatC(riga$limite_sup, digits = 2, format = "f")) else ""),
          style = paste0(
            "font-size:10px;",
            "font-weight:800;",
            "text-transform:uppercase;",
            "color:", colore, ";"
          )
        )
      )
    })
    
    tagList(
      div(
        style = paste0(
          "display:flex;",
          "align-items:flex-start;",
          "gap:8px;",
          "overflow-x:auto;",
          "padding-bottom:4px;"
        ),
        cards_periodo
      ),
      div(
        style = "margin-top:10px;",
        actionButton(
          "modal_btn_storico_utilizzo_nok",
          "Storico utilizzo",
          class = "btn-sm btn-default"
        )
      ),
      div(
        "Riassume quanto e con quale regolarità sono stati usati i componenti di sicurezza rispetto al passato. Un valore alto può indicare un utilizzo più intenso o meno regolare; un valore basso indica un utilizzo meno intenso o più regolare.",
        style = "margin-top:7px;font-size:13px;color:#5F6F7F;"
      ),
      div(
        paste0(
          "Intervallo normale ",
          if (
            identical(granularita, "Mese")
          ) "mensile" else "settimanale",
          ": ",
          if (nrow(periodi) == 1L) paste0(soglia_bassa, " – ", soglia_alta)
          else "specifico per ciascun periodo; riportato nelle tessere"
        ),
        style = "margin-top:4px;font-size:12px;color:#718096;"
      )
    )
  })
  
  render_storico_utilizzo_nok_gg <- function() {
    
    utilizzo <- utilizzo_nok_modal()
    storico <- utilizzo$storico_utilizzo_grafico
    granularita <- utilizzo$granularita_utilizzo
    
    validate(
      need(
        nrow(storico) > 0,
        "Nessun dato storico disponibile per l'utilizzo"
      )
    )
    
    selezionato <- storico |>
      filter(tipo == "Periodo selezionato")
    
    validate(
      need(
        nrow(selezionato) > 0,
        "Nessun periodo disponibile nella selezione"
      )
    )
    
    inizio_area <- min(selezionato$inizio_finestra, na.rm = TRUE)
    fine_area <- max(selezionato$fine_finestra, na.rm = TRUE)
    
    storico <- storico |>
      mutate(
        colore_punto = case_when(
          tipo == "Riferimento" ~ "Storico",
          stato == "Normale" ~ "Normale",
          stato == "Elevato" ~ "Elevato",
          stato == "Basso" ~ "Basso",
          TRUE ~ "N/D"
        ),
        etichetta_periodo = paste0(
          format(inizio_finestra, "%d-%m-%Y"),
          " / ",
          format(fine_finestra, "%d-%m-%Y")
        ),
        tooltip_utilizzo = paste0(
          "<b>",
          ifelse(
            tipo == "Periodo selezionato",
            paste0(
              if (
                identical(granularita, "Mese")
              ) "Mese: " else "Settimana: ",
              stato
            ),
            if (
              identical(granularita, "Mese")
            ) "Mese storico" else "Settimana storica"
          ),
          "</b><br/>",
          if (
            identical(granularita, "Mese")
          ) "Periodo: " else "Settimana: ",
          etichetta_periodo,
          "<br/>",
          "Utilizzo componenti di sicurezza: ", round(U_macchina, 3),
          "<br/>Intervallo normale: ", round(limite_inf, 3), "–", round(limite_sup, 3)
        )
      )
    
    ggplot(
      storico,
      aes(x = inizio_finestra, y = U_macchina)
    ) +
      geom_rect(
        aes(
          xmin = inizio_area,
          xmax = fine_area,
          ymin = -Inf,
          ymax = Inf
        ),
        inherit.aes = FALSE,
        fill = "#F6F1D8",
        alpha = 0.75
      ) +
      geom_line(
        color = "#B7BEC5",
        linewidth = 0.9
      ) +
      geom_segment(
        data = storico |> filter(tipo == "Periodo selezionato", is.finite(limite_inf)) |>
          tidyr::pivot_longer(c(limite_inf, limite_sup), values_to = "soglia"),
        aes(x = inizio_finestra, xend = fine_finestra, y = soglia, yend = soglia,
          linetype = "Limiti normali"),
        inherit.aes = FALSE, color = "#79A884", linewidth = 0.9
      ) +
      geom_point_interactive(
        aes(
          color = colore_punto,
          tooltip = tooltip_utilizzo,
          data_id = paste(
            tipo,
            inizio_finestra,
            sep = "__"
          )
        ),
        size = 3.2
      ) +
      scale_color_manual(
        values = c(
          "Storico" = "#B7BEC5",
          "Normale" = "#19764A",
          "Elevato" = "#A65A52",
          "Basso" = "#58758F",
          "N/D" = "#6E7781", "In corso" = "#6E7781"
        ),
        breaks = c(
          "Storico",
          "Normale",
          "Elevato",
          "Basso"
        ),
        name = NULL
      ) +
      scale_linetype_manual(
        values = c(
          "Limiti normali" = "dashed"
        ),
        name = NULL
      ) +
      scale_x_date(
        breaks = storico$inizio_finestra,
        labels = function(x) {
          if (identical(granularita, "Mese")) {
            format(
              as.Date(x, origin = "1970-01-01"),
              "%m-%Y"
            )
          } else {
            format(
              as.Date(x, origin = "1970-01-01"),
              "%d-%m-%Y"
            )
          }
        }
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 6),
        labels = scales::label_number(
          accuracy = 0.1,
          decimal.mark = ","
        ),
        expand = expansion(mult = c(0.03, 0.06))
      ) +
      labs(
        x = NULL,
        y = "Utilizzo componenti di sicurezza"
      ) +
      tema_san(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(
          color = "#E1E6EA",
          linewidth = 0.4
        ),
        axis.text.x = element_text(
          size = 9,
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.justification = "left"
      )
  }
  
  output$modal_storicoUtilizzoPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    girafe(
      ggobj = render_storico_utilizzo_nok_gg(),
      width_svg = w_px / 72,
      height_svg = 260 / 72,
      options = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.85;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
  render_utilizzo_nok_gg <- function() {
    
    utilizzo <- utilizzo_nok_modal()
    profilo <- utilizzo$per_sensore_confronto |>
      filter(
        is.finite(media_nok),
        is.finite(varianza_nok)
      )
    
    validate(
      need(
        nrow(profilo) > 0,
        "Dati insufficienti per confrontare periodo precedente e periodo selezionato"
      )
    )
    
    sensori_ordinati <- profilo |>
      distinct(cds_name, sensor_description, etichetta_sensore) |>
      ordina_naturale() |>
      pull(etichetta_sensore)
    
    etichette_asse <- profilo |>
      distinct(cds_name, etichetta_sensore) |>
      arrange(match(etichetta_sensore, sensori_ordinati)) |>
      pull(cds_name)
    # Stessa scala nei due pannelli; spazio minimo anche se la variabilita' e' nulla.
    valori_asse <- c(1, profilo$media_nok, profilo$banda_min, profilo$banda_max)
    valori_asse <- valori_asse[is.finite(valori_asse)]
    estremi <- range(valori_asse)
    centro <- mean(estremi)
    ampiezza <- max(diff(estremi), 0.5, abs(centro) * 0.20)
    limiti_y <- c(max(0, centro - ampiezza * 0.60), centro + ampiezza * 0.60)

    profilo <- profilo |>
      mutate(
        x = match(etichetta_sensore, sensori_ordinati),
        tooltip_utilizzo = paste0(
          "<b>", confronto, "</b><br/>",
          "<b>", etichetta_sensore, "</b><br/>",
          "Utilizzo medio rispetto allo storico: ", round(media_nok, 4), "<br/>",
          "Variazioni nell’utilizzo (più basso = più regolare): ", round(varianza_nok, 4), "<br/>",
          "Utilizzo componente: ", round(U_sensore, 4)
        )
      )
    
    ggplot(profilo, aes(x = x)) +
      geom_hline(yintercept = 1, colour = "#A9B2BA", linetype = "dashed", linewidth = 0.5) +
      geom_ribbon(
        aes(ymin = banda_min, ymax = banda_max),
        fill = "#5F748C",
        alpha = 0.22
      ) +
      geom_line(
        aes(y = media_nok),
        color = "#234A66",
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(
          y = media_nok,
          tooltip = tooltip_utilizzo,
          data_id = paste(confronto, etichetta_sensore, sep = "__")
        ),
        color = "#234A66",
        size = 3.4
      ) +
      facet_grid(
        cols = vars(confronto),
        labeller = labeller(confronto = utilizzo$etichette_confronto),
        scales = "free_x",
        space = "free_x"
      ) +
      scale_x_continuous(
        breaks = seq_along(sensori_ordinati),
        labels = stringr::str_wrap(etichette_asse, width = 12),
        limits = c(0.4, length(sensori_ordinati) + 0.6)
      ) +
      scale_y_continuous(
        limits = limiti_y,
        breaks = scales::pretty_breaks(n = 6),
        labels = scales::label_number(decimal.mark = ",", big.mark = "."),
        expand = expansion(mult = c(0.02, 0.04))
      ) +
      labs(
        x = NULL,
        y = "NOK",
        caption = "Punto e linea: media · Area: variabilità giornaliera · Linea tratteggiata: riferimento storico (NOK = 1)"
      ) +
      tema_san(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(
          fill = "#F4F5F6",
          color = "#CDD2D6"
        ),
        strip.text = element_text(
          size = 13,
          face = "bold",
          color = "#234A66"
        ),
        axis.text.x = element_text(
          size = 11,
          angle = 0,
          hjust = 0.5,
          vjust = 1
        ),
        axis.text.y = element_text(size = 11),
        plot.caption = element_text(size = 10, colour = "#66717A", hjust = 0)
      )
  }
  
  output$modal_utilizzoNokPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    girafe(
      ggobj = render_utilizzo_nok_gg(),
      width_svg = w_px / 72,
      height_svg = 360 / 72,
      options = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
  render_nok_tempo_gg <- function() {
    
    andamento <- nok_temporale_confronto_modal()
    sensori <- andamento$sensori
    media_macchina <- andamento$media_macchina
    granularita <- filtri_nok_modal()$granularita
    
    validate(
      need(
        nrow(sensori) > 0,
        "Dati insufficienti per confrontare l’utilizzo con lo storico nel periodo selezionato"
      )
    )
    
    ggplot() +
      geom_line(
        data = sensori,
        aes(
          x = periodo,
          y = NOK,
          group = interaction(confronto, etichetta_sensore)
        ),
        color = "#C7CDD3",
        linewidth = 0.75,
        alpha = 0.75
      ) +
      geom_point_interactive(
        data = sensori,
        aes(
          x = periodo,
          y = NOK,
          tooltip = paste0(
            "<b>", etichetta_sensore, "</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            "NOK: ", round(NOK, 3)
          ),
          data_id = paste(
            confronto,
            etichetta_sensore,
            periodo,
            sep = "__"
          )
        ),
        color = "#C7CDD3",
        size = 1.6,
        alpha = 0.75
      ) +
      geom_line(
        data = media_macchina,
        aes(
          x = periodo,
          y = NOK,
          group = confronto
        ),
        color = "#234A66",
        linewidth = 1.5
      ) +
      geom_point_interactive(
        data = media_macchina,
        aes(
          x = periodo,
          y = NOK,
          tooltip = paste0(
            "<b>Media macchina</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            "Utilizzo medio rispetto allo storico: ", round(NOK, 3)
          ),
          data_id = paste(
            confronto,
            "media_macchina",
            periodo,
            sep = "__"
          )
        ),
        color = "#234A66",
        size = 2.8
      ) +
      geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "#9AA5B1",
        linewidth = 0.7
      ) +
      facet_grid(
        cols = vars(confronto),
        labeller = labeller(confronto = andamento$etichette_confronto),
        scales = "free_x",
        space = "free_x"
      ) +
      scale_x_date(
        breaks = function(limits) {
          periodi <- sort(unique(sensori$periodo))
          periodi[
            periodi >= as.Date(limits[1], origin = "1970-01-01") &
            periodi <= as.Date(limits[2], origin = "1970-01-01")
          ]
        },
        labels = function(x) {
          formatta_periodo_label(
            as.Date(x, origin = "1970-01-01"),
            granularita
          )
        }
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 7),
        labels = scales::label_number(
          accuracy = 0.1,
          decimal.mark = ","
        ),
        expand = expansion(mult = c(0.03, 0.06))
      ) +
      labs(
        x = NULL,
        y = "NOK"
      ) +
      tema_san(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(
          color = "#D7DDE2",
          linewidth = 0.4
        ),
        strip.background = element_rect(
          fill = "#F4F5F6",
          color = "#CDD2D6"
        ),
        strip.text = element_text(
          size = 13,
          face = "bold",
          color = "#234A66"
        ),
        axis.text.x = element_text(
          size = 10,
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        axis.text.y = element_text(size = 11)
      )
  }
  
  output$modal_utilizzoTempoPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    girafe(
      ggobj = render_nok_tempo_gg(),
      width_svg = w_px / 72,
      height_svg = 360 / 72,
      options = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
