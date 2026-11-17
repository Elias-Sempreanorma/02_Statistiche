  # ---------------------------------------------------------------------
  # Dati per il grafico attivazioni.
  # La base e' ORARIA: per ogni giorno considero la macchina accesa dalla
  # prima ora con un segnale fino all'ora dell'ultimo segnale compresa
  # (equivalente a terminare un'ora dopo l'ultimo segnale).
  # Per ogni sensore vengono quindi create anche le ore senza attivazioni.
  # Le granularita' da Giorno ad Anno derivano da questa stessa base.
  # ---------------------------------------------------------------------
  dati_attivazioni_orarie_modal <- reactive({
    
    filtri <- filtri_attivazioni_modal()
    req(length(filtri$sensori) > 0)
    
    sensori_selezionati <- sensori_lookup |>
      filter(
        coupon == filtri$macchina,
        cds_name %in% filtri$sensori
      ) |>
      select(cds_name, sensor_description)
    
    finestre_periodo <- finestre_orarie_macchina |>
      filter(
        coupon == filtri$macchina,
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      select(day, prima_ora, ultima_ora)
    
    validate(
      need(
        nrow(sensori_selezionati) > 0 &&
          (nrow(finestre_periodo) > 0 || any(
            dati_attivazioni_orarie_grafico$coupon == filtri$macchina &
            dati_attivazioni_orarie_grafico$cds_name %in% filtri$sensori &
            dati_attivazioni_orarie_grafico$day >= filtri$date[1] &
            dati_attivazioni_orarie_grafico$day <= filtri$date[2])),
        "Nessun dato disponibile per i filtri scelti"
      )
    )
    
    ore_macchina <- if (nrow(finestre_periodo) == 0) {
      tibble(day = as.Date(character()),
        hour = as.POSIXct(character(), tz = "Europe/Rome"))
    } else finestre_periodo |>
      rowwise() |>
      reframe(
        day = day,
        hour = seq(
          from = prima_ora,
          to = ultima_ora,
          by = "hour"
        )
      ) |>
      ungroup()
    
    conteggi_periodo <- dati_attivazioni_orarie_grafico |>
      filter(
        coupon == filtri$macchina,
        day >= filtri$date[1],
        day <= filtri$date[2],
        cds_name %in% filtri$sensori
      ) |>
      select(
        day,
        hour,
        cds_name,
        sensor_description,
        attivazioni_orarie,
        redistribuite,
        gateway_disconnect_affected
      )
    
    tidyr::crossing(
      ore_macchina,
      sensori_selezionati
    ) |>
      mutate(.ora_osservata = TRUE) |>
      full_join(
        conteggi_periodo,
        by = c("day", "hour", "cds_name", "sensor_description")
      ) |>
      mutate(
        attivazioni_orarie = coalesce(attivazioni_orarie, 0),
        redistribuite = coalesce(redistribuite, FALSE),
        gateway_disconnect_affected = coalesce(
          gateway_disconnect_affected,
          FALSE
        )
      ) |>
      left_join(
        dati_attivazioni_orarie_base |> filter(coupon == filtri$macchina) |>
          select(day, hour, cds_name, sensor_description, anomalia_oraria),
        by = c("day", "hour", "cds_name", "sensor_description")
      ) |>
      mutate(anomalia_oraria = coalesce(anomalia_oraria, FALSE)) |>
      arrange(hour, cds_name)
  }) |>
    bindCache(
      filtri_attivazioni_modal()$macchina,
      filtri_attivazioni_modal()$date,
      filtri_attivazioni_modal()$sensori
    )
  
  dati_grafico_modal <- reactive({
    
    filtri <- filtri_attivazioni_modal()
    orari <- dati_attivazioni_orarie_modal()
    
    validate(
      need(nrow(orari) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    giornalieri <- orari |>
      group_by(day, cds_name, sensor_description) |>
      summarise(
        attivazioni_giornaliere = sum(attivazioni_orarie, na.rm = TRUE),
        ore_macchina = sum(.ora_osservata %in% TRUE),
        redistribuite = any(redistribuite),
        gateway_disconnect_affected = any(
          gateway_disconnect_affected,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    if (identical(filtri$granularita, "Ora")) {
      risultato <- orari |>
        left_join(
          giornalieri |>
            select(day, cds_name, sensor_description, ore_macchina),
          by = c("day", "cds_name", "sensor_description")
        ) |>
        transmute(
          periodo = hour,
          cds_name,
          sensor_description,
          attivazioni = attivazioni_orarie,
          ore_aperte = ore_macchina,
          anomalia_oraria,
          redistribuite,
          gateway_disconnect_affected,
          etichetta_ore_aperte = "Ore macchina del giorno"
        )
    } else {
      risultato <- giornalieri |>
        mutate(
          periodo = periodo_bucket(day, filtri$granularita)
        ) |>
        group_by(periodo, cds_name, sensor_description) |>
        summarise(
          attivazioni = mean(attivazioni_giornaliere, na.rm = TRUE),
          ore_aperte = mean(ore_macchina, na.rm = TRUE),
          redistribuite = any(redistribuite),
          gateway_disconnect_affected = any(
            gateway_disconnect_affected,
            na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        mutate(
          anomalia_oraria = FALSE,
          etichetta_ore_aperte = if (
            identical(filtri$granularita, "Giorno")
          ) {
            "Ore macchina"
          } else {
            "Ore macchina medie/giorno"
          }
        )
    }
    
    risultato |>
      mutate(
        etichetta = cds_name,
        etichetta_completa = paste(
          cds_name,
          sensor_description,
          sep = " - "
        ),
        periodo_label = formatta_periodo_label(
          periodo,
          filtri$granularita
        )
      ) |>
      ordina_naturale() |>
      mutate(
        etichetta = factor(etichetta, levels = unique(etichetta)),
        etichetta_completa = factor(
          etichetta_completa,
          levels = unique(etichetta_completa)
        ),
        periodo_label = factor(
          periodo_label,
          levels = unique(periodo_label[order(periodo)])
        )
      )
  }) |>
    bindCache(filtri_attivazioni_modal())
  
  # CSS del tooltip, condiviso tra main e modal
  tooltip_css <- paste0(
    "background-color:#234A66;",
    "color:#FFFFFF;",
    "border-left:3px solid #EBBD55;",
    "padding:8px 12px;",
    "border-radius:6px;",
    "font-size:13px;",
    "line-height:1.6;",
    "box-shadow:0 2px 8px rgba(0,0,0,0.3);"
  )
  
  # Costruisce solo il ggplot (senza girafe), usato sia dal
  # grafico principale sia dal modal
  render_activation_bar_gg <- function() {
    
    grafico <- dati_grafico_modal() |>
      filter(is.finite(attivazioni), attivazioni != 0)
    
    validate(
      need(nrow(grafico) > 0, "Nessuna manovra diversa da zero nel periodo selezionato")
    )
    
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    palette_sensori <- palette_sensori_brand(n_sensori)
    padding_x <- if (n_sensori <= 1) {
      1.60
    } else if (n_sensori == 2) {
      0.80
    } else {
      0.35
    }
    
    ggplot(
      grafico,
      aes(x = etichetta, y = attivazioni, fill = etichetta_completa)
    ) +
      geom_rect(
        data = grafico |> filter(redistribuite) |> distinct(periodo_label),
        aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
          alpha = "Interruzione: origine e redistribuzione delle manovre"),
        inherit.aes = FALSE, fill = "#EBBD55", show.legend = TRUE
      ) +
      legenda_redistribuzione() +
      geom_col_interactive(
        aes(
          tooltip = paste0(
            "<b>", etichetta_completa, "</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            if_else(
              filtri_attivazioni_modal()$granularita == "Ora",
              "Manovre nell'ora: ",
              "Manovre giornaliere: "
            ),
            scales::label_number(
              accuracy = 0.1,
              big.mark = "."
            )(attivazioni),
            "<br/>", etichetta_ore_aperte, ": ",
            scales::label_number(accuracy = 0.1, decimal.mark = ",")(ore_aperte),
            " h",
            if_else(redistribuite, nota_redistribuzione, "")
          ),
          data_id = paste(etichetta_completa, periodo_label, sep = "__")
        ),
        width = 0.62
      ) +
      scale_x_discrete(
        expand = expansion(add = padding_x)
      ) +
      facet_grid(
        cols = vars(periodo_label),
        scales = "free_x",
        space = "free_x",
        switch = "x"
      ) +
      scala_y_manovre_visualizzate(grafico$attivazioni) +
      scale_fill_manual(values = palette_sensori, name = "Componente") +
      labs(title = NULL, x = NULL, y = "Manovre",
        caption = if (any(grafico$redistribuite))
          "Sfondo giallo: manovre orarie anomale redistribuite per interruzione della connessione." else NULL) +
      tema_san(base_size = 13) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.spacing.x = grid::unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(size = 13, angle = 90),
        axis.text.y = element_text(size = 13),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)
      )
  }
  
  # Curva morbida (spline) con gap-detection: segmenti densi restano fluidi,
  # i vuoti lunghi spezzano la linea invece di generare parabole.
  # I punti reali sono interattivi con tooltip.
  render_activation_trend_gg <- function() {
    
    grafico <- dati_grafico_modal() |>
      group_by(cds_name, sensor_description) |>
      filter(any(is.finite(attivazioni) & attivazioni != 0)) |>
      ungroup()
    granularita <- filtri_attivazioni_modal()$granularita
    
    validate(
      need(nrow(grafico) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    palette_sensori <- palette_sensori_alternata(n_sensori)
    
    tooltip_punti <- paste0(
      "<b>", grafico$etichetta_completa, "</b><br/>",
      "Periodo: ", grafico$periodo_label, "<br/>",
      if (identical(granularita, "Ora")) "Manovre nell'ora: " else "Manovre giornaliere: ",
      scales::label_number(
        accuracy = 1,
        big.mark = "."
      )(grafico$attivazioni),
      "<br/>", grafico$etichetta_ore_aperte, ": ",
      scales::label_number(accuracy = 0.1, decimal.mark = ",")(grafico$ore_aperte),
      " h",
      ifelse(grafico$redistribuite, nota_redistribuzione, "")
    )
    
    sfondo <- grafico |> filter(redistribuite) |>
      distinct(periodo) |> mutate(
        xmin = if (identical(granularita, "Ora")) periodo else periodo - 0.5,
        xmax = if (identical(granularita, "Ora")) periodo + hours(1) else
          if (identical(granularita, "Settimana")) periodo + 7 - 0.5 else
          if (identical(granularita, "Mese")) periodo %m+% months(1) - 0.5 else
          if (identical(granularita, "Trimestre")) periodo %m+% months(3) - 0.5 else
          if (identical(granularita, "Anno")) periodo %m+% years(1) - 0.5 else periodo + 0.5)
    # Unisce intervalli adiacenti: un solo sfondo per ciascuna interruzione.
    if (nrow(sfondo) > 1L) {
      sfondo <- sfondo |> arrange(xmin)
      sfondo$.gruppo <- cumsum(c(TRUE,
        as.numeric(sfondo$xmin[-1]) > head(cummax(as.numeric(sfondo$xmax)), -1)))
      sfondo <- sfondo |> group_by(.gruppo) |>
        summarise(xmin = min(xmin), xmax = max(xmax), .groups = "drop")
    }
    valori_finiti <- grafico$attivazioni[is.finite(grafico$attivazioni)]
    sfondo$ymin <- rep(min(c(0, valori_finiti)), nrow(sfondo))
    sfondo$ymax <- rep(max(c(1, valori_finiti)) * 1.04, nrow(sfondo))
    sfondo_layer <- geom_rect(data = sfondo,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        alpha = "Interruzione: origine e redistribuzione delle manovre"),
      inherit.aes = FALSE, fill = "#EBBD55", show.legend = TRUE)

    if (identical(granularita, "Ora")) {
      return(
        ggplot(
          grafico |>
            mutate(
              giorno_linea = as.Date(
                periodo,
                tz = "Europe/Rome"
              )
            ),
          aes(
            x = periodo,
            y = attivazioni,
            color = etichetta_completa,
            group = interaction(
              etichetta_completa,
              giorno_linea
            )
          )
        ) +
          sfondo_layer +
          legenda_redistribuzione() +
          geom_line(linewidth = 0.8) +
          geom_point_interactive(
            aes(
              tooltip = tooltip_punti,
              data_id = paste(
                etichetta_completa,
                periodo_label,
                sep = "__"
              )
            ),
            size = 1.8
          ) +
          scale_x_datetime(
            breaks = sort(unique(grafico$periodo)),
            labels = function(x) {
              format(
                x,
                "%d-%m %H:00",
                tz = "Europe/Rome"
              )
            },
            minor_breaks = NULL,
            timezone = "Europe/Rome"
          ) +
          scala_y_manovre_visualizzate(grafico$attivazioni) +
          scale_color_manual(
            values = palette_sensori,
            name = "Componente"
          ) +
          labs(x = NULL, y = "Manovre", caption = if (any(grafico$redistribuite))
            "Sfondo giallo: manovre orarie anomale redistribuite per interruzione della connessione." else NULL) +
          tema_san(base_size = 13) +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(
              color = "#B8C4CC",
              linewidth = 0.5
            ),
            axis.text.x = element_text(
              size = 10,
              angle = 90,
              face = "bold",
              hjust = 1,
              vjust = 0.5
            ),
            axis.text.y = element_text(size = 11),
            plot.title = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 13),
            legend.text = element_text(size = 13)
          )
      )
    }
    
    breaks_periodo <- calcola_breaks_periodo(grafico$periodo)
    
    spline_att <- interpola_spline(
      grafico,
      "periodo",
      "attivazioni",
      "etichetta_completa"
    ) |>
      mutate(
        etichetta_completa = factor(
          etichetta_completa,
          levels = levels(grafico$etichetta_completa)
        )
      )
    
    ggplot() +
      sfondo_layer +
      legenda_redistribuzione() +
      geom_line(
        data = spline_att,
        aes(
          x = periodo,
          y = attivazioni,
          color = etichetta_completa
        ),
        linewidth = 1,
        na.rm = FALSE
      ) +
      geom_point_interactive(
        data = grafico,
        aes(
          x = periodo,
          y = attivazioni,
          color = etichetta_completa,
          tooltip = tooltip_punti,
          data_id = paste(
            etichetta_completa,
            periodo_label,
            sep = "__"
          )
        ),
        size = 1.8
      ) +
      scale_x_date(
        breaks = breaks_periodo,
        labels = formatta_periodo_label(
          breaks_periodo,
          granularita
        )
      ) +
      scala_y_manovre_visualizzate(grafico$attivazioni) +
      scale_color_manual(values = palette_sensori, name = "Componente") +
      labs(x = NULL, y = "Manovre", caption = if (any(grafico$redistribuite))
        "Sfondo giallo: manovre orarie anomale redistribuite per interruzione della connessione." else NULL) +
      tema_san(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(
          color = "#B8C4CC",
          linewidth = 0.5
        ),
        axis.text.x = element_text(
          size = 12,
          angle = 90,
          face = "bold",
          hjust = 1,
          vjust = 0.5
        ),
        axis.ticks.x = element_line(
          color = "#6B7380",
          linewidth = 0.5
        ),
        axis.text.y = element_text(size = 11),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)
      )
  }
  
  # Modal (size = "xl", ~95vw): larghezza misurata dal JS dopo l'apertura.
  # In modalita' oraria il contenitore interno ha una larghezza reale in pixel:
  # il grafico mantiene quindi la propria scala e lo scorrimento avviene nel
  # contenitore esterno, senza comprimere l'SVG per farlo stare a schermo.
  output$modal_activationPlot_container <- renderUI({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    grafico <- dati_grafico_modal()
    n_periodi <- dplyr::n_distinct(grafico$periodo)
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    
    granularita <- filtri_attivazioni_modal()$granularita
    
    plot_px <- if (identical(granularita, "Ora")) {
      # La vista oraria resta volutamente larga e scrollabile.
      max(
        w_px,
        n_periodi * max(90, n_sensori * 16)
      )
    } else if (identical(granularita, "Giorno")) {
      # La vista giornaliera puo' avere molti periodi, ma non deve diventare
      # una striscia enorme: al massimo circa due larghezze del modal.
      max(
        w_px,
        min(w_px * 2, n_periodi * 120)
      )
    } else {
      # Settimana / Mese / Trimestre / Anno: tutto compatto nel modal.
      w_px
    }
    
    div(
      class = "activation-scroll-inner",
      style = paste0(
        "width:", round(plot_px), "px;",
        "min-width:", round(plot_px), "px;"
      ),
      girafeOutput(
        "modal_activationPlot",
        width = "100%",
        height = "380px"
      )
    )
  })
  
  output$modal_activationTrendPlot_container <- renderUI({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    grafico <- dati_grafico_modal()
    n_periodi <- dplyr::n_distinct(grafico$periodo)
    
    plot_px <- if (
      identical(filtri_attivazioni_modal()$granularita, "Ora")
    ) {
      max(w_px, n_periodi * 55)
    } else {
      w_px
    }
    
    div(
      class = "activation-scroll-inner",
      style = paste0(
        "width:", round(plot_px), "px;",
        "min-width:", round(plot_px), "px;"
      ),
      girafeOutput(
        "modal_activationTrendPlot",
        width = "100%",
        height = "380px"
      )
    )
  })
  
  output$modal_activationPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    grafico <- dati_grafico_modal()
    n_periodi <- dplyr::n_distinct(grafico$periodo)
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    
    granularita <- filtri_attivazioni_modal()$granularita
    
    plot_px <- if (identical(granularita, "Ora")) {
      # La vista oraria resta volutamente larga e scrollabile.
      max(
        w_px,
        n_periodi * max(90, n_sensori * 16)
      )
    } else if (identical(granularita, "Giorno")) {
      # La vista giornaliera puo' avere molti periodi, ma non deve diventare
      # una striscia enorme: al massimo circa due larghezze del modal.
      max(
        w_px,
        min(w_px * 2, n_periodi * 120)
      )
    } else {
      # Settimana / Mese / Trimestre / Anno: tutto compatto nel modal.
      w_px
    }
    
    girafe(
      ggobj      = render_activation_bar_gg(),
      width_svg  = plot_px / 72,
      height_svg = 380 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
  output$modal_activationTrendPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    grafico <- dati_grafico_modal()
    n_periodi <- dplyr::n_distinct(grafico$periodo)
    
    plot_px <- if (
      identical(filtri_attivazioni_modal()$granularita, "Ora")
    ) {
      max(w_px, n_periodi * 55)
    } else {
      w_px
    }
    
    girafe(
      ggobj      = render_activation_trend_gg(),
      width_svg  = plot_px / 72,
      height_svg = 380 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
