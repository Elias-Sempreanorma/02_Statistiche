  disponibilita_macchina_modal <- reactive({
    filtri <- filtri_nok_modal()
    machine_availability_daily |>
      filter(
        coupon == filtri$macchina,
        day >= filtri$date[1], day <= filtri$date[2]
      ) |>
      mutate(periodo = as.Date(periodo_bucket(day, filtri$granularita))) |>
      group_by(periodo) |>
      summarise(
        ore_disponibili = mean(daily_available_hours),
        ore_totali = sum(daily_available_hours),
        ore_gateway = sum(daily_gateway_hours),
        .groups = "drop"
      ) |>
      mutate(
        periodo_label = formatta_periodo_label(periodo, filtri$granularita),
        percentuale = if_else(ore_gateway > 0, 100 * ore_totali / ore_gateway, NA_real_)
      )
  })

  output$modal_disponibilitaPlot <- renderGirafe({
    base <- disponibilita_macchina_modal()
    granularita <- filtri_nok_modal()$granularita
    validate(need(nrow(base) > 0,
      "Disponibilita non disponibile: eseguire l'ETL aggiornato e verificare i dati del periodo."))
    # Larghezze in giorni, coerenti con la scala temporale del NOK.
    larghezza <- switch(granularita,
      "Giorno" = 0.8, "Settimana" = 5.6, "Mese" = 24,
      "Trimestre" = 72, "Anno" = 290, 0.8)
    grafico <- ggplot(base, aes(x = periodo, y = percentuale)) +
      geom_col_interactive(aes(
        tooltip = paste0(
          "Periodo: ", periodo_label, "<br/>",
          "Disponibilità: ", formatC(percentuale, format = "f", digits = 1, decimal.mark = ","), "%<br/>",
          formatC(ore_totali, format = "f", digits = 2, decimal.mark = ","),
          " ore disponibili su ",
          formatC(ore_gateway, format = "f", digits = 2, decimal.mark = ","),
          " ore di accensione"
        ),
        data_id = paste("disponibilita", periodo, sep = "__")
      ), fill = "#234A66", width = larghezza) +
      scale_x_date(
        breaks = function(limits) {
          periodi <- sort(unique(base$periodo))
          periodi[periodi >= as.Date(limits[1], origin = "1970-01-01") &
                  periodi <= as.Date(limits[2], origin = "1970-01-01")]
        },
        labels = function(x) formatta_periodo_label(as.Date(x, origin = "1970-01-01"), granularita)
      ) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20),
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "Disponibilità (%)") +
      tema_san(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#D7DDE2", linewidth = 0.4),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 11)
      )
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    girafe(ggobj = grafico, width_svg = w_px / 72, height_svg = 360 / 72,
      options = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(hidden = c("selection", "zoom", "misc"))
      ))
  })

  render_nok_history_gg <- function() {
    
    storico <- kpi_nok_storico_modal()
    granularita <- filtri_nok_modal()$granularita
    
    validate(
      need(nrow(storico) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    storico <- storico |>
      ordina_naturale() |>
      mutate(
        etichetta_sensore = factor(etichetta_sensore, levels = unique(etichetta_sensore)),
        periodo_label = formatta_periodo_label(
          periodo,
          granularita
        )
      )
    
    n_sensori <- dplyr::n_distinct(storico$etichetta_sensore)
    palette_sensori <- palette_sensori_brand(n_sensori)
    
    breaks_periodo <- calcola_breaks_periodo(storico$periodo)
    
    spline_nok <- interpola_spline(storico, "periodo", "NOK_periodo", "etichetta_sensore") |>
      mutate(etichetta_sensore = factor(etichetta_sensore, levels = levels(storico$etichetta_sensore)))
    
    max_nok <- max(storico$NOK_periodo, na.rm = TRUE)
    
    ggplot() +
      geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "#9AA5B1",
        linewidth = 0.8
      ) +
      geom_line(
        data = spline_nok,
        aes(x = periodo, y = NOK_periodo, color = etichetta_sensore),
        linewidth = 1,
        na.rm = FALSE
      ) +
      geom_point_interactive(
        data = storico,
        aes(
          x       = periodo,
          y       = NOK_periodo,
          color   = etichetta_sensore,
          tooltip = paste0(
            "<b>", etichetta_sensore, "</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            "NOK: ", round(NOK_periodo, 3)
          ),
          data_id = paste(etichetta_sensore, periodo_label, sep = "__")
        ),
        size = 2.2
      ) +
      scale_x_date(
        breaks = breaks_periodo,
        labels = formatta_periodo_label(
          breaks_periodo,
          granularita
        )
      ) +
      scale_y_continuous(
        breaks = function(limits) {
          b <- scales::pretty_breaks(n = 8)(limits)
          sort(unique(c(b, 1, if (is.finite(max_nok)) max_nok)))
        },
        expand = expansion(mult = c(0.02, 0.04))
      ) +
      scale_color_manual(values = palette_sensori, name = "Componente") +
      labs(x = NULL, y = "NOK") +
      tema_san(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#B8C4CC", linewidth = 0.5),
        axis.text.x = element_text(size = 12, angle = 90, face = "bold", hjust = 1, vjust = 0.5),
        axis.ticks.x = element_line(color = "#6B7380", linewidth = 0.5),
        axis.text.y = element_text(size = 11),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)
      )
  }
  
  output$modal_nokPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    girafe(
      ggobj      = render_nok_history_gg(),
      width_svg  = w_px / 72,
      height_svg = 420 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
