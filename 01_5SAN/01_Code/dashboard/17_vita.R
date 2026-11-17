  # Dati per i serbatoi: stato attuale (non dipende dal periodo selezionato,
  # rappresenta il valore cumulato/corrente del sensore)
  tank_data_modal <- reactive({
    
    filtri <- filtri_vita_modal()
    req(length(filtri$sensori) > 0)
    
    base <- life_data |>
      filter(
        coupon == filtri$macchina,
        cds_name %in% filtri$sensori
      ) |>
      left_join(sensori_info, by = c("coupon", "cds_name")) |>
      mutate(etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")) |>
      ordina_naturale()
    
    # Nelle card il nome del sensore puo' usare tutta la larghezza:
    # niente wrapping forzato in base al numero di sensori selezionati.
    base <- base |>
      mutate(
        etichetta_sensore = factor(
          etichetta_sensore,
          levels = unique(etichetta_sensore)
        )
      )
    
    tank_attivazioni <- base |>
      transmute(
        etichetta_sensore,
        tipo = "B10dSAN",
        valore = count,
        massimo = cds_vds
      )
    
    tank_durata <- base |>
      transmute(
        etichetta_sensore,
        tipo = "T10d",
        valore = lifetime,
        massimo = cds_t10d
      )
    
    bind_rows(tank_attivazioni, tank_durata) |>
      mutate(
        percentuale = ifelse(massimo > 0, valore / massimo * 100, NA_real_),
        percentuale_capped = pmin(percentuale, 100),
        stato = ifelse(!is.na(percentuale) & valore > massimo, "over", "ok")
      )
  })
  
  # ---------------------------------------------------------------------
  # Vita sensori: griglia di card.
  # Ogni card mostra:
  # - valore corrente / massimo B10dSAN (attivazioni)
  # - valore corrente / massimo T10d (anni)
  # - percentuale utilizzata e residua
  # - marker sempre visibile anche con percentuali molto piccole
  #
  # Soglie SOLO grafiche:
  # <80% = OK; 80-100% = Attenzione; >100% = Superato.
  # ---------------------------------------------------------------------
  output$modal_tankCards <- renderUI({
    
    tanks <- tank_data_modal()
    
    validate(
      need(nrow(tanks) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    tanks <- tanks |>
      mutate(
        etichetta_sensore = gsub("\n", " ", as.character(etichetta_sensore)),
        percentuale_visuale = pmax(pmin(percentuale, 100), 0),
        residuo = pmax(100 - percentuale, 0)
      )
    
    formatta_vita_numero <- function(x, tipo) {
      if (length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
        return("N/D")
      }
      
      if (identical(tipo, "B10dSAN")) {
        formatC(
          round(x),
          format = "f",
          digits = 0,
          decimal.mark = ",",
          big.mark = "."
        )
      } else {
        formatC(
          x,
          format = "f",
          digits = 1,
          decimal.mark = ",",
          big.mark = "."
        )
      }
    }
    
    formatta_vita_percentuale <- function(x) {
      if (length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
        return("N/D")
      }
      
      # Evita di mostrare 0,00% quando esiste comunque un consumo reale.
      if (x > 0 && x < 0.01) {
        return("<0,01%")
      }
      
      paste0(
        formatC(
          x,
          format = "f",
          digits = 2,
          decimal.mark = ","
        ),
        "%"
      )
    }
    
    classe_vita <- function(percentuale) {
      if (is.na(percentuale) || is.nan(percentuale) || is.infinite(percentuale)) {
        return("")
      }
      if (percentuale > 100) return("over")
      if (percentuale >= 80) return("warning")
      ""
    }
    
    crea_riga_vita <- function(riga) {
      
      pct <- riga$percentuale_visuale
      stato_css <- classe_vita(riga$percentuale)
      
      # La barra mantiene il valore reale. Il pallino ha una posizione minima
      # dell'1% solo per restare visibile con valori come 391 / 2.000.000.
      marker_pct <- if (
        is.na(pct) || is.nan(pct) || is.infinite(pct)
      ) {
        0
      } else {
        min(max(pct, 1), 100)
      }
      
      unita <- if (
        identical(riga$tipo, "B10dSAN")
      ) {
        " manovre"
      } else {
        " anni"
      }
      
      valore_testo <- paste0(
        formatta_vita_numero(riga$valore, riga$tipo),
        " / ",
        formatta_vita_numero(riga$massimo, riga$tipo),
        unita
      )
      
      div(
        class = "life-row",
        
        div(
          class = "life-row-header",
          span(if (riga$tipo == "B10dSAN") "Manovre effettuate" else "Anni di utilizzo", class = "life-metric"),
          span(valore_testo, class = "life-value")
        ),
        
        div(
          class = "life-progress",
          div(
            class = paste("life-progress-fill", stato_css),
            style = sprintf(
              "width: %.6f%%;",
              ifelse(is.na(pct), 0, pct)
            )
          ),
          div(
            class = paste("life-progress-marker", stato_css),
            style = sprintf("left: %.6f%%;", marker_pct)
          )
        ),
        
        div(
          class = "life-row-footer",
          span(
            "Utilizzo: ",
            strong(
              formatta_vita_percentuale(riga$percentuale),
              class = stato_css
            )
          ),
          span(
            "Residuo: ",
            strong(
              formatta_vita_percentuale(riga$residuo),
              class = stato_css
            )
          )
        )
      )
    }
    
    # Mantiene l'ordine naturale gia' prodotto da ordina_naturale()
    # nella costruzione di tank_data_modal().
    ordine_sensori <- unique(tanks$etichetta_sensore)
    
    cards <- lapply(ordine_sensori, function(nome_sensore) {
      
      df <- tanks |>
        filter(etichetta_sensore == nome_sensore) |>
        arrange(match(tipo, c("B10dSAN", "T10d")))
      
      max_pct <- suppressWarnings(max(df$percentuale, na.rm = TRUE))
      if (!is.finite(max_pct)) {
        max_pct <- NA_real_
      }
      
      stato_label <- if (is.na(max_pct)) {
        "N/D"
      } else if (max_pct > 100) {
        "Superato"
      } else if (max_pct >= 80) {
        "Attenzione"
      } else {
        "OK"
      }
      
      stato_class <- if (identical(stato_label, "Superato")) {
        "life-status life-status-over"
      } else if (identical(stato_label, "Attenzione")) {
        "life-status life-status-warning"
      } else {
        "life-status life-status-ok"
      }
      
      div(
        class = "life-card",
        div(
          class = "life-card-header",
          span(nome_sensore, class = "life-card-title"),
          span(stato_label, class = stato_class)
        ),
        lapply(
          seq_len(nrow(df)),
          function(i) crea_riga_vita(df[i, ])
        )
      )
    })
    
    div(class = "life-grid", cards)
  })
  
