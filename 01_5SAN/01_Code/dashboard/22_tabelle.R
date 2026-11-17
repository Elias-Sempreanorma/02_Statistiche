  # -----------------------------------------------------------------------
  # Tabelle mostrate/nascoste dentro le finestre principali.
  # -----------------------------------------------------------------------
  
  output$modal_tabella_dati_attivazioni <- renderDT({
    dati_grafico_modal() |>
      transmute(
        Periodo     = as.character(periodo_label),
        Componente     = as.character(etichetta_completa),
        Manovre = attivazioni,
        Anomalia = if_else(anomalia_oraria, "SI", "")
      ) |>
      arrange(Periodo, Componente)
  }, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
  
  output$modal_tabella_dati_trend <- renderDT({
    dati_grafico_modal() |>
      transmute(
        Componente     = as.character(etichetta_completa),
        Periodo     = as.character(periodo_label),
        Manovre = attivazioni,
        Anomalia = if_else(anomalia_oraria, "SI", "")
      ) |>
      arrange(Componente, Periodo)
  }, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
  
  output$modal_tabella_dati_tank <- renderDT({
    tank_data_modal() |>
      transmute(
        Componente = gsub("\n", " ", as.character(etichetta_sensore)),
        Tipo    = if_else(tipo == "B10dSAN", "Manovre effettuate", "Anni di utilizzo"),
        Valore  = round(valore, 1),
        Massimo = massimo,
        `%`     = ifelse(
          is.na(percentuale),
          NA_character_,
          paste0(round(percentuale, 1), " %")
        )
      )
  }, rownames = FALSE, options = list(pageLength = 25, dom = "t"))
  
  output$modal_tabella_outlier_nok <- renderDT({
    
    esclusi <- allarmi_attivazioni_modal()
    
    validate(
      need(
        nrow(esclusi) > 0,
        "Nessuna manovra anomala nel periodo selezionato."
      )
    )
    
    tabella_segnalazioni(esclusi, "Data", righe = 25)
  }, server = TRUE)
  
  output$modal_tabella_dati_profilo_nok <- renderDT({
    
    utilizzo_nok_modal()$per_sensore_confronto |>
      transmute(
        Confronto = as.character(confronto),
        Componente = etichetta_sensore,
        `Utilizzo medio rispetto allo storico` = round(media_nok, 3),
        `Variazioni nell’utilizzo (più basso = più regolare)` = round(varianza_nok, 3),
        `Utilizzo sensore` = round(U_sensore, 3)
      ) |>
      arrange(Confronto, Componente)
  },
  rownames = FALSE,
  options = list(pageLength = 25, dom = "tip"))
  
  output$modal_tabella_dati_nok <- renderDT({
    
    {
      andamento <- nok_temporale_confronto_modal()
      
      bind_rows(
        andamento$sensori |>
          transmute(
            Confronto = as.character(confronto),
            Periodo = periodo_label,
            Serie = etichetta_sensore,
            `Utilizzo / storico` = round(NOK, 3)
          ),
        andamento$media_macchina |>
          transmute(
            Confronto = as.character(confronto),
            Periodo = periodo_label,
            Serie = "Media macchina",
            `Utilizzo / storico` = round(NOK, 3)
          )
      ) |>
        arrange(Confronto, Periodo, Serie)
    }
  },
  rownames = FALSE,
  options = list(pageLength = 25, dom = "tip"))
