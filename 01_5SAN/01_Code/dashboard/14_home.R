  # ---------------------------------------------------------------------
  # Reactive condivisi: il valore giornaliero per sensore e la media
  # storica (NMN) vengono calcolati una sola volta e riusati sia dalla
  # tabella NOK sia dal grafico storico, invece di essere ricalcolati
  # due volte in reactive separate.
  # bindCache: se piu' utenti (o la stessa sessione in momenti diversi)
  # scelgono la stessa macchina/sensori, il risultato viene riusato
  # invece di ricalcolato.
  # ---------------------------------------------------------------------
  # Unica funzione per preparare i dati giornalieri usati dal NOK.
  # Legge i flag LOF + limiti al 95% gia' salvati dall'ETL:
  # nessuna classificazione o stima di distribuzioni nella dashboard.
  prepara_valori_giornalieri_nok <- function(macchina, sensori) {
    prepara_valori_giornalieri_nok_data(
      dati, macchina = macchina, sensori = sensori
    )
  }
  
  valori_giornalieri <- reactive({
    
    filtri <- filtri_principali()
    req(length(filtri$sensori) > 0)
    
    prepara_valori_giornalieri_nok(
      filtri$macchina,
      filtri$sensori
    )
  }) |>
    bindCache(filtri_principali()$macchina, filtri_principali()$sensori)
  
  nmn_storico <- reactive({
    filtri <- filtri_principali()
    
    valori_giornalieri() |>
      filter(
        !outlier_lof,
        day < filtri$date[1] | day > filtri$date[2]
      ) |>
      group_by(cds_name, sensor_description) |>
      summarise(NMN = mean(daily_value, na.rm = TRUE), .groups = "drop")
  })
  
  
  kpi_nok <- reactive({
    
    filtri <- filtri_principali()
    
    nmm_per_sensore <- valori_giornalieri() |>
      filter(
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      group_by(cds_name, sensor_description) |>
      summarise(NMM = mean(daily_value, na.rm = TRUE), .groups = "drop")
    
    nmn_storico() |>
      left_join(nmm_per_sensore, by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK = case_when(
          is.na(NMN) | is.na(NMM) | NMM == 0 ~ NA_real_,
          TRUE ~ NMM / NMN
        )
      ) |>
      ordina_naturale()
  })
  
  # ---------------------------------------------------------------------
  # Schema interattivo nella Home. Mostra esclusivamente le immagini
  # disponibili per il progetto della macchina selezionata. I punti sono
  # limitati ai sensori selezionati e realmente presenti nella macchina.
  # ---------------------------------------------------------------------
  output$schema_sensori <- renderUI({
    
    filtri <- filtri_principali()
    
    coupon_macchina <- filtri$macchina
    req(length(coupon_macchina) == 1L, !is.na(coupon_macchina), nzchar(coupon_macchina))

    immagine <- immagini_macchine |>
      filter(coupon == coupon_macchina) |>
      slice_head(n = 1)

    if (
      nrow(immagine) == 0 ||
      !dir.exists(cds_images_dir) ||
      !file.exists(file.path(cds_images_dir, immagine$file_name))
    ) {
      return(NULL)
    }
    
    sensori_selezionati <- filtri$sensori
    
    sensori_macchina <- sensori_lookup |>
      filter(
        coupon == filtri$macchina,
        cds_name %in% sensori_selezionati
      ) |>
      mutate(cds_key = str_to_upper(str_squish(cds_name))) |>
      group_by(coupon, cds_key) |>
      summarise(
        cds_name = first(cds_name),
        sensor_description = paste(
          unique(na.omit(sensor_description)),
          collapse = ", "
        ),
        .groups = "drop"
      )
    
    metriche <- if (length(sensori_selezionati) == 0) {
      data.frame(
        cds_key = character(),
        N_medio = double(),
        ore_aperte_medie = double(),
        NOK = double()
      )
    } else {
      raw_avg <- dati |>
        filter(
          coupon == filtri$macchina,
          cds_name %in% sensori_selezionati,
          day >= filtri$date[1],
          day <= filtri$date[2]
        ) |>
        group_by(cds_name, day) |>
        summarise(
          daily_count = if (
            any(is.finite(increment))
          ) {
            sum(
              increment[is.finite(increment)],
              na.rm = TRUE
            )
          } else {
            NA_real_
          },
          daily_open_hours = if (
            any(is.finite(daily_open_hours))
          ) {
            max(daily_open_hours[is.finite(daily_open_hours)])
          } else {
            0
          },
          .groups = "drop"
        ) |>
        group_by(cds_name) |>
        summarise(
          N_medio = mean(daily_count, na.rm = TRUE),
          ore_aperte_medie = mean(daily_open_hours, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(cds_key = str_to_upper(str_squish(cds_name)))
      
      nok_home <- kpi_nok() |>
        mutate(cds_key = str_to_upper(str_squish(cds_name))) |>
        select(cds_key, NOK) |>
        group_by(cds_key) |>
        summarise(NOK = first(NOK), .groups = "drop")
      
      raw_avg |>
        select(cds_key, N_medio, ore_aperte_medie) |>
        left_join(nok_home, by = "cds_key")
    }
    
    punti <- mappa_sensori |>
      filter(coupon == coupon_macchina) |>
      select(coupon, cds_key, x_pct, y_pct) |>
      inner_join(sensori_macchina, by = c("coupon", "cds_key")) |>
      left_join(metriche, by = "cds_key")

    # ST21: i soli RA dell'anagrafica fornita devono restare individuabili
    # sul disegno anche se non sono ancora presenti nei dati elaborati.
    # I componenti presenti ma deselezionati restano esclusi.
    if (identical(coupon_macchina, "LIN-SEMP-3357-9670-4875-9907-093")) {
      ra_senza_dati <- mappa_sensori |>
        filter(
          coupon == coupon_macchina,
          grepl("^RA[2-5] \\((CHIAVE|RESET)\\)$", cds_name)
        ) |>
        anti_join(
          sensori_lookup |>
            filter(coupon == coupon_macchina) |>
            mutate(cds_key = str_to_upper(str_squish(cds_name))),
          by = c("coupon", "cds_key")
        ) |>
        transmute(
          coupon, cds_key, x_pct, y_pct, cds_name,
          sensor_description = NA_character_,
          N_medio = NA_real_,
          ore_aperte_medie = NA_real_,
          NOK = NA_real_
        )
      punti <- bind_rows(punti, ra_senza_dati)
    }
    
    formatta_numero <- function(x, decimali) {
      if (length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
        return("N/D")
      }
      
      formatC(
        x,
        format = "f",
        digits = decimali,
        decimal.mark = ",",
        big.mark = "."
      )
    }
    
    hotspot <- lapply(seq_len(nrow(punti)), function(i) {
      punto <- punti[i, ]
      descrizione <- if (
        is.na(punto$sensor_description) ||
        trimws(punto$sensor_description) == ""
      ) {
        punto$cds_name
      } else {
        paste(punto$cds_name, punto$sensor_description, sep = " - ")
      }
      
      attivazioni_medie <- formatta_numero(punto$N_medio, 1)
      ore_aperte <- formatta_numero(punto$ore_aperte_medie, 0)
      etichetta_ore_aperte <- if (
        length(filtri$date) == 2 &&
        identical(filtri$date[1], filtri$date[2])
      ) {
        "Ore aperto"
      } else {
        "Ore aperto medie/giorno"
      }
      nok_periodo <- formatta_numero(punto$NOK, 3)
      
      tags$span(
        class = "sensor-hotspot",
        tabindex = "0",
        `data-sensor` = punto$cds_name,
        `aria-label` = paste0(
          descrizione,
          "; manovre giornaliere medie: ", attivazioni_medie,
          "; ", etichetta_ore_aperte, ": ", ore_aperte,
          "; NOK: ", nok_periodo
        ),
        style = paste0(
          sprintf(
            "left: %.4f%%; top: %.4f%%;",
            punto$x_pct,
            punto$y_pct
          ),
          # Su ST21 CHIAVE e RESET sono vicini: dimensioni proporzionali
          # al disegno mantengono separate le due aree anche al resize.
          if (
            identical(coupon_macchina, "LIN-SEMP-3357-9670-4875-9907-093") &&
            grepl("^RA[2-5] \\((CHIAVE|RESET)\\)$", punto$cds_name)
          ) {
            sprintf("width: %.4f%%; height: %.4f%%;", 100 * 14 / 913, 100 * 12 / 558)
          } else if (
            identical(coupon_macchina, "LIN-SEMP-3167-2891-4825-4255-052") &&
            punto$cds_name %in% c("BIM 1 - IN", "BIM 1 - AV", "BIM2 - IN", "BIM 2 - AV")
          ) {
            sprintf("width: %.4f%%; height: %.4f%%;", 100 * 8 / 801, 100 * 10 / 340)
          } else {
            ""
          }
        ),
        tags$span(
          class = "sensor-tooltip",
          tags$span(descrizione, class = "sensor-tooltip-title"),
          tags$div("Manovre giornaliere medie: ", tags$strong(attivazioni_medie)),
          tags$div(etichetta_ore_aperte, ": ", tags$strong(paste0(ore_aperte, " h"))),
          tags$div("NOK: ", tags$strong(nok_periodo))
        )
      )
    })
    
    image_version <- unname(
      tools::md5sum(file.path(cds_images_dir, immagine$file_name))
    )
    
    div(
      class = "schema-home",
      div(
        class = if (immagine$file_name %in% c("Siboni_Banco_1.png", "Siboni_Banco_2.png")) {
          "schema-frame schema-frame--siboni-large"
        } else {
          "schema-frame"
        },
        tags$img(
          src = paste0(
            "cds-images/",
            immagine$file_name,
            "?v=",
            image_version
          ),
          alt = paste("Schema componenti della macchina", coupon_macchina)
        ),
        hotspot
      )
    )
  })
  
