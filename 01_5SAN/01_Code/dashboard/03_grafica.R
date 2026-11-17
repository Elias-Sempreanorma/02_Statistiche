# ---------------------------------------------------------------------------
# Identita' visiva Sempreanorma.
# Il giallo viene usato come accento, mentre blu, grigi e bianco restano
# i colori dominanti dell'interfaccia e dei grafici.
# ---------------------------------------------------------------------------
SAN_BLUE <- "#234A66"
SAN_BLUE_2 <- "#5F748C"
SAN_YELLOW <- "#EBBD55"
SAN_GREY <- "#A9B2BA"
SAN_GREY_LIGHT <- "#D4D9DD"

palette_sensori_brand <- function(n) {
  base <- c(
    SAN_BLUE,
    "#3D617A",
    SAN_BLUE_2,
    "#7D919F",
    "#9EABB4",
    "#BCC4CA",
    SAN_YELLOW,
    "#D4A23B"
  )

  if (n <= length(base)) {
    base[seq_len(n)]
  } else {
    grDevices::colorRampPalette(base)(n)
  }
}

# Colori alternati dagli estremi: primo, ultimo, secondo, penultimo, ...
palette_sensori_alternata <- function(n) {
  palette <- palette_sensori_brand(max(8L, n))
  m <- length(palette)
  ordine <- unique(as.vector(rbind(seq_len(ceiling(m / 2)),
    m - seq_len(ceiling(m / 2)) + 1L)))
  palette[ordine][seq_len(n)]
}


# Unica scala per barre e andamento: solo valori della copia grafica redistribuita.
scala_y_manovre_visualizzate <- function(valori) {
  finiti <- valori[is.finite(valori)]
  minimo <- min(c(0, finiti))
  massimo <- max(c(0, finiti))
  ampiezza <- massimo - minimo
  limiti <- if (ampiezza > 0) c(minimo, massimo + ampiezza * 0.06) else c(0, 1)
  scale_y_continuous(
    limits = limiti,
    breaks = scales::pretty_breaks(n = 7),
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    oob = scales::squish,
    expand = expansion(mult = c(0, 0))
  )
}

legenda_redistribuzione <- function() {
  scale_alpha_manual(name = NULL,
    values = c("Interruzione: origine e redistribuzione delle manovre" = 0.18),
    guide = guide_legend(override.aes = list(fill = "#EBBD55", colour = NA, alpha = 0.3)))
}


# ---------------------------------------------------------------------------
# Lookup precalcolati per i filtri a cascata: tabelle piccole, distinct
# su poche colonne, cosi' gli observeEvent non devono piu' scandire
# l'intero dataset `dati` ogni volta che cambia un filtro.
# ---------------------------------------------------------------------------
company <- dati |>
  distinct(company) |>
  arrange(company) |>
  pull(company)

stabilimenti_lookup <- dati |>
  distinct(company, field) |>
  arrange(company, field)

macchine_lookup <- dati |>
  distinct(company, field, coupon, project, machine_name) |>
  arrange(company, field, project, machine_name)

# Un solo ordinamento per menu, card e grafici: FCM1, FCM2, ..., FCM10.
# numeric = TRUE gestisce anche codici con piu' gruppi numerici o suffissi.
ordina_naturale <- function(df) {
  df <- mutate(df, cds_name = as.character(cds_name))
  df[stringr::str_order(df$cds_name, numeric = TRUE), , drop = FALSE]
}

# Finestra visualizzata nei confronti NOK/Utilizzo: almeno due mesi prima
# della selezione, o pari durata se la selezione e' piu' lunga.
# Non modifica le finestre usate per soglie e report.
periodo_precedente_grafici <- function(date_selezionate, date_disponibili) {
  inizio <- as.Date(date_selezionate[1])
  fine <- as.Date(date_selezionate[2])
  durata <- as.integer(fine - inizio) + 1L
  da <- min(
    inizio - durata,
    lubridate::`%m-%`(inizio, lubridate::period(2, units = "month"))
  )
  a <- inizio - 1L
  precedenti <- as.Date(date_disponibili)
  precedenti <- precedenti[!is.na(precedenti) & precedenti <= a]
  if (length(precedenti) > 0L) {
    da <- max(da, min(precedenti))
  }
  list(
    inizio = da,
    fine = a,
    etichette = c(
      "Periodo precedente" = paste0(
        "Storico precedente\n", format(da, "%d/%m/%Y"),
        " - ", format(a, "%d/%m/%Y")
      ),
      "Periodo selezionato" = paste0(
        "Periodo selezionato\n", format(inizio, "%d/%m/%Y"),
        " - ", format(fine, "%d/%m/%Y")
      )
    )
  )
}

# Tema condiviso dei grafici; dati, scale e colori delle serie restano invariati.
tema_san <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(colour = "#234A66"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E8EDF1", linewidth = 0.35),
      axis.text = element_text(colour = "#66717A"),
      axis.title = element_text(colour = "#234A66", face = "bold"),
      plot.background = element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      legend.background = element_rect(fill = "#FFFFFF", colour = NA),
      legend.key = element_rect(fill = "#FFFFFF", colour = NA),
      strip.text = element_text(colour = "#234A66", face = "bold"),
      plot.margin = margin(16, 18, 12, 12)
    )
}

sensori_lookup <- dati |>
  distinct(coupon, cds_name, sensor_description) |>
  ordina_naturale() |>
  arrange(coupon)

# Stessa regola usata dall'ETL: nessun alert prima di 7 giorni distinti
# con conteggi validi. Qui il lookup resta utile ai report storici.
sensori_allarmabili <- calcola_sensori_allarmabili(dati)

# ---------------------------------------------------------------------------
# Base oraria precalcolata una sola volta all'avvio di Shiny.
# Le reactive dei grafici filtrano questi oggetti gia' aggregati invece di
# ricostruire ogni volta tutto lo storico della macchina.
# ---------------------------------------------------------------------------
dati_attivazioni_orarie_base <- prepara_manovre_orarie(dati) |>
  group_by(coupon, day, hour, cds_name, sensor_description) |>
  summarise(attivazioni_orarie = if (any(is.finite(hourly_count)))
      sum(hourly_count[is.finite(hourly_count)]) else NA_real_,
    anomalia_oraria = any(anomalia_oraria),
    gateway_disconnect_affected = any(gateway_disconnect_affected, na.rm = TRUE),
    .groups = "drop")

finestre_orarie_macchina <- dati |>
  mutate(
    timestamp_local = with_tz(timestamp, "Europe/Rome"),
    day_oraria = as.Date(timestamp_local, tz = "Europe/Rome"),
    hour_oraria = floor_date(timestamp_local, "hour")
  ) |>
  group_by(coupon, day = day_oraria) |>
  summarise(
    prima_ora = min(hour_oraria, na.rm = TRUE),
    ultima_ora = max(hour_oraria, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(
    is.finite(as.numeric(prima_ora)),
    is.finite(as.numeric(ultima_ora))
  )

# Copia destinata esclusivamente ai grafici: dati, modelli e medie restano originali.
# Redistribuisce solo accumuli da disconnessione classificati come anomalie
# orarie dall'ETL. Le quote sono proporzionali ai secondi in ogni ora.
redistribuisci_manovre_grafico <- function(dati, orari) {
  chiavi <- c("coupon", "gateway_id", "sensor_id", "cds_name", "sensor_description")
  anomali <- orari |>
    filter(anomalia_oraria %in% TRUE) |>
    distinct(across(all_of(c(chiavi, "hour")))) |>
    mutate(.anomalo = TRUE)
  base <- dati |>
    ungroup() |>
    group_by(across(all_of(chiavi))) |>
    arrange(timestamp, .by_group = TRUE) |>
    mutate(.precedente = lag(timestamp)) |>
    ungroup() |>
    mutate(hour = floor_date(with_tz(timestamp, "Europe/Rome"), "hour")) |>
    left_join(anomali, by = c(chiavi, "hour")) |>
    mutate(.spalma = gateway_disconnect_affected %in% TRUE &
      .anomalo %in% TRUE & is.finite(increment) & increment > 0 &
      !is.na(.precedente) & timestamp > .precedente)
  normali <- base |> filter(!.spalma) |>
    transmute(coupon, cds_name, sensor_description,
      hour = floor_date(with_tz(timestamp, "Europe/Rome"), "hour"),
      attivazioni_orarie = increment, redistribuite = FALSE,
      gateway_disconnect_affected)
  eventi <- base |> filter(.spalma)
  quote <- lapply(seq_len(nrow(eventi)), function(i) {
    ev <- eventi[i, ]
    inizio <- as.numeric(ev$.precedente)
    fine <- as.numeric(ev$timestamp)
    ore <- seq(floor(inizio / 3600) * 3600,
      (ceiling(fine / 3600) - 1) * 3600, by = 3600)
    peso <- (pmin(ore + 3600, fine) - pmax(ore, inizio)) / (fine - inizio)
    tibble(coupon = ev$coupon, cds_name = ev$cds_name,
      sensor_description = ev$sensor_description,
      hour = as.POSIXct(ore, origin = "1970-01-01", tz = "Europe/Rome"),
      attivazioni_orarie = ev$increment * peso,
      redistribuite = TRUE, gateway_disconnect_affected = TRUE)
  }) |> bind_rows()
  # Solo nella copia grafica: evidenzia anche l'ora da cui si toglie l'accumulo.
  origini <- eventi |> transmute(coupon, cds_name, sensor_description,
    hour = floor_date(with_tz(timestamp, "Europe/Rome"), "hour"),
    attivazioni_orarie = 0, redistribuite = TRUE, gateway_disconnect_affected = TRUE)
  bind_rows(normali, quote, origini) |>
    mutate(day = as.Date(hour, tz = "Europe/Rome")) |>
    group_by(coupon, day, hour, cds_name, sensor_description) |>
    summarise(attivazioni_orarie = if (any(is.finite(attivazioni_orarie)))
        sum(attivazioni_orarie[is.finite(attivazioni_orarie)]) else NA_real_,
      redistribuite = any(redistribuite),
      gateway_disconnect_affected = any(gateway_disconnect_affected, na.rm = TRUE),
      .groups = "drop")
}

dati_attivazioni_orarie_grafico <- redistribuisci_manovre_grafico(
  dati, prepara_manovre_orarie(dati))

nota_redistribuzione <- paste0(
  "<br/><b>Manovre redistribuite graficamente per interruzione della connessione.</b>",
  "<br/>Accumulo classificato come anomalo nell’ora. ",
  "La distribuzione temporale è stimata.")

data_min <- min(dati$day, na.rm = TRUE)
data_max <- max(dati$day, na.rm = TRUE)
data_start_default <- max(data_min, data_max - 27)

