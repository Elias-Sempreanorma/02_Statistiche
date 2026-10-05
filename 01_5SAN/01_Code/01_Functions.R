# Soglia unica usata da ETL e dashboard: un sensore puo' generare alert
# solo dopo almeno 30 osservazioni finite del contatore, indipendentemente
# dal valore osservato.
MIN_OSSERVAZIONI_ALLARME <- 30L

calcola_sensori_allarmabili <- function(dati) {
  dati |>
    dplyr::filter(
      !is.na(coupon),
      !is.na(gateway_id),
      !is.na(sensor_id),
      is.finite(count)
    ) |>
    dplyr::count(
      coupon,
      gateway_id,
      sensor_id,
      cds_name,
      sensor_description,
      name = "n_osservazioni_conteggio"
    ) |>
    dplyr::filter(
      n_osservazioni_conteggio >= MIN_OSSERVAZIONI_ALLARME
    )
}

# Base giornaliera condivisa tra dashboard ed ETL.
# Mantiene la stessa logica LOF gia' usata dalla dashboard, ma la rende
# riutilizzabile senza duplicare calcoli o regole.
prepara_valori_giornalieri_nok_data <- function(
  dati,
  macchina = NULL,
  sensori = NULL
) {
  base <- dati |>
    dplyr::ungroup()

  if (!is.null(macchina)) {
    base <- base |>
      dplyr::filter(coupon == macchina)
  }
  if (!is.null(sensori)) {
    base <- base |>
      dplyr::filter(cds_name %in% sensori)
  }

  base |>
    dplyr::group_by(
      company,
      field,
      project,
      coupon,
      machine_name,
      gateway_id,
      gateway_name,
      sensor_id,
      cds_name,
      sensor_description,
      day
    ) |>
    dplyr::summarise(
      daily_count = if (any(is.finite(increment))) {
        sum(increment[is.finite(increment)], na.rm = TRUE)
      } else {
        NA_real_
      },
      daily_uptime = if (all(is.na(daily_uptime))) {
        NA_real_
      } else {
        max(daily_uptime, na.rm = TRUE)
      },
      gateway_disconnect_affected = any(
        gateway_disconnect_affected,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      daily_value = dplyr::case_when(
        is.na(daily_uptime) | daily_uptime <= 0 ~ NA_real_,
        TRUE ~ daily_count / daily_uptime
      )
    ) |>
    dplyr::group_by(
      coupon,
      gateway_id,
      sensor_id,
      cds_name,
      sensor_description
    ) |>
    dplyr::group_modify(~ {
      x <- .x$daily_count
      validi <- is.finite(x)

      .x$lof_score <- NA_real_
      .x$outlier_lof <- FALSE

      n_validi <- sum(validi)
      media_attivazioni_giornaliere <- if (n_validi > 0) {
        mean(x[validi], na.rm = TRUE)
      } else {
        NA_real_
      }

      zero_anomalo <- (
        validi &
        x == 0 &
        is.finite(media_attivazioni_giornaliere) &
        media_attivazioni_giornaliere > 0
      )
      .x$outlier_lof[zero_anomalo] <- TRUE

      if (
        n_validi >= 6L &&
        dplyr::n_distinct(x[validi]) >= 3L
      ) {
        k <- min(4L, n_validi - 1L)
        score <- dbscan::lof(
          matrix(x[validi], ncol = 1),
          minPts = k
        )

        x_validi <- x[validi]
        mediana_senza_valore <- vapply(
          seq_along(x_validi),
          function(i) {
            altri_valori <- x_validi[-i]
            if (length(altri_valori) == 0) {
              NA_real_
            } else {
              stats::median(altri_valori, na.rm = TRUE)
            }
          },
          numeric(1)
        )

        rapporto_anomalo <- (
          is.finite(mediana_senza_valore) &
          x_validi >= mediana_senza_valore * 5
        )
        differenza_anomala <- (
          is.finite(mediana_senza_valore) &
          (x_validi - mediana_senza_valore) >= 49
        )
        lof_anomalo <- !is.na(score) & score > 2

        .x$lof_score[validi] <- score
        .x$outlier_lof[validi] <- (
          .x$outlier_lof[validi] |
          lof_anomalo |
          rapporto_anomalo |
          differenza_anomala
        )
      }

      .x
    }) |>
    dplyr::ungroup()
}

# Eventi di sensore aperto / assenza di contatto elettrico condivisi tra
# ETL e dashboard. L'eventuale filtro dei sensori maturi avviene prima del
# calcolo degli intervalli, evitando lavoro inutile.
calcola_eventi_sensore_aperto <- function(
  raw_data,
  sensori_allarmabili = NULL,
  macchina = NULL,
  sensori = NULL,
  data_inizio = NULL,
  data_fine = NULL
) {
  base <- raw_data

  if (!is.null(sensori_allarmabili)) {
    base <- base |>
      dplyr::semi_join(
        sensori_allarmabili |>
          dplyr::select(coupon, gateway_id, sensor_id),
        by = c("coupon", "gateway_id", "sensor_id")
      )
  }
  if (!is.null(macchina)) {
    base <- base |>
      dplyr::filter(coupon == macchina)
  }
  if (!is.null(sensori)) {
    base <- base |>
      dplyr::filter(cds_name %in% sensori)
  }

  base <- base |>
    dplyr::mutate(
      field = dplyr::if_else(
        is.na(field) | trimws(field) == "",
        "(Non specificato)",
        field
      ),
      timestamp_local = lubridate::with_tz(
        timestamp,
        "Europe/Rome"
      ),
      day = as.Date(
        timestamp_local,
        tz = "Europe/Rome"
      )
    )

  if (!is.null(data_inizio)) {
    base <- base |>
      dplyr::filter(day >= as.Date(data_inizio))
  }
  if (!is.null(data_fine)) {
    base <- base |>
      dplyr::filter(day <= as.Date(data_fine))
  }

  gruppi <- c(
    "company", "field", "project", "coupon",
    "machine_name", "machine_serial_number",
    "gateway_id", "gateway_name", "sensor_id",
    "cds_name", "cds_description", "cds_brand",
    "cds_use", "cds_vds", "sensor_description"
  )

  base |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(gruppi)),
      day
    ) |>
    dplyr::arrange(timestamp_local, .by_group = TRUE) |>
    dplyr::mutate(
      previous_timestamp = dplyr::lag(timestamp_local),
      previous_status = dplyr::lag(status),
      previous_raw_count = dplyr::lag(count),
      sensore_aperto_intervallo = (
        !is.na(previous_timestamp) &
        is.finite(status) &
        is.finite(previous_status) &
        status == 0 &
        previous_status == 0 &
        is.finite(count) &
        is.finite(previous_raw_count) &
        count == previous_raw_count
      ),
      nuovo_evento_aperto = (
        sensore_aperto_intervallo &
        !dplyr::lag(
          sensore_aperto_intervallo,
          default = FALSE
        )
      ),
      open_event_id = cumsum(nuovo_evento_aperto)
    ) |>
    dplyr::filter(sensore_aperto_intervallo) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(gruppi)),
      day,
      open_event_id
    ) |>
    dplyr::summarise(
      event_start = min(previous_timestamp, na.rm = TRUE),
      event_end = max(timestamp_local, na.rm = TRUE),
      open_hours = as.numeric(
        difftime(
          event_end,
          event_start,
          units = "hours"
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(
      is.finite(open_hours),
      open_hours > 1
    )
}

# funzione per connettersi a DB postgres

connetti_postgres <- function(database) {
  if (file.exists(db)) {
    readRenviron(db)
  }
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PG_HOST"),
    port = as.integer(Sys.getenv("PG_PORT")),
    dbname = database,
    user = Sys.getenv("PG_USER"),
    password = Sys.getenv("PG_PASSWORD")
  )
}


# funzione per connettersi a Maria DB

connetti_maria <- function() {
  if (file.exists(db)) {
    readRenviron(db)
  }
  
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

# ---------------------------------------------------------------------------
# Helper: raggruppa data/ora nel periodo scelto (ora/giorno/settimana/mese/...)
# e restituisce anche un'etichetta leggibile per gli assi/facet
# ---------------------------------------------------------------------------
periodo_bucket <- function(day, granularita) {
  switch(
    granularita,
    "Ora"        = floor_date(day, "hour"),
    "Giorno"     = as.Date(day),
    "Settimana"  = floor_date(as.Date(day), "week", week_start = 1),
    "Mese"       = floor_date(as.Date(day), "month"),
    "Trimestre"  = floor_date(as.Date(day), "quarter"),
    "Anno"       = floor_date(as.Date(day), "year"),
    day
  )
}

formatta_periodo_label <- function(periodo, granularita) {
  as.character(switch(
    granularita,
    "Ora"       = format(periodo, "%d-%m-%Y %H:00"),
    "Giorno"    = format(as.Date(periodo), "%d-%m-%Y"),
    "Settimana" = paste0("Sett. ", format(as.Date(periodo), "%d-%m-%Y")),
    "Mese"      = format(as.Date(periodo), "%b %Y"),
    "Trimestre" = paste0("Q", quarter(as.Date(periodo)), " ", format(as.Date(periodo), "%Y")),
    "Anno"      = format(as.Date(periodo), "%Y"),
    format(periodo, "%d-%m-%Y")
  ))
}

# Sceglie al massimo `max_breaks` date da mostrare sull'asse x, distribuite
# uniformemente, cosi' le etichette non si accavallano quando i periodi
# disponibili sono molti (usato dai grafici trend/storico)
calcola_breaks_periodo <- function(periodi) {
  sort(unique(as.Date(periodi)))
}

# Interpola un dataframe per gruppo usando spline cubica naturale, in modo che
# la curva passi esattamente per tutti i punti dati originali.
# Interpola con spline cubica monotona, ma spezza la curva dove il gap
# tra due punti consecutivi supera `gap_factor` volte il gap mediano.
# Cosi' i tratti densi restano morbidi e i vuoti lunghi non generano parabole.
interpola_spline <- function(df, x_col, y_col, group_col, n = 300, gap_factor = 2.5) {
  df |>
    group_by(across(all_of(group_col))) |>
    group_modify(function(d, ...) {
      x_num <- as.numeric(d[[x_col]])
      y_val <- d[[y_col]]
      validi <- !is.na(x_num) & !is.na(y_val)
      x_num <- x_num[validi]
      y_val <- y_val[validi]
      
      if (length(x_num) < 2) {
        return(setNames(
          data.frame(as.Date(x_num, origin = "1970-01-01"), y_val),
          c(x_col, y_col)
        ))
      }
      
      # Individua i gap anomali e suddivide in segmenti
      diffs       <- diff(x_num)
      soglia      <- gap_factor * median(diffs)
      grandi_gap  <- which(diffs > soglia)
      break_pts   <- c(0L, grandi_gap, length(x_num))
      
      n_seg <- length(break_pts) - 1L
      
      parti <- lapply(seq_len(n_seg), function(i) {
        idx <- (break_pts[i] + 1L):break_pts[i + 1L]
        xs  <- x_num[idx]
        ys  <- y_val[idx]
        
        if (length(xs) < 2L) {
          return(setNames(
            data.frame(as.Date(xs, origin = "1970-01-01"), ys),
            c(x_col, y_col)
          ))
        }
        
        sf       <- stats::splinefun(xs, ys, method = "monoH.FC")
        n_punti  <- max(round(n / n_seg), length(xs))
        x_interp <- seq(min(xs), max(xs), length.out = n_punti)
        setNames(
          data.frame(as.Date(x_interp, origin = "1970-01-01"), sf(x_interp)),
          c(x_col, y_col)
        )
      })
      
      # Unisce i segmenti separandoli con una riga NA (spezza la linea)
      righe_na <- setNames(
        data.frame(as.Date(NA), NA_real_),
        c(x_col, y_col)
      )
      risultato <- parti[[1]]
      if (n_seg > 1L) {
        for (i in 2:n_seg) {
          risultato <- rbind(risultato, righe_na, parti[[i]])
        }
      }
      risultato
    }) |>
    ungroup()
}