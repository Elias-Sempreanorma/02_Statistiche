# Almeno 30 giornate distinte con un conteggio valido per componente.
MIN_GIORNI_ALLARME <- 30L

calcola_sensori_allarmabili <- function(dati) {
  dati |>
    dplyr::filter(
      !is.na(coupon), !is.na(gateway_id), !is.na(sensor_id),
      is.finite(count), !is.na(timestamp)
    ) |>
    dplyr::mutate(
      .giorno_allarme = as.Date(timestamp, tz = "Europe/Rome")
    ) |>
    dplyr::distinct(
      coupon, gateway_id, sensor_id, cds_name, sensor_description,
      .giorno_allarme
    ) |>
    dplyr::count(
      coupon, gateway_id, sensor_id, cds_name, sensor_description,
      name = "n_giorni_conteggio"
    ) |>
    dplyr::filter(n_giorni_conteggio >= MIN_GIORNI_ALLARME)
}

# Parametri della sola classificazione giornaliera; stima eseguita dall'ETL.
MANOVRE_MODEL_DAYS <- 7L
MANOVRE_MODEL_VERSION <- 1L
MANOVRE_ALPHA <- .05

stima_modello_manovre <- function(df) {
  df <- df[order(df$day), ]
  df <- df[is.finite(df$daily_count) & df$daily_count >= 0 &
    abs(df$daily_count - round(df$daily_count)) < 1e-6 &
    is.finite(df$daily_uptime) & df$daily_uptime > 0, ]
  if (nrow(df) < MIN_GIORNI_ALLARME) return(NULL)
  df$daily_count <- round(df$daily_count)
  # Verifica temporale: il modello non vede le giornate di verifica.
  n_train <- nrow(df) - max(10L, floor(nrow(df) / 5))
  train <- df[seq_len(n_train), ]
  test <- df[seq.int(n_train + 1L, nrow(df)), ]
  fit <- function(d, family) {
    # Uno storico solo a zero non dimostra che il tasso reale sia nullo.
    if (all(d$daily_count == 0)) return(NULL)
    tryCatch({
      setTimeLimit(elapsed = 10, transient = TRUE)
      if (family == "poisson") {
        m <- robustbase::glmrob(
          daily_count ~ 1 + offset(log(daily_uptime)), data = d,
          family = poisson(), method = "Mqle",
          start = log(max(median(d$daily_count / d$daily_uptime),
                          1 / sum(d$daily_uptime)))
        )
        if (!isTRUE(m$converged)) stop("Stima Poisson non convergente")
        c(rate = unname(exp(coef(m)[1])), size = Inf)
      } else {
        m <- withCallingHandlers(robNB::nb.glm.rob(
          y = d$daily_count, designX = matrix(1, nrow(d), 1),
          offset = log(d$daily_uptime), c.tukey.beta = 5,
          c.tukey.sigma = 5, maxit = 30, warn = TRUE
        ), message = function(m) stop(conditionMessage(m)))
        c(rate = unname(exp(m$coef[2])), size = unname(1 / m$coef[1]))
      }
    }, error = function(e) NULL,
       finally = setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
  }
  candidates <- lapply(c("poisson", "negative_binomial"), function(family) {
    m <- suppressWarnings(fit(train, family))
    if (is.null(m) || !is.finite(m["rate"]) || m["rate"] < 0 ||
        is.na(m["size"]) || m["size"] <= 0) return(NULL)
    mu <- m["rate"] * test$daily_uptime
    lower <- if (is.infinite(m["size"])) qpois(MANOVRE_ALPHA / 2, mu) else qnbinom(MANOVRE_ALPHA / 2, mu = mu, size = m["size"])
    upper <- if (is.infinite(m["size"])) qpois(1 - MANOVRE_ALPHA / 2, mu) else qnbinom(1 - MANOVRE_ALPHA / 2, mu = mu, size = m["size"])
    outside <- test$daily_count < lower | test$daily_count > upper
    # Non forzo un modello che fallisce sistematicamente sui giorni successivi.
    if (binom.test(sum(outside), nrow(test), p = MANOVRE_ALPHA,
                   alternative = "greater")$p.value < .01) return(NULL)
    logp <- if (is.infinite(m["size"])) dpois(test$daily_count, mu, log = TRUE) else
      dnbinom(test$daily_count, mu = mu, size = m["size"], log = TRUE)
    # Score predittivo limitato: un solo estremo non decide la distribuzione.
    data.frame(family, score = mean(pmin(-logp, -log(.001))),
               coverage = mean(!outside))
  }) |>
    dplyr::bind_rows()
  if (nrow(candidates) == 0) return(NULL)
  chosen <- candidates[which.min(candidates$score), ]
  m <- suppressWarnings(fit(df, chosen$family))
  if (is.null(m) || !is.finite(m["rate"]) || is.na(m["size"]) || m["size"] <= 0) return(NULL)
  tibble::tibble(family = chosen$family, rate = unname(m["rate"]),
    size = unname(m["size"]), coverage = chosen$coverage,
    fitted_at = Sys.time(), train_end = max(df$day), n_days = nrow(df))
}

aggiorna_modelli_manovre <- function(giornalieri, path) {
  if (!requireNamespace("robustbase", quietly = TRUE) || !requireNamespace("robNB", quietly = TRUE)) {
    stop("Mancano robustbase/robNB: ricostruire l'immagine Docker prima dell'ETL.")
  }
  cache <- if (file.exists(path)) readRDS(path) else NULL
  if (is.null(cache) || !identical(cache$version, MANOVRE_MODEL_VERSION)) {
    cache <- list(version = MANOVRE_MODEL_VERSION, models = tibble::tibble())
  }
  # Il giorno corrente e' incompleto: non entra nella stima del riferimento.
  storico <- giornalieri |>
    dplyr::filter(day < as.Date(Sys.time(), tz = "Europe/Rome"))
  models <- storico |>
    dplyr::group_by(coupon, gateway_id, sensor_id) |>
    dplyr::group_modify(function(.x, .y) {
      old <- cache$models
      if (nrow(old) > 0) old <- dplyr::semi_join(old, .y,
        by = c("coupon", "gateway_id", "sensor_id"))
      history <- if (nrow(old) > 0) dplyr::filter(.x, day <= old$train_end[1]) else .x
      unchanged <- nrow(old) > 0 && nrow(history) == old$n_history[1] &&
        isTRUE(all.equal(sum(history$daily_count, na.rm = TRUE), old$sum_count[1])) &&
        isTRUE(all.equal(sum(history$daily_uptime, na.rm = TRUE), old$sum_hours[1]))
      if (unchanged && (old$n_days[1] >= MIN_GIORNI_ALLARME || nrow(.x) < MIN_GIORNI_ALLARME) &&
          as.numeric(difftime(Sys.time(), old$fitted_at[1], units = "days")) < MANOVRE_MODEL_DAYS) {
        return(dplyr::select(old, -coupon, -gateway_id, -sensor_id))
      }
      model <- stima_modello_manovre(.x)
      if (is.null(model)) model <- tibble::tibble(
        family = NA_character_, rate = NA_real_, size = NA_real_,
        coverage = NA_real_, fitted_at = Sys.time(),
        train_end = if (nrow(.x)) max(.x$day) else as.Date(NA), n_days = nrow(.x)
      )
      model <- dplyr::mutate(model, n_history = nrow(.x),
        sum_count = sum(.x$daily_count, na.rm = TRUE),
        sum_hours = sum(.x$daily_uptime, na.rm = TRUE))
      if (nrow(.x) == 0) model[FALSE, ] else model
    }) |>
    dplyr::ungroup()
  cache$models <- models
  saveRDS(cache, path)
  models
}

# Il nome outlier_lof resta compatibile con tutti i calcoli NMN esistenti:
# ora significa LOF > 2 E conteggio fuori dai limiti probabilistici al 95%.
classifica_anomalie_giornaliere <- function(df, k = 4L) {
  validi <- is.finite(df$daily_value) & is.finite(df$daily_count) &
    df$daily_count >= 0 & is.finite(df$daily_uptime) & df$daily_uptime > 0
  validi <- validi & abs(df$daily_count - round(df$daily_count)) < 1e-6
  df$lof_score <- NA_real_
  df$outlier_lof <- FALSE
  df$limite_manovre_inf <- df$limite_manovre_sup <- NA_real_
  if (sum(validi) < MIN_GIORNI_ALLARME ||
      !"rate" %in% names(df) || !is.finite(df$rate[1])) return(df)
  mu <- df$rate[1] * df$daily_uptime[validi]
  size <- df$size[1]
  df$limite_manovre_inf[validi] <- if (is.infinite(size)) qpois(MANOVRE_ALPHA / 2, mu) else qnbinom(MANOVRE_ALPHA / 2, mu = mu, size = size)
  df$limite_manovre_sup[validi] <- if (is.infinite(size)) qpois(1 - MANOVRE_ALPHA / 2, mu) else qnbinom(1 - MANOVRE_ALPHA / 2, mu = mu, size = size)
  if (dplyr::n_distinct(df$daily_value[validi]) >= 2L) {
    score <- dbscan::lof(matrix(df$daily_value[validi], ncol = 1),
      minPts = min(as.integer(k), sum(validi) - 1L))
    df$lof_score[validi] <- score
    df$outlier_lof[validi] <- !is.na(score) & score > 2 &
      (df$daily_count[validi] < df$limite_manovre_inf[validi] |
       df$daily_count[validi] > df$limite_manovre_sup[validi])
  }
  df
}

# Lettura dei risultati ETL: nessun LOF o modello viene stimato in Shiny.
prepara_valori_giornalieri_nok_data <- function(dati, macchina = NULL, sensori = NULL) {
  base <- if (all(c("daily_count", "outlier_lof") %in% names(dati))) dati else
    attr(dati, "giornalieri_nok")
  if (is.null(base)) stop("Eseguire l'ETL aggiornato per generare i dati giornalieri NOK.")
  base <- dplyr::ungroup(base)
  if (!is.null(macchina)) base <- dplyr::filter(base, coupon == macchina)
  if (!is.null(sensori)) base <- dplyr::filter(base, cds_name %in% sensori)
  base
}


# Base giornaliera condivisa tra dashboard ed ETL.
# Aggregazione eseguita una sola volta nell'ETL.
aggrega_valori_giornalieri_nok_data <- function(
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
    )
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


# Durate leggibili nei messaggi e nei report, senza ore decimali.
durata_in_testo <- function(ore) {
  minuti <- round(ore * 60)
  h <- minuti %/% 60
  m <- minuti %% 60
  testo_ore <- paste(h, ifelse(h == 1, "ora", "ore"))
  testo_minuti <- paste(m, ifelse(m == 1, "minuto", "minuti"))
  ifelse(!is.finite(ore) | ore < 0, "durata non disponibile",
    ifelse(minuti == 0 & ore > 0, "meno di un minuto",
      ifelse(h == 0, testo_minuti,
        ifelse(m == 0, testo_ore, paste(testo_ore, "e", testo_minuti)))))
}

testo_manovre_giornaliere <- function(conteggio, ore, minimo, massimo) {
  paste0(round(conteggio), " manovre in ", durata_in_testo(ore),
    " di attività. Per una durata simile, in base allo storico ci si aspetta da ",
    minimo, " a ", massimo, " manovre.")
}
