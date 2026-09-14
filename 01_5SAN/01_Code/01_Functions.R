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
# Helper: raggruppa una data nel periodo scelto (giorno/settimana/mese/...)
# e restituisce anche un'etichetta leggibile per gli assi/facet
# ---------------------------------------------------------------------------
periodo_bucket <- function(day, granularita) {
  switch(
    granularita,
    "Giorno"     = day,
    "Settimana"  = floor_date(day, "week", week_start = 1),
    "Mese"       = floor_date(day, "month"),
    "Trimestre"  = floor_date(day, "quarter"),
    "Anno"       = floor_date(day, "year"),
    day
  )
}

formatta_periodo_label <- function(periodo, granularita) {
  as.character(switch(
    granularita,
    "Giorno"    = format(periodo, "%d-%m-%Y"),
    "Settimana" = paste0("Sett. ", format(periodo, "%d-%m-%Y")),
    "Mese"      = format(periodo, "%b %Y"),
    "Trimestre" = paste0("Q", quarter(periodo), " ", format(periodo, "%Y")),
    "Anno"      = format(periodo, "%Y"),
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