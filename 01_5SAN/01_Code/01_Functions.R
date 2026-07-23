# funzione per connettersi a DB postgres

connetti_postgres <- function(database) {
  readRenviron(db)
  
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
  readRenviron(db)
  
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
}


# funzioni controllo valori nulli

# Funzione somma solo se esiste almeno un valore valido.
# Evita di trasformare ore senza dati validi in finti zeri.
somma_o_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

# Funzione per calcolare statistiche evitando NaN o Inf
stat_o_na <- function(x, fun) {
  if (all(is.na(x))) NA_real_ else fun(x, na.rm = TRUE)
}

# Sceglie il formato delle etichette dell'asse X
# in base al periodo e alla granularità scelta con x_breaks
scegli_x_labels <- function(periodo, x_breaks) {
  
  x_breaks <- stringr::str_to_lower(x_breaks)
  
  if (periodo == "orario") {
    if (stringr::str_detect(x_breaks, "min|hour")) return("%d %b\n%H:%M")
    if (stringr::str_detect(x_breaks, "day")) return("%d %b")
    if (stringr::str_detect(x_breaks, "week")) return("sett. %d %b")
    if (stringr::str_detect(x_breaks, "month")) return("%b %Y")
  }
  
  if (periodo == "giornaliero") {
    if (stringr::str_detect(x_breaks, "day")) return("%d %b")
    if (stringr::str_detect(x_breaks, "week")) return("sett. %d %b")
    if (stringr::str_detect(x_breaks, "month")) return("%b %Y")
  }
  
  if (periodo == "settimanale") {
    if (stringr::str_detect(x_breaks, "week")) return("sett. %d %b")
    if (stringr::str_detect(x_breaks, "month")) return("%b %Y")
  }
  
  "%d %b"
}

# funzione di plot
plot_macchina_attivazioni <- function(coupon_macchina,
                                      periodo = "orario",
                                      sensori = NULL,
                                      x_breaks = NULL) {
  
  periodo <- match.arg(periodo, c("orario", "giornaliero", "settimanale"))
  
  # Funzioni usate da stat_summary
  somma <- function(x) sum(x, na.rm = TRUE)
  
  somma_meno_sd <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) <= 1) return(NA_real_)
    max(sum(x) - sd(x), 0)
  }
  
  somma_piu_sd <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) <= 1) return(NA_real_)
    sum(x) + sd(x)
  }
  
  # Parto sempre dalla tabella oraria
  dati_plot <- sensore_orario |>
    filter(coupon == coupon_macchina, !is.na(attivazioni_orarie))
  
  # Filtro opzionale sui sensori/CdS
  if (!is.null(sensori)) {
    dati_plot <- dati_plot |>
      filter(cds_name %in% sensori)
  }
  
  # Creo la variabile tempo in base al periodo scelto
  if (periodo == "orario") {
    dati_plot <- dati_plot |>
      mutate(tempo = ora)
    
    x_label <- "Timestamp"
    y_label <- "Attivazioni orarie"
    subtitle <- "Attivazioni orarie per CdS"
    if (is.null(x_breaks)) x_breaks <- "1 day"
  }
  
  if (periodo == "giornaliero") {
    dati_plot <- dati_plot |>
      mutate(tempo = as.Date(ora))
    
    x_label <- "Giorno"
    y_label <- "Attivazioni giornaliere"
    subtitle <- "Attivazioni giornaliere per CdS"
    if (is.null(x_breaks)) x_breaks <- "1 day"
  }
  
  if (periodo == "settimanale") {
    dati_plot <- dati_plot |>
      mutate(tempo = as.Date(floor_date(ora, "week", week_start = 1)))
    
    x_label <- "Settimana"
    y_label <- "Attivazioni settimanali"
    subtitle <- "Attivazioni settimanali per CdS"
    if (is.null(x_breaks)) x_breaks <- "1 week"
  }
  
  if (nrow(dati_plot) == 0) {
    stop("Nessun dato disponibile per la macchina/CdS selezionati.")
  }
  
  x_labels <- scegli_x_labels(periodo, x_breaks)
  
  nome_macchina <- dati_plot |>
    distinct(machine_name) |>
    pull(machine_name) |>
    first()
  
  # Grafico base
  p <- ggplot(dati_plot, aes(x = tempo, y = attivazioni_orarie, group = 1)) +
    facet_grid(cds_name ~ ., scales = "free_y") +
    scale_y_continuous(
      breaks = function(x) pretty(x, n = 3),
      minor_breaks = NULL
    ) +
    labs(
      title = nome_macchina,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 7),
      strip.text.y = element_text(angle = 0, size = 7),
      strip.background = element_blank()
    )
  
  # Orario: i dati sono già orari, quindi non serve aggregare
  if (periodo == "orario") {
    p <- p +
      geom_line(linewidth = 0.25, alpha = 0.7, color = "steelblue") +
      geom_point(size = 1.8, alpha = 0.9, color = "steelblue") +
      scale_x_datetime(
        date_breaks = x_breaks,
        date_labels = x_labels,
        minor_breaks = NULL
      )
  }
  
  # Giornaliero / settimanale:
  # stat_summary aggrega le ore nel giorno o nella settimana
  if (periodo != "orario") {
    p <- p +
      stat_summary(
        fun.min = somma_meno_sd,
        fun.max = somma_piu_sd,
        geom = "errorbar",
        color = "violet",
        width = 0.1,
        linewidth = 0.3
      ) +
      stat_summary(
        fun = somma,
        geom = "line",
        linewidth = 0.25,
        alpha = 0.7,
        color = "steelblue"
      ) +
      stat_summary(
        fun = somma,
        geom = "point",
        size = 2,
        alpha = 0.9,
        color = "steelblue"
      ) +
      scale_x_date(
        date_breaks = x_breaks,
        date_labels = x_labels,
        minor_breaks = NULL
      )
  }
  
  p
}
