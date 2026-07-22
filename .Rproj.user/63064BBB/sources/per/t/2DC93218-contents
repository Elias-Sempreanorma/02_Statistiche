library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(stringr)
library(here)
library(slider)

# Grafico orario di una macchina.
# Ogni pannello è un sensore.
# Usiamo sensore_orario perché vogliamo vedere i sensori separati.
sensore_orario <- sensore_orario |>
  filter(
    ora >= as.POSIXct("2026-06-25 00:00:00"),
    ora <  as.POSIXct("2026-07-08 00:00:00")
  )

plot_macchina_attivazioni <- function(coupon_macchina,
                                      periodo = "orario",
                                      sensori = NULL,
                                      x_breaks = NULL) {
  
  periodo <- match.arg(periodo, c("orario", "giornaliero", "settimanale"))
  
  # Funzioni per stat_summary
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
  
  # Dati base: sempre tabella oraria
  dati_plot <- sensore_orario |>
    filter(coupon == coupon_macchina, !is.na(attivazioni_orarie))
  
  # Filtro opzionale sensori
  if (!is.null(sensori)) {
    dati_plot <- dati_plot |>
      filter(sensor_name %in% sensori)
  }
  
  # Creo la variabile tempo in base al periodo scelto
  if (periodo == "orario") {
    dati_plot <- dati_plot |>
      mutate(tempo = ora)
    
    x_label <- "Timestamp"
    y_label <- "Attivazioni orarie"
    subtitle <- "Attivazioni orarie per sensore"
    if (is.null(x_breaks)) x_breaks <- "1 day"
  }
  
  if (periodo == "giornaliero") {
    dati_plot <- dati_plot |>
      mutate(tempo = as.Date(ora))
    
    x_label <- "Giorno"
    y_label <- "Attivazioni giornaliere"
    subtitle <- "Attivazioni giornaliere per sensore"
    if (is.null(x_breaks)) x_breaks <- "1 day"
  }
  
  if (periodo == "settimanale") {
    dati_plot <- dati_plot |>
      mutate(tempo = as.Date(floor_date(ora, "week", week_start = 1)))
    
    x_label <- "Settimana"
    y_label <- "Attivazioni settimanali"
    subtitle <- "Attivazioni settimanali per sensore"
    if (is.null(x_breaks)) x_breaks <- "1 week"
  }
  
  if (nrow(dati_plot) == 0) {
    stop("Nessun dato disponibile per la macchina/sensori selezionati.")
  }
  
  nome_macchina <- dati_plot |>
    distinct(machine_name) |>
    pull(machine_name) |>
    first()
  
  # Grafico base
  p <- ggplot(dati_plot, aes(x = tempo, y = attivazioni_orarie, group = 1)) +
    facet_grid(sensor_name ~ ., scales = "free_y") +
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
  
  # Orario: dati già aggregati, niente stat_summary
  if (periodo == "orario") {
    p <- p +
      geom_line(linewidth = 0.25, alpha = 0.7, color = "steelblue") +
      geom_point(size = 1.6, alpha = 0.9, color = "steelblue") +
      scale_x_datetime(
        date_breaks = x_breaks,
        date_labels = "%d %b",
        minor_breaks = NULL
      )
  }
  
  # Giornaliero/settimanale: aggrego nel plot con stat_summary
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
        size = 1.8,
        alpha = 0.9,
        color = "steelblue"
      ) +
      scale_x_date(
        date_breaks = x_breaks,
        date_labels = "%d %b",
        minor_breaks = NULL
      )
  }l
  
  p
}

# indicare coupon, periodo ("orario"/ "giornaliero" / "settimanale"), sensori e x_breaks
plot_macchina_attivazioni("CIS-IMEL-8132-6401-9728-7945-157", periodo = "giornaliero", sensori = c("Input1"), x_breaks = "1 day")

plot_macchina_attivazioni("CAD-RITE-8841-6712-8594-2083-027", periodo = "giornaliero", sensori = c("Output1", "Sensor 3"), x_breaks = "1 day")
