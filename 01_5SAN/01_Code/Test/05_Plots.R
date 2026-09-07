library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)
library(shinyWidgets)
library(RColorBrewer)
library(here)
library(stringr)
library(lubridate)
library(scales)

dati <- readRDS(here("02_Output", "sensor_count_increment.rds")) |>
  mutate(field = if_else(is.na(field) | trimws(field) == "", "(Non specificato)", field))

life_data <- readRDS(here("02_Output", "raw_data.rds")) |>
  group_by(coupon, cds_name, cds_vds, cds_t10d) |>
  summarise(
    count = max(count),
    offset = max(offset),
    lifetime = max(lifetime),
    .groups = "drop_last"
  ) |>
  select(coupon, cds_name, cds_vds, cds_t10d, count, offset, lifetime) |>
  unique() |>
  mutate(
    count = count + offset,
    lifetime = lifetime / 60 / 60 / 24 / 360
  ) |>
  select(-offset)

# Anagrafica sensori (coupon + cds_name -> descrizione), usata per
# arricchire le etichette dei serbatoi
sensori_info <- dati |>
  distinct(coupon, cds_name, sensor_description)

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

sensori_lookup <- dati |>
  distinct(coupon, cds_name, sensor_description) |>
  arrange(coupon, cds_name)

data_min <- min(dati$day, na.rm = TRUE)
data_max <- max(dati$day, na.rm = TRUE)

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
calcola_breaks_periodo <- function(periodi, max_breaks = 12) {
  periodi_ordinati <- sort(unique(periodi))
  if (length(periodi_ordinati) <= 1) return(periodi_ordinati)
  indici <- unique(round(seq(1, length(periodi_ordinati), length.out = min(max_breaks, length(periodi_ordinati)))))
  periodi_ordinati[indici]
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

ui <- fluidPage(
  
  tags$head(
    tags$script(HTML("
      function aggiornaLarghezzaAttivazioni() {
        setTimeout(function () {
          var el = document.getElementById('activationPlot');

          if (el && el.clientWidth > 0) {
            Shiny.setInputValue(
              'activationPlot_px_width',
              el.clientWidth,
              {priority: 'event'}
            );
          }
        }, 100);
      }

      $(document).on('shiny:connected', aggiornaLarghezzaAttivazioni);
      $(document).on('shown.bs.tab', aggiornaLarghezzaAttivazioni);
      $(window).on('resize', aggiornaLarghezzaAttivazioni);

      /* Larghezza del modal: misurata quando e' completamente visibile */
      $(document).on('shown.bs.modal', function () {
        var w = $('.modal-body').first().width();
        if (w > 0) Shiny.setInputValue('modal_px_width', w, {priority: 'event'});
      });
    ")),
    tags$style(HTML("
      /* Modal grande: quasi a schermo intero */
      .modal-xl {
        width: 95vw;
        max-width: 95vw;
      }
      .modal-xl .modal-body {
        max-height: 82vh;
        overflow-y: auto;
      }

      #nok_table table {
        width: 100%;
        border-collapse: collapse;
      }
      #nok_table th {
        background-color: #E9EDF1;
        color: #4A4A4A;
        font-weight: 600;
        text-align: center !important;
        padding: 8px 10px;
        border-bottom: 2px solid #D5DBE0;
      }
      #nok_table td {
        text-align: center !important;
        padding: 8px 10px;
        border-bottom: 1px solid #E4E7EB;
        font-weight: 600;
        font-size: 15px;
      }
      .pannello-filtri {
        background-color: #F4F6F8;
        border-radius: 6px;
        padding: 16px 20px 4px 20px;
        margin-bottom: 18px;
      }
      .titolo-sezione {
        color: #4A4A4A;
        font-weight: 600;
        margin-top: 10px;
        margin-bottom: 10px;
      }
      .card-home {
        display: block;
        width: 100%;
        min-height: 190px;
        background: #FFFFFF;
        border: 1px solid #E4E7EB;
        border-radius: 10px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
        transition: transform 0.15s ease, box-shadow 0.15s ease, border-color 0.15s ease;
        text-align: center;
        white-space: normal;
        padding: 24px 16px;
        margin-bottom: 20px;
        color: #4A4A4A;
      }
      .card-home:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 18px rgba(0,0,0,0.10);
        border-color: #7FA6C9;
      }
      .card-home .card-icona {
        font-size: 30px;
        color: #7FA6C9;
        margin-bottom: 10px;
      }
      .card-home h4 {
        font-weight: 700;
        margin-bottom: 8px;
      }
      .card-home p {
        font-size: 13px;
        color: #8A94A0;
        margin: 0;
      }
    "))
  ),
  
  titlePanel("Dashboard 5SAN"),
  
  # Filtri in orizzontale, a piena larghezza, sempre visibili
  div(
    class = "pannello-filtri",
    fluidRow(
      column(2, selectInput("azienda", "Azienda:", choices = company)),
      column(2, selectInput("stabilimento", "Stabilimento:", choices = NULL)),
      column(3, selectInput("macchina", "Macchina:", choices = NULL)),
      column(3,
             dateRangeInput(
               "date",
               "Periodo:",
               start = data_min,
               end = data_max,
               min = data_min,
               max = data_max,
               format = "dd-mm-yyyy",
               separator = " a ",
               language = "it"
             )
      ),
      column(2,
             pickerInput(
               "sensori",
               "Sensori:",
               choices = NULL,
               multiple = TRUE,
               options = pickerOptions(
                 actionsBox = TRUE,
                 liveSearch = TRUE,
                 selectedTextFormat = "count > 3",
                 countSelectedText = "{0} sensori selezionati"
               )
             )
      )
    )
  ),
  
  tabsetPanel(
    id = "pagina",
    
    tabPanel(
      "Home",
      
      br(),
      fluidRow(
        column(4,
               actionButton(
                 "home_attivazioni",
                 label = div(
                   icon("chart-bar", class = "card-icona"),
                   h4("Conteggio attivazioni"),
                   p("Grafico a barre e trend nel tempo per sensore")
                 ),
                 class = "card-home"
               )
        ),
        column(4,
               actionButton(
                 "home_vita",
                 label = div(
                   icon("gauge", class = "card-icona"),
                   h4("Vita sensori"),
                   p("Stato dei sensori rispetto alle soglie B10dSAN e T10d")
                 ),
                 class = "card-home"
               )
        ),
        column(4,
               actionButton(
                 "home_nok",
                 label = div(
                   icon("chart-line", class = "card-icona"),
                   h4("Storico NOK"),
                   p("KPI e andamento storico del NOK per sensore")
                 ),
                 class = "card-home"
               )
        )
      )
    ),
    
    tabPanel(
      "Conteggio attivazioni",
      
      fluidRow(
        column(12, h4("Conteggio attivazioni", class = "titolo-sezione"))
      ),
      
      fluidRow(
        column(3,
               radioButtons(
                 "granularita",
                 "  ",
                 choices = c("Giorno", "Settimana", "Mese", "Trimestre", "Anno"),
                 selected = "Giorno",
                 inline = TRUE
               )
        )
      ),
      
      fluidRow(
        column(12, girafeOutput(
          "activationPlot",
          width = "100%",
          height = "500px"
        ))
      ),
      
      fluidRow(
        column(12,
               h4("Andamento per sensore", class = "titolo-sezione"),
               girafeOutput("activationTrendPlot", height = "500px")
        )
      )
    ),
    
    tabPanel(
      "Vita sensori",
      
      fluidRow(
        column(12, h4("Vita sensori", class = "titolo-sezione"))
      ),
      
      fluidRow(
        column(12, plotOutput("tankPlot", height = "430px"))
      )
    ),
    
    tabPanel(
      "Storico NOK",
      
      fluidRow(
        column(12,
               h4("NOK per sensore", class = "titolo-sezione"),
               tableOutput("nok_table")
        )
      ),
      
      fluidRow(
        column(12, h4("Andamento storico del NOK", class = "titolo-sezione"))
      ),
      
      fluidRow(
        column(3,
               radioButtons(
                 "granularita_nok",
                 "  ",
                 choices = c("Giorno", "Settimana", "Mese", "Trimestre", "Anno"),
                 selected = "Giorno",
                 inline = TRUE
               )
        )
      ),
      
      fluidRow(
        column(12, girafeOutput("nokHistoryPlot", height = "550px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # 1. Quando cambia l'azienda, aggiorno gli stabilimenti disponibili
  observeEvent(input$azienda, {
    
    req(input$azienda)
    
    stabilimenti <- stabilimenti_lookup |>
      filter(company == input$azienda) |>
      pull(field)
    
    updateSelectInput(
      session,
      "stabilimento",
      choices = stabilimenti
    )
  })
  
  # 2. Quando cambia lo stabilimento (o l'azienda), aggiorno le macchine disponibili
  observeEvent(input$stabilimento, {
    
    req(input$azienda, input$stabilimento)
    
    macchine_filtrate <- macchine_lookup |>
      filter(
        company == input$azienda,
        field == input$stabilimento
      )
    
    updateSelectInput(
      session,
      "macchina",
      choices = setNames(
        macchine_filtrate$coupon,
        paste(macchine_filtrate$project, macchine_filtrate$machine_name, sep = " - ")
      )
    )
  })
  
  # 3. Quando cambia la macchina, aggiorno i sensori disponibili
  observeEvent(input$macchina, {
    
    req(input$macchina)
    
    sensori_macchina <- sensori_lookup |>
      filter(coupon == input$macchina)
    
    updatePickerInput(
      session,
      "sensori",
      choices = setNames(
        sensori_macchina$cds_name,
        paste(sensori_macchina$cds_name, sensori_macchina$sensor_description, sep = " - ")
      ),
      selected = sensori_macchina$cds_name
    )
  })
  
  # ---------------------------------------------------------------------
  # Popup di anteprima dalla Home: riusano gli stessi reactive/filtri
  # della pagina completa, quindi non ricalcolano nulla di pesante.
  # ---------------------------------------------------------------------
  observeEvent(input$home_attivazioni, {
    showModal(modalDialog(
      title = "Conteggio attivazioni",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Chiudi"),
      girafeOutput("modal_activationPlot", height = "380px"),
      br(),
      girafeOutput("modal_activationTrendPlot", height = "380px")
    ))
  })
  
  observeEvent(input$home_vita, {
    showModal(modalDialog(
      title = "Vita sensori",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Chiudi"),
      plotOutput("modal_tankPlot", height = "430px")
    ))
  })
  
  observeEvent(input$home_nok, {
    showModal(modalDialog(
      title = "Storico NOK",
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Chiudi"),
      girafeOutput("modal_nokPlot", height = "420px")
    ))
  })
  
  # ---------------------------------------------------------------------
  # Reactive condivisi: il valore giornaliero per sensore e la media
  # storica (NMN) vengono calcolati una sola volta e riusati sia dalla
  # tabella NOK sia dal grafico storico, invece di essere ricalcolati
  # due volte in reactive separati.
  # bindCache: se piu' utenti (o la stessa sessione in momenti diversi)
  # scelgono la stessa macchina/sensori, il risultato viene riusato
  # invece di ricalcolato.
  # ---------------------------------------------------------------------
  valori_giornalieri <- reactive({
    
    req(input$macchina, input$sensori)
    
    dati |>
      ungroup() |>
      filter(
        coupon == input$macchina,
        cds_name %in% input$sensori
      ) |>
      group_by(cds_name, sensor_description, day) |>
      summarise(daily_value = sum(increment, na.rm = TRUE) / unique(daily_uptime), .groups = "drop")
  }) |>
    bindCache(input$macchina, input$sensori)
  
  nmn_storico <- reactive({
    valori_giornalieri() |>
      group_by(cds_name, sensor_description) |>
      summarise(NMN = mean(daily_value, na.rm = TRUE), .groups = "drop")
  })
  
  # Ordinamento naturale condiviso: prefisso alfabetico + numero,
  # cosi' FCM2 viene prima di FCM10
  ordina_naturale <- function(df) {
    df |>
      mutate(
        cds_name = as.character(cds_name),
        .prefisso = str_extract(cds_name, "^[^0-9]+"),
        .numero = as.numeric(str_extract(cds_name, "[0-9]+$"))
      ) |>
      arrange(.prefisso, .numero) |>
      select(-.prefisso, -.numero)
  }
  
  kpi_nok <- reactive({
    
    req(input$date)
    
    nmm_per_sensore <- valori_giornalieri() |>
      filter(
        day >= input$date[1],
        day <= input$date[2]
      ) |>
      group_by(cds_name, sensor_description) |>
      summarise(NMM = mean(daily_value, na.rm = TRUE), .groups = "drop")
    
    nmn_storico() |>
      left_join(nmm_per_sensore, by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK = case_when(
          is.na(NMN) | is.na(NMM) | NMM == 0 ~ NA_real_,
          TRUE ~ NMN / NMM
        )
      ) |>
      ordina_naturale()
  })
  
  output$nok_table <- renderTable({
    
    kpi <- kpi_nok()
    
    validate(
      need(nrow(kpi) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    # Tabella orizzontale: un sensore per colonna, un'unica riga di valori NOK
    kpi |>
      transmute(
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        NOK = ifelse(
          is.na(NOK) | is.infinite(NOK),
          "N/D",
          format(round(NOK, 3), nsmall = 3)
        )
      ) |>
      pivot_wider(names_from = Sensore, values_from = NOK)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", align = "c")
  
  # Dati per i serbatoi: stato attuale (non dipende dal periodo selezionato,
  # rappresenta il valore cumulato/corrente del sensore)
  tank_data <- reactive({
    
    req(input$macchina, input$sensori)
    
    base <- life_data |>
      filter(
        coupon == input$macchina,
        cds_name %in% input$sensori
      ) |>
      left_join(sensori_info, by = c("coupon", "cds_name")) |>
      mutate(etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")) |>
      ordina_naturale()
    
    larghezza_etichetta <- max(
      10,
      floor(120 / max(n_distinct(base$etichetta_sensore), 1))
    )
    
    base <- base |>
      mutate(
        etichetta_sensore = str_wrap(
          etichetta_sensore,
          width = larghezza_etichetta,
          whitespace_only = FALSE
        ),
        etichetta_sensore = factor(etichetta_sensore, levels = unique(etichetta_sensore))
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
  
  render_tank_plot <- function() {
    
    tanks <- tank_data()
    
    validate(
      need(nrow(tanks) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    ggplot() +
      # "vasca" vuota di sfondo, sempre alta uguale (0-100)
      geom_col(
        data = tanks,
        aes(x = tipo, y = 100),
        fill = "#E4E7EB",
        width = 0.65
      ) +
      # livello di riempimento effettivo, in percentuale sul massimo
      geom_col(
        data = tanks,
        aes(x = tipo, y = percentuale_capped, fill = stato),
        width = 0.65
      ) +
      geom_text(
        data = tanks,
        aes(
          x = tipo,
          y = pmax(percentuale_capped, 10),
          label = paste0(round(valore, 1), " / ", massimo)
        ),
        vjust = -0.4,
        size = 5,
        fontface = "bold",
        color = "#2C3E50"
      ) +
      facet_wrap(~etichetta_sensore, nrow = 1) +
      scale_fill_manual(
        values = c(ok = "#9DC3A0", over = "#D99B94"),
        guide = "none"
      ) +
      scale_y_continuous(limits = c(0, 118), expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(
          fill = scales::alpha("#7FA6C9", 0.10),
          color = NA
        ),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        strip.text = element_text(
          size = 13,
          face = "bold",
          color = "#4A4A4A",
          lineheight = 1.05,
          margin = margin(6, 4, 7, 4)
        ),
        strip.background = element_rect(
          fill = scales::alpha("#7FA6C9", 0.18),
          color = NA
        ),
        panel.spacing.x = unit(8, "pt")
      )
  }
  
  output$tankPlot <- renderPlot({ render_tank_plot() })
  output$modal_tankPlot <- renderPlot({ render_tank_plot() })
  
  # ---------------------------------------------------------------------
  # Dati per il grafico attivazioni: raggruppati nel periodo scelto
  # (giorno/settimana/mese/trimestre/anno). Usati sia dal grafico a
  # barre (facet per periodo) sia dal grafico trend (facet per sensore).
  # ---------------------------------------------------------------------
  dati_grafico <- reactive({
    
    req(input$macchina, input$date, input$sensori, input$granularita)
    
    dati |>
      filter(
        coupon == input$macchina,
        cds_name %in% input$sensori,
        day >= input$date[1],
        day <= input$date[2]
      ) |>
      mutate(periodo = periodo_bucket(day, input$granularita)) |>
      group_by(
        periodo,
        cds_name,
        sensor_description
      ) |>
      summarise(
        attivazioni = sum(increment, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        etichetta = cds_name,
        etichetta_completa = paste(cds_name, sensor_description, sep = " - "),
        periodo_label = formatta_periodo_label(periodo, input$granularita)
      ) |>
      ordina_naturale() |>
      mutate(
        etichetta = factor(etichetta, levels = unique(etichetta)),
        etichetta_completa = factor(etichetta_completa, levels = unique(etichetta_completa)),
        periodo_label = factor(periodo_label, levels = unique(periodo_label[order(periodo)]))
      )
  }) |>
    bindCache(input$macchina, input$sensori, input$date, input$granularita)
  
  # CSS del tooltip, condiviso tra main e modal
  tooltip_css <- paste0(
    "background-color:#2C3E50;",
    "color:#FFFFFF;",
    "padding:8px 12px;",
    "border-radius:6px;",
    "font-size:13px;",
    "line-height:1.6;",
    "box-shadow:0 2px 8px rgba(0,0,0,0.3);"
  )
  
  # Costruisce solo il ggplot (senza girafe), usato sia dal
  # grafico principale sia dal modal
  render_activation_bar_gg <- function() {
    
    grafico <- dati_grafico()
    
    validate(
      need(nrow(grafico) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    palette_sensori <- colorRampPalette(brewer.pal(8, "Set2"))(n_sensori)
    
    ggplot(
      grafico,
      aes(x = etichetta, y = attivazioni, fill = etichetta_completa)
    ) +
      geom_col_interactive(
        aes(
          tooltip = paste0(
            "<b>", etichetta_completa, "</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            "Attivazioni: ", scales::label_number(accuracy = 1, big.mark = ".")(attivazioni)
          ),
          data_id = paste(etichetta_completa, periodo_label, sep = "__")
        ),
        width = 0.8
      ) +
      facet_grid(
        cols = vars(periodo_label),
        scales = "free_x",
        space = "free_x",
        switch = "x"
      ) +
      scale_y_continuous(
        breaks = function(limits) {
          b <- unique(round(scales::pretty_breaks(n = 8)(limits)))
          max_y <- round(max(grafico$attivazioni, na.rm = TRUE))
          sort(unique(c(b, max_y)))
        },
        labels = scales::label_number(accuracy = 1),
        expand = expansion(mult = c(0, 0.04))
      ) +
      scale_fill_manual(values = palette_sensori, name = "Sensore") +
      labs(title = NULL, x = NULL, y = "Attivazioni") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(size = 13, angle = 90),
        axis.text.y = element_text(size = 13),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)
      )
  }
  
  # Curva morbida (spline) con gap-detection: segmenti densi restano fluidi,
  # i vuoti lunghi spezzano la linea invece di generare parabole.
  # I punti reali sono interattivi con tooltip.
  render_activation_trend_gg <- function() {
    
    grafico <- dati_grafico()
    
    validate(
      need(nrow(grafico) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    palette_sensori <- colorRampPalette(brewer.pal(8, "Set2"))(n_sensori)
    
    breaks_periodo <- calcola_breaks_periodo(grafico$periodo)
    
    spline_att <- interpola_spline(grafico, "periodo", "attivazioni", "etichetta_completa") |>
      mutate(etichetta_completa = factor(etichetta_completa, levels = levels(grafico$etichetta_completa)))
    
    ggplot() +
      geom_line(
        data = spline_att,
        aes(x = periodo, y = attivazioni, color = etichetta_completa),
        linewidth = 1,
        na.rm = FALSE   # gli NA spezzano la linea nel punto di gap
      ) +
      geom_point_interactive(
        data = grafico,
        aes(
          x       = periodo,
          y       = attivazioni,
          color   = etichetta_completa,
          tooltip = paste0(
            "<b>", etichetta_completa, "</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            "Attivazioni: ", scales::label_number(accuracy = 1, big.mark = ".")(attivazioni)
          ),
          data_id = paste(etichetta_completa, periodo_label, sep = "__")
        ),
        size = 1.8
      ) +
      scale_x_date(
        breaks = breaks_periodo,
        labels = formatta_periodo_label(breaks_periodo, input$granularita)
      ) +
      scale_y_continuous(
        breaks = function(limits) {
          b <- scales::pretty_breaks(n = 8)(limits)
          max_y <- max(grafico$attivazioni, na.rm = TRUE)
          sort(unique(c(b, max_y)))
        },
        expand = expansion(mult = c(0, 0.04))
      ) +
      scale_color_manual(values = palette_sensori, name = "Sensore") +
      labs(x = NULL, y = "Attivazioni") +
      theme_minimal(base_size = 13) +
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
  
  # Grafico principale: width_svg misurata dal JS sul contenitore reale.
  # Si divide per 72 (DPI interno ggiraph) e non 96, cosi' il fattore di
  # scala SVG->schermo e' 1:1 e testo/leggenda non risultano ingranditi.
  output$activationPlot <- renderGirafe({
    req(input$activationPlot_px_width > 0)
    
    girafe(
      ggobj      = render_activation_bar_gg(),
      width_svg  = input$activationPlot_px_width / 72,
      height_svg = 500 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;")
      )
    )
  })
  
  # Modal (size = "xl", ~95vw): larghezza misurata dal JS dopo l'apertura
  output$modal_activationPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    girafe(
      ggobj      = render_activation_bar_gg(),
      width_svg  = w_px / 72,
      height_svg = 380 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;")
      )
    )
  })
  
  # Riusa la larghezza del grafico a barre (stessa colonna, stessa dimensione)
  output$activationTrendPlot <- renderGirafe({
    req(input$activationPlot_px_width > 0)
    girafe(
      ggobj      = render_activation_trend_gg(),
      width_svg  = input$activationPlot_px_width / 72,
      height_svg = 500 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;")
      )
    )
  })
  
  output$modal_activationTrendPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    girafe(
      ggobj      = render_activation_trend_gg(),
      width_svg  = w_px / 72,
      height_svg = 380 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;")
      )
    )
  })
  
  # Storico del NOK per sensore: i valori giornalieri vengono aggregati
  # nel periodo scelto e confrontati con la media storica (NMN) dello
  # stesso sensore, calcolata su tutto lo storico disponibile.
  kpi_nok_storico <- reactive({
    
    req(input$date, input$granularita_nok)
    
    valori_giornalieri() |>
      filter(
        day >= input$date[1],
        day <= input$date[2]
      ) |>
      mutate(
        periodo = as.Date(periodo_bucket(day, input$granularita_nok))
      ) |>
      group_by(periodo, cds_name, sensor_description) |>
      summarise(
        valore_periodo = mean(daily_value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      left_join(nmn_storico(), by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK_periodo = case_when(
          is.na(valore_periodo) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ valore_periodo / NMN
        ),
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")
      ) |>
      arrange(periodo)
  }) |>
    bindCache(
      input$macchina,
      input$sensori,
      input$date,
      input$granularita_nok
    )
  
  render_nok_history_gg <- function() {
    
    storico <- kpi_nok_storico()
    
    validate(
      need(nrow(storico) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    storico <- storico |>
      ordina_naturale() |>
      mutate(
        etichetta_sensore = factor(etichetta_sensore, levels = unique(etichetta_sensore)),
        periodo_label     = formatta_periodo_label(periodo, input$granularita_nok)
      )
    
    n_sensori <- dplyr::n_distinct(storico$etichetta_sensore)
    palette_sensori <- colorRampPalette(brewer.pal(8, "Set2"))(n_sensori)
    
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
        labels = formatta_periodo_label(breaks_periodo, input$granularita_nok)
      ) +
      scale_y_continuous(
        breaks = function(limits) {
          b <- scales::pretty_breaks(n = 8)(limits)
          sort(unique(c(b, 1, if (is.finite(max_nok)) max_nok)))
        },
        expand = expansion(mult = c(0.02, 0.04))
      ) +
      scale_color_manual(values = palette_sensori, name = "Sensore") +
      labs(x = NULL, y = "NOK") +
      theme_minimal(base_size = 13) +
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
  
  output$nokHistoryPlot <- renderGirafe({
    req(input$activationPlot_px_width > 0)
    girafe(
      ggobj      = render_nok_history_gg(),
      width_svg  = input$activationPlot_px_width / 72,
      height_svg = 550 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;")
      )
    )
  })
  
  output$modal_nokPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    girafe(
      ggobj      = render_nok_history_gg(),
      width_svg  = w_px / 72,
      height_svg = 420 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;")
      )
    )
  })
}

shinyApp(ui, server)