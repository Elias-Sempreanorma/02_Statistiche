# VERSIONE FIX FILTRI 2026-09-10
# Debounce 800 ms + isolamento del renderUI per eliminare il loop.
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
library(DT)

# Le immagini non sono nella cartella www: le espongo a Shiny con un
# resource path dedicato. Se la cartella non esiste, l'app continua comunque
# a funzionare e nella Home non viene mostrato alcuno schema.
cds_images_dir <- here("00_Data", "02_CdS")
if (dir.exists(cds_images_dir)) {
  addResourcePath("cds-images", cds_images_dir)
}

dati <- readRDS(here("02_Output", "Test", "sensor_count_increment.rds")) |>
  mutate(field = if_else(is.na(field) | trimws(field) == "", "(Non specificato)", field))

life_data <- readRDS(here("02_Output", "Test", "raw_data.rds")) |>
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
# Immagini disponibili e coordinate dei punti interattivi.
# Le coordinate sono espresse inizialmente in pixel rispetto all'immagine
# originale e poi convertite in percentuale, cosi' restano corrette anche
# quando l'immagine si ridimensiona.
# ---------------------------------------------------------------------------
immagini_progetti <- data.frame(
  project = c("A3020", "C14GR", "E11RI", "F0400"),
  file_name = c(
    "Giacomini_G1_A3020.png",
    "Giacomini_G1_C14GR.png",
    "Giacomini_G1_E11RI.png",
    "Giacomini_G1_F0400.png"
  ),
  stringsAsFactors = FALSE
)

mappa_sensori <- bind_rows(
  data.frame(
    project = "A3020", image_width = 964, image_height = 519,
    cds_name = c(
      "EML", "REG", "PE2", "BIM2", "FCM5", "FCM6", "FCM4",
      "FCM3", "FCM2", "FCM1", "FCM7", "FCM8", "FCM13", "PE1",
      "FCM9", "FCM12", "FCM11", "FCM10", "BIM1"
    ),
    x = c(536, 556, 96, 86, 129, 144, 159, 324, 341, 372,
          62, 62, 473, 457, 371, 172, 324, 341, 169),
    y = c(136, 185, 228, 249, 268, 268, 267, 278, 277, 295,
          307, 330, 338, 343, 359, 362, 375, 375, 384)
  ),
  data.frame(
    project = "C14GR", image_width = 964, image_height = 700,
    cds_name = c(
      "EML", "REG", "MB4", "MB3", "SM3", "MB2", "SM2", "SC1",
      "SM1", "FCM1", "MB1", "PE1", "RE1", "MB5", "MB6", "SM4"
    ),
    x = c(683, 722, 367, 391, 460, 465, 569, 602,
          768, 725, 761, 469, 560, 366, 396, 296),
    y = c(58, 58, 155, 156, 145, 223, 194, 242,
          397, 431, 439, 461, 460, 478, 477, 464)
  ),
  data.frame(
    project = "E11RI", image_width = 964, image_height = 712,
    cds_name = c(
      "MF1", "MF2", "FCM6", "FCM5", "RA2", "FCM7", "RA6",
      "FCM4", "UPe2", "MBe2", "RA5", "FCM3", "FCM2", "FCM1",
      "MBe1", "UPe1", "RA4", "RA7", "UPe3", "MB4", "MB3",
      "FCM9", "FTCe1", "UPe4", "FCM8", "FMEe3", "RA3", "RA1",
      "FMEe1", "MB1", "MB2", "FMEe2", "EML", "REG"
    ),
    x = c(121, 229, 463, 522, 326, 371, 505, 615, 560, 559, 559,
          615, 522, 463, 497, 514, 484, 426, 466, 287, 307, 361,
          384, 426, 371, 343, 260, 238, 121, 160, 200, 227, 780, 810),
    y = c(176, 176, 186, 186, 239, 269, 240, 279, 277, 295, 334,
          357, 430, 430, 354, 379, 364, 270, 245, 303, 323, 298,
          318, 318, 357, 282, 353, 390, 362, 362, 362, 354, 489, 518)
  ),
  data.frame(
    project = "F0400", image_width = 966, image_height = 712,
    cds_name = c(
      "EML", "REG", "MB6", "MB5", "MB4", "MB3", "MB2", "SM3",
      "SM2", "R3", "PE1", "FCM1", "FTC1", "MB1", "SM1"
    ),
    x = c(302, 263, 412, 234, 234, 595, 595, 223,
          598, 576, 233, 180, 535, 502, 602),
    y = c(108, 108, 184, 206, 227, 205, 223, 289,
          289, 392, 397, 430, 446, 495, 473)
  )
) |>
  mutate(
    cds_key = str_to_upper(str_squish(cds_name)),
    x_pct = 100 * x / image_width,
    y_pct = 100 * y / image_height
  )

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
      function aggiornaLarghezzaModal() {
        setTimeout(function () {
          var el = document.querySelector('.modal-body');

          if (el && el.clientWidth > 0) {
            Shiny.setInputValue(
              'modal_px_width',
              el.clientWidth,
              {priority: 'event'}
            );
          }
        }, 100);
      }

      $(document).on('shown.bs.modal', aggiornaLarghezzaModal);
      $(window).on('resize', aggiornaLarghezzaModal);

      $(document).on('click', '.sensor-hotspot', function () {
        Shiny.setInputValue(
          'schema_sensor_click',
          {
            sensor: $(this).attr('data-sensor'),
            nonce: Date.now()
          },
          {priority: 'event'}
        );
      });

      $(document).on('keydown', '.sensor-hotspot', function (event) {
        if (event.key === 'Enter' || event.key === ' ') {
          event.preventDefault();
          $(this).trigger('click');
        }
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
        min-height: 132px;
        background: #FFFFFF;
        border: 1px solid #E4E7EB;
        border-radius: 10px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
        transition: transform 0.15s ease, box-shadow 0.15s ease, border-color 0.15s ease;
        text-align: left;
        white-space: normal;
        padding: 18px 16px;
        margin-bottom: 16px;
        color: #4A4A4A;
      }
      .card-home:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 18px rgba(0,0,0,0.10);
        border-color: #7FA6C9;
      }
      .card-home .card-icona {
        font-size: 25px;
        color: #7FA6C9;
        margin-bottom: 7px;
      }
      .card-home h4 {
        font-weight: 700;
        margin: 0 0 6px 0;
      }
      .card-home p {
        font-size: 13px;
        color: #8A94A0;
        margin: 0;
      }
      .schema-home {
        width: 100%;
        display: flex;
        justify-content: center;
        margin: 0 0 28px 0;
      }
      .schema-frame {
        position: relative;
        width: 100%;
        max-width: 100%;
      }
      .schema-frame img {
        display: block;
        width: 100%;
        height: auto;
        border: 1px solid #E4E7EB;
        border-radius: 8px;
        background: #FFFFFF;
      }
      .sensor-hotspot {
        position: absolute;
        width: 24px;
        height: 24px;
        transform: translate(-50%, -50%);
        border: 2px solid transparent;
        border-radius: 50%;
        background: transparent;
        box-shadow: none;
        cursor: pointer;
        z-index: 2;
        outline: none;
      }
      .sensor-hotspot:hover,
      .sensor-hotspot:focus {
        background: rgba(127, 166, 201, 0.58);
        border-color: #2C3E50;
        z-index: 20;
      }
      .sensor-tooltip {
        display: none;
        position: absolute;
        left: 50%;
        bottom: calc(100% + 10px);
        transform: translateX(-50%);
        min-width: 220px;
        padding: 10px 12px;
        border-radius: 7px;
        background: #2C3E50;
        color: #FFFFFF;
        text-align: left;
        font-size: 13px;
        line-height: 1.5;
        white-space: nowrap;
        box-shadow: 0 4px 12px rgba(0,0,0,0.25);
        pointer-events: none;
      }
      .sensor-tooltip::after {
        content: '';
        position: absolute;
        top: 100%;
        left: 50%;
        margin-left: -6px;
        border-width: 6px;
        border-style: solid;
        border-color: #2C3E50 transparent transparent transparent;
      }
      .sensor-hotspot:hover .sensor-tooltip,
      .sensor-hotspot:focus .sensor-tooltip {
        display: block;
      }
      .sensor-tooltip-title {
        display: block;
        margin-bottom: 4px;
        font-weight: 700;
      }
      .home-menu {
        padding-right: 18px;
      }
      .modal-filters {
        background: #F4F6F8;
        border-radius: 7px;
        padding: 14px 16px 4px 16px;
        margin-bottom: 18px;
      }
      .modal-data-button {
        margin: 6px 0 14px 0;
        text-align: right;
      }
      .modal-data-panel {
        margin: 0 0 22px 0;
        padding: 12px;
        border: 1px solid #E4E7EB;
        border-radius: 7px;
        background: #FFFFFF;
      }
      .modal-nav-bar {
        margin-bottom: 20px;
        padding-bottom: 14px;
        border-bottom: 2px solid #E4E7EB;
      }
      .modal-nav-bar .btn-group .btn {
        font-weight: 600;
        font-size: 14px;
        padding: 8px 18px;
      }
      @media (max-width: 700px) {
        .home-menu {
          padding-right: 15px;
        }
        .sensor-hotspot {
          width: 18px;
          height: 18px;
        }
        .sensor-tooltip {
          min-width: 190px;
          font-size: 12px;
          white-space: normal;
        }
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
  
  # Home unica: menu verticale a sinistra e schema della macchina a destra.
  br(),
  fluidRow(
    column(
      3,
      class = "home-menu",
      actionButton(
        "home_attivazioni",
        label = div(
          icon("chart-bar", class = "card-icona"),
          h4("Conteggio attivazioni"),
          p("Grafico a barre e trend nel tempo per sensore")
        ),
        class = "card-home"
      ),
      actionButton(
        "home_vita",
        label = div(
          icon("gauge", class = "card-icona"),
          h4("Vita sensori"),
          p("Stato dei sensori rispetto alle soglie B10dSAN e T10d")
        ),
        class = "card-home"
      ),
      actionButton(
        "home_nok",
        label = div(
          icon("chart-line", class = "card-icona"),
          h4("Storico NOK"),
          p("KPI e andamento storico del NOK per sensore")
        ),
        class = "card-home"
      )
    ),
    # Se non esiste un'immagine associata al progetto selezionato,
    # renderUI restituisce NULL e la colonna destra resta vuota.
    column(9, uiOutput("schema_sensori"))
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
  
  # Tabelle dati mostrate direttamente all'interno dei modal.
  mostra_dati_attivazioni <- reactiveVal(FALSE)
  mostra_dati_trend <- reactiveVal(FALSE)
  mostra_dati_tank <- reactiveVal(FALSE)
  mostra_dati_nok <- reactiveVal(FALSE)
  
  # I filtri vengono applicati solo dopo una breve pausa dall'ultima scelta.
  # In questo modo una selezione multipla genera un solo aggiornamento.
  ritardo_filtri_ms <- 800
  
  normalizza_sensori <- function(x) {
    if (is.null(x)) character(0) else sort(unique(as.character(x)))
  }
  
  sensori_modal_correnti <- reactiveVal(character(0))
  date_modal_correnti <- reactiveVal(c(data_min, data_max))
  granularita_attivazioni_corrente <- reactiveVal("Giorno")
  granularita_nok_corrente <- reactiveVal("Giorno")
  
  filtri_principali <- reactive({
    req(input$macchina, input$date)
    
    list(
      macchina = input$macchina,
      date = as.Date(input$date),
      sensori = normalizza_sensori(input$sensori)
    )
  }) |>
    debounce(millis = ritardo_filtri_ms)
  
  # ---------------------------------------------------------------------
  # Finestre complete aperte dalla Home. Periodo e sensori partono dai
  # valori generali; le modifiche vengono riportate alla Home solo dopo
  # la pausa definita da ritardo_filtri_ms.
  # ---------------------------------------------------------------------
  # Modal unico con navigazione interna tramite radioGroupButtons.
  # apri_modal() viene chiamata dai tre bottoni Home: imposta la vista
  # iniziale e mostra il dialogo. Il contenuto cambia senza chiuderlo.
  # ---------------------------------------------------------------------
  apri_modal <- function(vista_iniziale, sensori_iniziali = NULL) {
    if (is.null(sensori_iniziali)) {
      sensori_iniziali <- isolate(input$sensori)
    }
    
    sensori_modal_correnti(normalizza_sensori(sensori_iniziali))
    date_modal_correnti(as.Date(isolate(input$date)))
    granularita_attivazioni_corrente("Giorno")
    granularita_nok_corrente("Giorno")
    
    mostra_dati_attivazioni(FALSE)
    mostra_dati_trend(FALSE)
    mostra_dati_tank(FALSE)
    mostra_dati_nok(FALSE)
    
    showModal(modalDialog(
      title = NULL,
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Chiudi"),
      
      div(
        class = "modal-nav-bar",
        radioGroupButtons(
          "vista_selezionata",
          label = NULL,
          choices = c(
            "Conteggio attivazioni" = "attivazioni",
            "Vita sensori"          = "vita",
            "Storico NOK"           = "nok"
          ),
          selected = vista_iniziale,
          status   = "primary",
          size     = "normal",
          width    = "auto"
        )
      ),
      
      uiOutput("modal_contenuto")
    ))
  }
  
  observeEvent(input$home_attivazioni, {
    req(input$macchina, input$date)
    apri_modal("attivazioni")
  })
  
  observeEvent(input$home_vita, {
    req(input$macchina)
    apri_modal("vita")
  })
  
  observeEvent(input$home_nok, {
    req(input$macchina, input$date)
    apri_modal("nok")
  })
  
  # Click su un CdS nello schema: seleziona quel sensore e apre la tenda
  # direttamente sulla vista Conteggio attivazioni.
  observeEvent(input$schema_sensor_click, {
    req(input$macchina, input$date, input$schema_sensor_click$sensor)
    
    sensore_cliccato <- input$schema_sensor_click$sensor
    sensori_validi <- sensori_lookup |>
      filter(coupon == input$macchina) |>
      pull(cds_name)
    
    req(sensore_cliccato %in% sensori_validi)
    
    updatePickerInput(
      session,
      "sensori",
      selected = sensore_cliccato
    )
    
    apri_modal("attivazioni", sensore_cliccato)
  })
  
  # Quando si cambia vista, azzera la visibilita' delle tabelle dati
  observeEvent(input$vista_selezionata, {
    mostra_dati_attivazioni(FALSE)
    mostra_dati_trend(FALSE)
    mostra_dati_tank(FALSE)
    mostra_dati_nok(FALSE)
  }, ignoreInit = TRUE)
  
  # Contenuto del modal: si aggiorna al cambio di vista senza chiudere il dialogo
  output$modal_contenuto <- renderUI({
    req(input$vista_selezionata, input$macchina)
    
    vista <- input$vista_selezionata
    
    sensori_macchina <- sensori_lookup |>
      filter(coupon == input$macchina)
    
    sensori_selezionati <- intersect(
      isolate(sensori_modal_correnti()),
      sensori_macchina$cds_name
    )
    
    date_selezionate <- isolate(date_modal_correnti())
    
    scelte_sensori <- setNames(
      sensori_macchina$cds_name,
      paste(sensori_macchina$cds_name, sensori_macchina$sensor_description, sep = " - ")
    )
    
    picker_opts <- pickerOptions(
      actionsBox = TRUE,
      liveSearch = TRUE,
      selectedTextFormat = "count > 3",
      countSelectedText = "{0} sensori selezionati"
    )
    
    if (vista == "attivazioni") {
      tagList(
        div(
          class = "modal-filters",
          fluidRow(
            column(4,
                   dateRangeInput(
                     "modal_date_attivazioni", "Periodo:",
                     start = date_selezionate[1], end = date_selezionate[2],
                     min = data_min, max = data_max,
                     format = "dd-mm-yyyy", separator = " a ", language = "it"
                   )
            ),
            column(8,
                   pickerInput(
                     "modal_sensori_attivazioni", "Sensori:",
                     choices = scelte_sensori, selected = sensori_selezionati,
                     multiple = TRUE, options = picker_opts
                   )
            )
          ),
          radioButtons(
            "modal_granularita_attivazioni", "Raggruppamento:",
            choices = c("Giorno", "Settimana", "Mese", "Trimestre", "Anno"),
            selected = isolate(granularita_attivazioni_corrente()), inline = TRUE
          )
        ),
        h4("Conteggio attivazioni", class = "titolo-sezione"),
        girafeOutput("modal_activationPlot", height = "380px"),
        div(
          class = "modal-data-button",
          actionButton("modal_btn_dati_attivazioni", "Dati", icon = icon("table"), class = "btn-sm btn-default")
        ),
        uiOutput("modal_panel_dati_attivazioni"),
        h4("Andamento per sensore", class = "titolo-sezione"),
        br(),
        girafeOutput("modal_activationTrendPlot", height = "380px"),
        div(
          class = "modal-data-button",
          actionButton("modal_btn_dati_trend", "Dati", icon = icon("table"), class = "btn-sm btn-default")
        ),
        uiOutput("modal_panel_dati_trend")
      )
      
    } else if (vista == "vita") {
      tagList(
        div(
          class = "modal-filters",
          pickerInput(
            "modal_sensori_vita", "Sensori:",
            choices = scelte_sensori, selected = sensori_selezionati,
            multiple = TRUE, options = picker_opts
          )
        ),
        plotOutput("modal_tankPlot", height = "430px"),
        div(
          class = "modal-data-button",
          actionButton("modal_btn_dati_tank", "Dati", icon = icon("table"), class = "btn-sm btn-default")
        ),
        uiOutput("modal_panel_dati_tank")
      )
      
    } else if (vista == "nok") {
      tagList(
        div(
          class = "modal-filters",
          fluidRow(
            column(4,
                   dateRangeInput(
                     "modal_date_nok", "Periodo:",
                     start = date_selezionate[1], end = date_selezionate[2],
                     min = data_min, max = data_max,
                     format = "dd-mm-yyyy", separator = " a ", language = "it"
                   )
            ),
            column(8,
                   pickerInput(
                     "modal_sensori_nok", "Sensori:",
                     choices = scelte_sensori, selected = sensori_selezionati,
                     multiple = TRUE, options = picker_opts
                   )
            )
          ),
          radioButtons(
            "modal_granularita_nok", "Raggruppamento:",
            choices = c("Giorno", "Settimana", "Mese", "Trimestre", "Anno"),
            selected = isolate(granularita_nok_corrente()), inline = TRUE
          )
        ),
        h4("NOK per sensore", class = "titolo-sezione"),
        tableOutput("modal_nok_table"),
        h4("Andamento storico del NOK", class = "titolo-sezione"),
        girafeOutput("modal_nokPlot", height = "520px"),
        div(
          class = "modal-data-button",
          actionButton("modal_btn_dati_nok", "Dati", icon = icon("table"), class = "btn-sm btn-default")
        ),
        uiOutput("modal_panel_dati_nok")
      )
    }
  })
  
  observeEvent(input$modal_btn_dati_attivazioni, {
    mostra_dati_attivazioni(!mostra_dati_attivazioni())
  })
  
  observeEvent(input$modal_btn_dati_trend, {
    mostra_dati_trend(!mostra_dati_trend())
  })
  
  observeEvent(input$modal_btn_dati_tank, {
    mostra_dati_tank(!mostra_dati_tank())
  })
  
  observeEvent(input$modal_btn_dati_nok, {
    mostra_dati_nok(!mostra_dati_nok())
  })
  
  # ---------------------------------------------------------------------
  # Coda ottimizzata dei filtri modal.
  # Il debounce aspetta 800 ms dall'ultima modifica; solo allora aggiorna
  # grafici, stato del modal e filtri principali. Il contenuto del modal
  # non dipende direttamente dai filtri principali, evitando il loop di
  # distruzione e ricreazione continua degli input.
  # ---------------------------------------------------------------------
  filtri_attivazioni_modal <- reactive({
    req(
      identical(input$vista_selezionata, "attivazioni"),
      input$modal_date_attivazioni,
      input$modal_granularita_attivazioni
    )
    
    list(
      macchina = input$macchina,
      date = as.Date(input$modal_date_attivazioni),
      sensori = normalizza_sensori(input$modal_sensori_attivazioni),
      granularita = input$modal_granularita_attivazioni
    )
  }) |>
    debounce(millis = ritardo_filtri_ms)
  
  filtri_vita_modal <- reactive({
    req(identical(input$vista_selezionata, "vita"))
    
    list(
      macchina = input$macchina,
      sensori = normalizza_sensori(input$modal_sensori_vita)
    )
  }) |>
    debounce(millis = ritardo_filtri_ms)
  
  filtri_nok_modal <- reactive({
    req(
      identical(input$vista_selezionata, "nok"),
      input$modal_date_nok,
      input$modal_granularita_nok
    )
    
    list(
      macchina = input$macchina,
      date = as.Date(input$modal_date_nok),
      sensori = normalizza_sensori(input$modal_sensori_nok),
      granularita = input$modal_granularita_nok
    )
  }) |>
    debounce(millis = ritardo_filtri_ms)
  
  applica_filtri_principali <- function(filtri) {
    sensori_modal_correnti(filtri$sensori)
    
    if (!identical(normalizza_sensori(input$sensori), filtri$sensori)) {
      updatePickerInput(session, "sensori", selected = filtri$sensori)
    }
    
    if (!is.null(filtri$date)) {
      date_modal_correnti(filtri$date)
      
      if (!identical(as.Date(input$date), filtri$date)) {
        updateDateRangeInput(
          session,
          "date",
          start = filtri$date[1],
          end = filtri$date[2]
        )
      }
    }
  }
  
  observeEvent(filtri_attivazioni_modal(), {
    filtri <- filtri_attivazioni_modal()
    granularita_attivazioni_corrente(filtri$granularita)
    applica_filtri_principali(filtri)
  }, ignoreInit = TRUE)
  
  observeEvent(filtri_vita_modal(), {
    applica_filtri_principali(filtri_vita_modal())
  }, ignoreInit = TRUE)
  
  observeEvent(filtri_nok_modal(), {
    filtri <- filtri_nok_modal()
    granularita_nok_corrente(filtri$granularita)
    applica_filtri_principali(filtri)
  }, ignoreInit = TRUE)
  
  output$modal_panel_dati_attivazioni <- renderUI({
    if (!mostra_dati_attivazioni()) return(NULL)
    div(class = "modal-data-panel", DTOutput("modal_tabella_dati_attivazioni"))
  })
  
  output$modal_panel_dati_trend <- renderUI({
    if (!mostra_dati_trend()) return(NULL)
    div(class = "modal-data-panel", DTOutput("modal_tabella_dati_trend"))
  })
  
  output$modal_panel_dati_tank <- renderUI({
    if (!mostra_dati_tank()) return(NULL)
    div(class = "modal-data-panel", DTOutput("modal_tabella_dati_tank"))
  })
  
  output$modal_panel_dati_nok <- renderUI({
    if (!mostra_dati_nok()) return(NULL)
    div(class = "modal-data-panel", DTOutput("modal_tabella_dati_nok"))
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
    
    filtri <- filtri_principali()
    req(length(filtri$sensori) > 0)
    
    dati |>
      ungroup() |>
      filter(
        coupon == filtri$macchina,
        cds_name %in% filtri$sensori
      ) |>
      group_by(cds_name, sensor_description, day) |>
      summarise(daily_value = sum(increment, na.rm = TRUE) / unique(daily_uptime), .groups = "drop")
  }) |>
    bindCache(filtri_principali()$macchina, filtri_principali()$sensori)
  
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
          TRUE ~ NMN / NMM
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
    
    progetto <- macchine_lookup |>
      filter(coupon == filtri$macchina) |>
      distinct(project) |>
      slice_head(n = 1) |>
      pull(project)
    
    if (length(progetto) == 0 || is.na(progetto)) {
      return(NULL)
    }
    
    immagine <- immagini_progetti |>
      filter(project == progetto) |>
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
      group_by(cds_key) |>
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
        summarise(daily_count = sum(increment, na.rm = TRUE), .groups = "drop") |>
        group_by(cds_name) |>
        summarise(N_medio = mean(daily_count, na.rm = TRUE), .groups = "drop") |>
        mutate(cds_key = str_to_upper(str_squish(cds_name)))
      
      kpi_nok() |>
        mutate(cds_key = str_to_upper(str_squish(cds_name))) |>
        select(cds_key, NOK) |>
        group_by(cds_key) |>
        summarise(NOK = first(NOK), .groups = "drop") |>
        left_join(select(raw_avg, cds_key, N_medio), by = "cds_key")
    }
    
    punti <- mappa_sensori |>
      filter(project == progetto) |>
      select(project, cds_key, x_pct, y_pct) |>
      inner_join(sensori_macchina, by = "cds_key") |>
      left_join(metriche, by = "cds_key")
    
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
      nok_periodo <- formatta_numero(punto$NOK, 3)
      
      tags$span(
        class = "sensor-hotspot",
        tabindex = "0",
        `data-sensor` = punto$cds_name,
        `aria-label` = paste0(
          descrizione,
          "; attivazioni giornaliere medie: ", attivazioni_medie,
          "; NOK: ", nok_periodo
        ),
        style = sprintf(
          "left: %.4f%%; top: %.4f%%;",
          punto$x_pct,
          punto$y_pct
        ),
        tags$span(
          class = "sensor-tooltip",
          tags$span(descrizione, class = "sensor-tooltip-title"),
          tags$div("Attivazioni giornaliere medie: ", tags$strong(attivazioni_medie)),
          tags$div("NOK: ", tags$strong(nok_periodo))
        )
      )
    })
    
    div(
      class = "schema-home",
      div(
        class = "schema-frame",
        tags$img(
          src = paste0("cds-images/", immagine$file_name),
          alt = paste("Schema sensori del progetto", progetto)
        ),
        hotspot
      )
    )
  })
  
  valori_giornalieri_modal_nok <- reactive({
    
    filtri <- filtri_nok_modal()
    req(length(filtri$sensori) > 0)
    
    dati |>
      ungroup() |>
      filter(
        coupon == filtri$macchina,
        cds_name %in% filtri$sensori
      ) |>
      group_by(cds_name, sensor_description, day) |>
      summarise(
        daily_value = sum(increment, na.rm = TRUE) / unique(daily_uptime),
        .groups = "drop"
      )
  }) |>
    bindCache(filtri_nok_modal()$macchina, filtri_nok_modal()$sensori)
  
  nmn_storico_modal_nok <- reactive({
    valori_giornalieri_modal_nok() |>
      group_by(cds_name, sensor_description) |>
      summarise(NMN = mean(daily_value, na.rm = TRUE), .groups = "drop")
  })
  
  kpi_nok_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    nmm_per_sensore <- valori_giornalieri_modal_nok() |>
      filter(
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      group_by(cds_name, sensor_description) |>
      summarise(NMM = mean(daily_value, na.rm = TRUE), .groups = "drop")
    
    nmn_storico_modal_nok() |>
      left_join(nmm_per_sensore, by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK = case_when(
          is.na(NMN) | is.na(NMM) | NMM == 0 ~ NA_real_,
          TRUE ~ NMN / NMM
        )
      ) |>
      ordina_naturale()
  })
  
  output$modal_nok_table <- renderTable({
    
    kpi <- kpi_nok_modal()
    
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
    
    tanks <- tank_data_modal()
    
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
  
  output$modal_tankPlot <- renderPlot({ render_tank_plot() })
  
  # ---------------------------------------------------------------------
  # Dati per il grafico attivazioni: raggruppati nel periodo scelto
  # (giorno/settimana/mese/trimestre/anno). Usati sia dal grafico a
  # barre (facet per periodo) sia dal grafico trend (facet per sensore).
  # ---------------------------------------------------------------------
  dati_grafico_modal <- reactive({
    
    filtri <- filtri_attivazioni_modal()
    req(length(filtri$sensori) > 0)
    
    dati |>
      filter(
        coupon == filtri$macchina,
        cds_name %in% filtri$sensori,
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      mutate(periodo = periodo_bucket(day, filtri$granularita)) |>
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
        periodo_label = formatta_periodo_label(
          periodo,
          filtri$granularita
        )
      ) |>
      ordina_naturale() |>
      mutate(
        etichetta = factor(etichetta, levels = unique(etichetta)),
        etichetta_completa = factor(etichetta_completa, levels = unique(etichetta_completa)),
        periodo_label = factor(periodo_label, levels = unique(periodo_label[order(periodo)]))
      )
  }) |>
    bindCache(filtri_attivazioni_modal())
  
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
    
    grafico <- dati_grafico_modal()
    
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
    
    grafico <- dati_grafico_modal()
    granularita <- filtri_attivazioni_modal()$granularita
    
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
        labels = formatta_periodo_label(
          breaks_periodo,
          granularita
        )
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
  kpi_nok_storico_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    valori_giornalieri_modal_nok() |>
      filter(
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      mutate(
        periodo = as.Date(periodo_bucket(day, filtri$granularita))
      ) |>
      group_by(periodo, cds_name, sensor_description) |>
      summarise(
        valore_periodo = mean(daily_value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      left_join(nmn_storico_modal_nok(), by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK_periodo = case_when(
          is.na(valore_periodo) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ valore_periodo / NMN
        ),
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")
      ) |>
      arrange(periodo)
  }) |>
    bindCache(filtri_nok_modal())
  
  render_nok_history_gg <- function() {
    
    storico <- kpi_nok_storico_modal()
    granularita <- filtri_nok_modal()$granularita
    
    validate(
      need(nrow(storico) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    storico <- storico |>
      ordina_naturale() |>
      mutate(
        etichetta_sensore = factor(etichetta_sensore, levels = unique(etichetta_sensore)),
        periodo_label = formatta_periodo_label(
          periodo,
          granularita
        )
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
        labels = formatta_periodo_label(
          breaks_periodo,
          granularita
        )
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
  
  # -----------------------------------------------------------------------
  # Tabelle mostrate/nascoste dentro le finestre principali.
  # -----------------------------------------------------------------------
  
  output$modal_tabella_dati_attivazioni <- renderDT({
    dati_grafico_modal() |>
      transmute(
        Periodo     = as.character(periodo_label),
        Sensore     = as.character(etichetta_completa),
        Attivazioni = attivazioni
      ) |>
      arrange(Periodo, Sensore)
  }, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
  
  output$modal_tabella_dati_trend <- renderDT({
    dati_grafico_modal() |>
      transmute(
        Sensore     = as.character(etichetta_completa),
        Periodo     = as.character(periodo_label),
        Attivazioni = attivazioni
      ) |>
      arrange(Sensore, Periodo)
  }, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
  
  output$modal_tabella_dati_tank <- renderDT({
    tank_data_modal() |>
      transmute(
        Sensore = gsub("\n", " ", as.character(etichetta_sensore)),
        Tipo    = tipo,
        Valore  = round(valore, 1),
        Massimo = massimo,
        `%`     = ifelse(
          is.na(percentuale),
          NA_character_,
          paste0(round(percentuale, 1), " %")
        )
      )
  }, rownames = FALSE, options = list(pageLength = 25, dom = "t"))
  
  output$modal_tabella_dati_nok <- renderDT({
    granularita <- filtri_nok_modal()$granularita
    
    kpi_nok_storico_modal() |>
      transmute(
        Periodo = formatta_periodo_label(
          periodo,
          granularita
        ),
        Sensore = etichetta_sensore,
        NOK     = round(NOK_periodo, 3)
      ) |>
      arrange(Periodo, Sensore)
  }, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
}

shinyApp(ui, server)
