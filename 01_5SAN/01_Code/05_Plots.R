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
library(dbscan)

# Modalita' applicativa:
# - internal: dashboard completa con selezione Azienda/Stabilimento/Macchina
# - web: macchina fissata dal parametro ?coupon=... della querystring
APP_MODE <- tolower(trimws(Sys.getenv("APP_MODE", unset = "internal")))
if (!APP_MODE %in% c("internal", "web")) {
  warning("APP_MODE non riconosciuto: uso 'internal'.")
  APP_MODE <- "internal"
}
IS_WEB_MODE <- identical(APP_MODE, "web")

# Le immagini non sono nella cartella www: le espongo a Shiny con un
# resource path dedicato. Se la cartella non esiste, l'app continua comunque
# a funzionare e nella Home non viene mostrato alcuno schema.
cds_images_dir <- here("00_Data", "02_CdS")
if (dir.exists(cds_images_dir)) {
  addResourcePath("cds-images", cds_images_dir)
}

# DOCX statici scaricabili dalla dashboard. Restano fuori da www:
# il download passa quindi sempre da Shiny.
documenti_dir <- here("00_Data", "03_Documenti")
documenti_files <- if (dir.exists(documenti_dir)) {
  sort(
    list.files(
      documenti_dir,
      pattern = "\\.docx$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  )
} else {
  character(0)
}

dati <- readRDS(here("02_Output", "sensor_count_increment.rds")) |>
  mutate(field = if_else(is.na(field) | trimws(field) == "", "(Non specificato)", field))

# Compatibilita' con dataset generati prima dell'introduzione delle ore aperte.
# Dopo il rerun completo dell'ETL la colonna e' presente realmente.
if (!"daily_open_hours" %in% names(dati)) {
  dati <- dati |>
    mutate(daily_open_hours = 0)
}

# Compatibilita' con dataset precedenti al flag di disconnessione gateway.
if (!"gateway_disconnect_affected" %in% names(dati)) {
  dati <- dati |>
    mutate(gateway_disconnect_affected = FALSE)
}

nota_disconnessione_gateway <-
  ", dato possibilmente alterato da una disconnessione del gateway"

raw_life_data <- readRDS(here("02_Output", "raw_data.rds")) |>
  mutate(timestamp = with_tz(timestamp, "Europe/Rome"))

# La vita temporale del componente non usa piu' il campo lifetime inviato
# dal dispositivo. Parte dalla prima osservazione assoluta del CdS sulla
# macchina ed e' quindi legata esclusivamente a coupon + cds_name:
# eventuali cambi di sensor_id/codice non azzerano il conteggio.
life_reference_ts <- Sys.time()

life_data <- raw_life_data |>
  filter(
    !is.na(coupon),
    !is.na(cds_name),
    !is.na(timestamp)
  ) |>
  arrange(timestamp) |>
  group_by(coupon, cds_name) |>
  summarise(
    cds_vds = last(cds_vds[!is.na(cds_vds)], default = NA_real_),
    cds_t10d = last(cds_t10d[!is.na(cds_t10d)], default = NA_real_),
    count = if (all(is.na(count))) NA_real_ else max(count, na.rm = TRUE),
    offset = if (all(is.na(offset))) NA_real_ else max(offset, na.rm = TRUE),
    first_seen = min(timestamp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    count = count + offset,
    lifetime = as.numeric(
      difftime(life_reference_ts, first_seen, units = "secs")
    ) / 60 / 60 / 24 / 360
  ) |>
  select(coupon, cds_name, cds_vds, cds_t10d, count, lifetime)

# Anagrafica sensori (coupon + cds_name -> descrizione), usata per
# arricchire le etichette dei serbatoi
sensori_info <- dati |>
  distinct(coupon, cds_name, sensor_description)

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

# ---------------------------------------------------------------------------
# Base oraria precalcolata una sola volta all'avvio di Shiny.
# Le reactive dei grafici filtrano questi oggetti gia' aggregati invece di
# ricostruire ogni volta tutto lo storico della macchina.
# ---------------------------------------------------------------------------
dati_attivazioni_orarie_base <- dati |>
  mutate(
    timestamp_local = with_tz(timestamp, "Europe/Rome"),
    day_oraria = as.Date(timestamp_local, tz = "Europe/Rome"),
    hour_oraria = floor_date(timestamp_local, "hour")
  ) |>
  group_by(
    coupon,
    day = day_oraria,
    hour = hour_oraria,
    cds_name,
    sensor_description
  ) |>
  summarise(
    attivazioni_orarie = if (
      any(is.finite(increment))
    ) {
      sum(increment[is.finite(increment)], na.rm = TRUE)
    } else {
      NA_real_
    },
    gateway_disconnect_affected = any(
      gateway_disconnect_affected,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

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

mediane_attivazioni_orarie <- dati_attivazioni_orarie_base |>
  filter(
    is.finite(attivazioni_orarie),
    attivazioni_orarie > 0
  ) |>
  group_by(coupon, cds_name, sensor_description) |>
  summarise(
    mediana_oraria = median(attivazioni_orarie, na.rm = TRUE),
    .groups = "drop"
  )

data_min <- min(dati$day, na.rm = TRUE)
data_max <- max(dati$day, na.rm = TRUE)
data_start_default <- max(data_min, data_max - 27)

# ---------------------------------------------------------------------------
# Immagini disponibili e coordinate dei punti interattivi.
# Le coordinate sono espresse inizialmente in pixel rispetto all'immagine
# originale e poi convertite in percentuale, cosi' restano corrette anche
# quando l'immagine si ridimensiona.
# ---------------------------------------------------------------------------
immagini_progetti <- data.frame(
  project = c("A3020", "A1220", "C14GR", "E11RI", "F0400"),
  file_name = c(
    "Giacomini_G1_A3020.png",
    "Giacomini_G1_A1220.png",
    "Giacomini_G1_C14GR.png",
    "Giacomini_G1_E11RI.png",
    "Giacomini_G1_F0400.png"
  ),
  stringsAsFactors = FALSE
)

mappa_sensori <- bind_rows(
  data.frame(
    project = "A3020", image_width = 964, image_height = 345,
    cds_name = c(
      "EML", "REG", "PE2", "BIM2", "FCM5", "FCM6", "FCM4",
      "FCM3", "FCM2", "FCM1", "FCM7", "FCM8", "FCM13", "PE1",
      "FCM9", "FCM12", "FCM11", "FCM10", "BIM1"
    ),
    x = c(
      536, 556, 96, 86, 129, 144, 159, 324, 341, 372,
      62, 62, 473, 457, 371, 172, 324, 341, 169
    ),
    y = c(
      29, 78, 121, 142, 161, 161, 160, 171, 170, 188,
      200, 223, 231, 236, 252, 255, 268, 268, 277
    )
  ),
  data.frame(
    project = "A1220", image_width = 898, image_height = 325,
    cds_name = c(
      "EML", "REG", "FCM5", "FCM4", "FCM3", "FCM2", "FCM1",
      "FCM7", "FCM8", "FCM12", "FCM11", "FCM10", "FCM9",
      "FCM14", "FCM15", "PE1", "FCM13", "BIM1 - A", "BIM2 - A"
    ),
    x = c(
      485, 505, 93, 108, 273, 290, 321,
      11, 11, 100, 273, 290, 320,
      355, 387, 406, 422, 90, 65
    ),
    y = c(
      32, 81, 164, 163, 174, 173, 191,
      203, 226, 254, 271, 271, 255,
      242, 236, 239, 234, 304, 137
    )
  ),
  data.frame(
    project = "C14GR", image_width = 774, image_height = 642,
    cds_name = c(
      "EML", "REG", "MB4", "MB3", "SM3", "MB2", "SM2", "SC1",
      "SM1", "FCM1", "MB1", "PE1", "RE1", "MB5", "MB6", "SM4"
    ),
    x = c(
      662, 701, 346, 370, 439, 444, 548, 581, 747, 704,
      740, 448, 539, 345, 375, 275
    ),
    y = c(
      35, 35, 132, 133, 122, 200, 171, 219, 374, 408,
      416, 438, 437, 455, 454, 441
    )
  ),
  data.frame(
    project = "E11RI", image_width = 790, image_height = 508,
    cds_name = c(
      "MF1", "MF2", "FCM6", "FCM5", "RA2", "FCM7", "RA6",
      "FCM4", "UPe2", "MBe2", "RA5", "FCM3", "FCM2", "FCM1",
      "MBe1", "UPe1", "RA4", "RA7", "UPe3", "MB4", "MB3",
      "FCM9", "FTCe1", "UPe4", "FCM8", "FMEe3", "RA3", "RA1",
      "FMEe1", "MB1", "MB2", "FMEe2", "EML", "REG"
    ),
    x = c(
      60, 168, 402, 461, 265, 310, 444, 554, 499, 498,
      498, 554, 461, 402, 436, 453, 423, 365, 405, 226,
      246, 300, 323, 365, 310, 282, 199, 177, 60, 99,
      139, 166, 719, 749
    ),
    y = c(
      36, 36, 46, 46, 99, 129, 100, 139, 137, 155,
      194, 217, 290, 290, 214, 239, 224, 130, 105, 163,
      183, 158, 178, 178, 217, 142, 213, 250, 222, 222,
      222, 214, 349, 378
    )
  ),
  data.frame(
    project = "F0400", image_width = 932, image_height = 678,
    cds_name = c(
      "EML", "REG", "MB6", "MB5", "MB4", "MB3", "MB2", "SM3",
      "SM2", "R3", "PE1", "FCM1", "FTC1", "MB1", "SM1"
    ),
    x = c(
      302, 263, 412, 234, 234, 595, 595, 223, 598, 576,
      233, 180, 535, 502, 602
    ),
    y = c(
      97, 97, 173, 195, 216, 194, 212, 278, 278, 381,
      386, 419, 435, 484, 462
    )
  )
) |>
  mutate(
    cds_key = str_to_upper(str_squish(cds_name)),
    x_pct = 100 * x / image_width,
    y_pct = 100 * y / image_height
  )


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

      function aggiornaLayoutHome() {
        var stage = document.querySelector('.home-stage');
        var frame = stage ? stage.querySelector('.schema-frame') : null;
        var img = frame ? frame.querySelector('img') : null;

        if (!stage || !frame || !img || window.innerWidth <= 700) {
          return;
        }

        if (img.naturalWidth > 0 && img.naturalHeight > 0) {
          var rapporto = img.naturalWidth / img.naturalHeight;
          var stageWidth = stage.clientWidth;
          var stageHeight = stage.clientHeight;

          var maxWidth = Math.min(stageWidth * 0.84, 1500);
          var maxHeight = stageHeight * 0.86;
          var targetWidth = Math.min(maxWidth, maxHeight * rapporto);

          frame.style.width = Math.max(560, targetWidth) + 'px';
        }
      }

      function pianificaLayoutHome() {
        window.requestAnimationFrame(function () {
          setTimeout(aggiornaLayoutHome, 40);
        });
      }

      $(document).on('shown.bs.modal', aggiornaLarghezzaModal);
      $(window).on('resize', function () {
        aggiornaLarghezzaModal();
        pianificaLayoutHome();
      });

      $(document).ready(function () {
        pianificaLayoutHome();

        var schemaOutput = document.getElementById('schema_sensori');
        if (schemaOutput && window.MutationObserver) {
          var observer = new MutationObserver(function () {
            pianificaLayoutHome();
          });
          observer.observe(schemaOutput, {childList: true, subtree: true});
        }
      });

      document.addEventListener('load', function (event) {
        if (
          event.target &&
          event.target.matches &&
          event.target.matches('.schema-frame img')
        ) {
          pianificaLayoutHome();
        }
      }, true);

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

      #nok_table table,
      #modal_nok_table table {
        width: 100%;
        border-collapse: collapse;
      }
      #nok_table th,
      #modal_nok_table th {
        background-color: #E9EDF1;
        color: #4A4A4A;
        font-weight: 600;
        text-align: center !important;
        padding: 8px 10px;
        border-bottom: 2px solid #D5DBE0;
      }
      #nok_table td,
      #modal_nok_table td {
        text-align: center !important;
        padding: 10px 12px;
        border-bottom: 1px solid #E4E7EB;
        color: #234A66;
        font-weight: 700;
        font-size: 18px;
      }
      .pannello-filtri {
        background-color: #F4F6F8;
        border-radius: 6px;
        padding: 8px 14px 0 14px;
        margin-bottom: 8px;
      }
      .pannello-filtri .form-group {
        margin-bottom: 8px;
      }
      .pannello-filtri label {
        margin-bottom: 3px;
        font-size: 12px;
      }
      .pannello-filtri .form-control {
        height: 32px;
        padding: 4px 8px;
        font-size: 13px;
      }
      .pannello-filtri .input-group-addon {
        padding: 4px 8px;
        font-size: 12px;
      }
      .titolo-sezione {
        color: #4A4A4A;
        font-weight: 600;
        margin-top: 10px;
        margin-bottom: 10px;
      }
      .activation-scroll {
        width: 100%;
        overflow-x: auto;
        overflow-y: hidden;
        padding-bottom: 8px;
      }
      .activation-scroll-inner {
        display: block;
      }
      .documenti-bar {
        display: flex;
        align-items: center;
        gap: 14px;
        flex-wrap: wrap;
        margin: 0 0 10px 0;
        padding: 9px 13px;
        border: 1px solid #D9DDE0;
        border-radius: 7px;
        background: #F7F7F7;
      }
      .documenti-title {
        display: flex;
        align-items: center;
        gap: 7px;
        color: #234A66;
        font-size: 13px;
        font-weight: 700;
      }
      .documenti-links {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-wrap: wrap;
      }
      .documento-download {
        display: inline-block;
        padding: 5px 9px;
        border: 1px solid #CDD2D6;
        border-radius: 6px;
        background: #FFFFFF;
        color: #234A66 !important;
        font-size: 12px;
        font-weight: 600;
        text-decoration: none !important;
      }
      .documento-download:hover,
      .documento-download:focus {
        background: #F1F2F3;
        color: #234A66 !important;
      }
      .modal-documenti {
        padding-top: 2px;
      }
      .modal-documenti-title {
        display: flex;
        align-items: center;
        gap: 7px;
        margin-bottom: 6px;
        color: #234A66;
        font-size: 12px;
        font-weight: 700;
      }
      .modal-documenti .documenti-links {
        display: flex;
        align-items: flex-start;
        gap: 6px;
        flex-wrap: wrap;
      }
      .card-home {
        display: block;
        width: 100%;
        min-height: 104px;
        background: #FFFFFF;
        border: 1px solid #E4E7EB;
        border-radius: 9px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
        transition: transform 0.15s ease, box-shadow 0.15s ease, border-color 0.15s ease;
        text-align: left;
        white-space: normal;
        padding: 12px 13px;
        margin-bottom: 10px;
        color: #4A4A4A;
      }
      .card-home:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 18px rgba(0,0,0,0.10);
        border-color: #5F748C;
      }
      .card-home .card-icona {
        font-size: 20px;
        color: #5F748C;
        margin-bottom: 4px;
      }
      .card-home h4 {
        font-size: 15px;
        font-weight: 700;
        margin: 0 0 4px 0;
      }
      .card-home p {
        font-size: 11.5px;
        line-height: 1.3;
        color: #8A94A0;
        margin: 0;
      }
      .schema-home {
        width: 100%;
        display: flex;
        justify-content: center;
        margin: 0;
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
        border: 2px solid #8F9AA5;
        border-radius: 7px;
        background: #FFFFFF;
        box-shadow: 0 1px 4px rgba(36,54,75,0.12);
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
        border-color: #234A66;
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
        background: #234A66;
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
        border-color: #234A66 transparent transparent transparent;
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
      .home-stage {
        position: relative;
        width: 100%;
        height: clamp(600px, calc(100vh - 190px), 760px);
        min-height: 600px;
        margin: 0 0 12px 0;
        overflow: hidden;
        background: #F4F5F6;
      }

      /* Schema grande, centrato e sopra ai quattro pulsanti. */
      .home-schema-layer {
        position: absolute;
        inset: 0;
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: 5;
        pointer-events: none;
      }
      .home-schema-layer .schema-home {
        width: 100%;
        margin: 0;
        display: flex;
        justify-content: center;
        pointer-events: none;
      }
      .home-schema-layer .schema-frame {
        width: min(84vw, 1500px);
        max-width: calc(100% - 180px);
        pointer-events: auto;
      }

      /* Quattro pulsanti rettangolari: ognuno occupa un quarto della pagina. */
      .home-corner {
        position: absolute;
        z-index: 1;
        width: 50%;
        height: 50%;
      }
      .home-corner-tl {
        top: 0;
        left: 0;
      }
      .home-corner-tr {
        top: 0;
        right: 0;
      }
      .home-corner-bl {
        bottom: 0;
        left: 0;
      }
      .home-corner-br {
        right: 0;
        bottom: 0;
      }

      .home-corner .card-home {
        position: relative;
        width: 100%;
        height: 100%;
        min-height: 0;
        margin: 0;
        padding: 0;
        border: 0;
        border-radius: 0;
        background: #F4F5F6;
        box-shadow: none;
        text-align: left;
        transition: background-color 0.15s ease;
      }
      .home-corner .card-home:hover {
        transform: none;
        background: #E7E0C8;
        border: 0;
        box-shadow: inset 0 0 0 2px rgba(161, 148, 103, 0.12);
      }
      .home-corner-tl .card-home {
        border-right: 2px solid #CDD2D6;
        border-bottom: 2px solid #CDD2D6;
      }
      .home-corner-tr .card-home {
        border-bottom: 2px solid #CDD2D6;
      }
      .home-corner-bl .card-home {
        border-right: 2px solid #CDD2D6;
      }

      /* Descrizione visibile nell'angolo esterno del relativo rettangolo. */
      .home-corner .card-home .home-card-label {
        position: absolute;
        width: clamp(180px, 12vw, 215px);
        min-height: 86px;
        padding: 13px 15px;
        border: 0;
        border-radius: 9px;
        background: #FFFFFF;
        box-shadow: none;
        z-index: 2;
      }
      .home-corner .card-home:hover .home-card-label {
        background: #F3F4F5;
        box-shadow: none;
      }
      .home-corner .card-home h4 {
        font-size: 17px;
        font-weight: 700;
        margin: 0 0 5px 0;
        color: #234A66;
      }
      .home-corner .card-home p {
        font-size: 12.5px;
        line-height: 1.35;
        margin: 0;
        color: #68737E;
      }
      .home-corner-tl .home-card-label {
        top: 12px;
        left: 12px;
      }
      .home-corner-tr .home-card-label {
        top: 12px;
        right: 12px;
      }
      .home-corner-bl .home-card-label {
        bottom: 12px;
        left: 12px;
      }
      .home-corner-br .home-card-label {
        right: 12px;
        bottom: 12px;
      }

      .alarm-section {
        margin: 0 0 28px 0;
        padding: 0 0 24px 0;
        border-bottom: 1px solid #E4E7EB;
      }
      .alarm-section:last-child {
        border-bottom: 0;
        margin-bottom: 0;
        padding-bottom: 0;
      }
      .alarm-empty {
        margin-top: 10px;
        padding: 12px 14px;
        border: 1px solid #DDE4EA;
        border-radius: 7px;
        background: #F8FAFB;
        color: #5F6F7F;
        font-size: 13px;
      }
      .report-panel {
        margin: 0 0 28px 0;
        padding: 18px 20px;
        border: 1px solid #D9DDE0;
        border-radius: 12px;
        background: linear-gradient(135deg, #FBFAF6 0%, #F1F3F4 100%);
        box-shadow: 0 3px 12px rgba(36,54,75,0.06);
      }
      .report-buttons {
        display: flex;
        flex-wrap: wrap;
        gap: 9px;
        margin-top: 10px;
      }
      .report-period-btn {
        border: 1px solid #CDD2D6;
        border-radius: 999px;
        background: #FFFFFF;
        color: #234A66;
        font-size: 12px;
        font-weight: 700;
        padding: 8px 13px;
        cursor: pointer;
        box-shadow: 0 1px 3px rgba(36,54,75,0.05);
        transition: background 0.15s ease, transform 0.15s ease;
      }
      .report-period-btn:hover {
        background: #F1F2F3;
        transform: translateY(-1px);
      }
      .report-modal {
        color: #334155;
        padding-bottom: 18px;
        font-family: 'Segoe UI', 'Inter', 'Helvetica Neue', Arial, sans-serif;
        letter-spacing: -0.005em;
      }
      .report-hero {
        margin: -15px -15px 22px -15px;
        padding: 25px 28px 24px 28px;
        background: linear-gradient(135deg, #234A66 0%, #354C65 100%);
        color: #FFFFFF;
        border-radius: 8px 8px 14px 14px;
        box-shadow: 0 7px 20px rgba(36,54,75,0.16);
      }
      .report-modal-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 22px;
      }
      .report-heading-left {
        display: flex;
        align-items: flex-start;
        gap: 13px;
        min-width: 0;
      }
      .report-back-btn.btn {
        flex-shrink: 0;
        width: 38px;
        height: 38px;
        margin-top: 1px;
        padding: 0 !important;
        border: 1px solid rgba(255,255,255,0.30) !important;
        border-radius: 50% !important;
        background: rgba(255,255,255,0.08) !important;
        color: #FFFFFF !important;
        font-size: 16px;
        box-shadow: none !important;
      }
      .report-back-btn.btn:hover,
      .report-back-btn.btn:focus {
        background: rgba(255,255,255,0.18) !important;
        color: #FFFFFF !important;
        outline: none !important;
      }
      .report-title {
        margin: 0;
        color: #FFFFFF;
        font-size: 32px;
        line-height: 1.12;
        font-weight: 780;
        letter-spacing: -0.025em;
      }
      .report-subtitle {
        margin-top: 10px;
        color: #E6EDF3;
        font-size: 15px;
        line-height: 1.42;
      }
      .report-download.btn {
        flex-shrink: 0;
        margin-top: 2px;
        padding: 9px 14px;
        border: 1px solid rgba(255,255,255,0.36) !important;
        border-radius: 8px !important;
        background: rgba(255,255,255,0.10) !important;
        color: #FFFFFF !important;
        font-weight: 700;
      }
      .report-download.btn:hover {
        background: rgba(255,255,255,0.18) !important;
      }
      .report-meta {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        margin-top: 15px;
      }
      .report-meta-chip {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 6px 10px;
        border: 1px solid rgba(255,255,255,0.18);
        border-radius: 999px;
        background: rgba(255,255,255,0.07);
        color: #EEF3F7;
        font-size: 12px;
        font-weight: 650;
      }
      .report-kpi-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0,1fr));
        gap: 11px;
        margin: 0 0 22px 0;
      }
      .report-kpi-card {
        min-width: 0;
        padding: 14px 15px;
        border: 1px solid #E0E5EA;
        border-radius: 10px;
        background: #FFFFFF;
        box-shadow: 0 2px 8px rgba(36,54,75,0.05);
      }
      .report-kpi-label {
        color: #718096;
        font-size: 10px;
        font-weight: 800;
        letter-spacing: 0.07em;
        text-transform: uppercase;
      }
      .report-kpi-value {
        margin-top: 5px;
        color: #234A66;
        font-size: 23px;
        line-height: 1.12;
        font-weight: 800;
      }
      .report-kpi-note {
        margin-top: 4px;
        color: #718096;
        font-size: 11px;
        line-height: 1.35;
      }
      .report-summary {
        margin: 0 0 24px 0;
        padding: 20px 22px;
        border: 1px solid #D9DDE0;
        border-left: 5px solid #EBBD55;
        border-radius: 10px;
        background: #F7F7F7;
        color: #334155;
        line-height: 1.58;
        font-size: 15px;
      }
      .report-summary-title {
        margin: 0 0 12px 0;
        color: #234A66;
        font-size: 20px;
        font-weight: 800;
        letter-spacing: -0.015em;
      }
      .report-summary p {
        margin: 0 0 12px 0;
      }
      .report-summary p:last-child {
        margin-bottom: 0;
      }
      .report-section {
        margin: 0 0 28px 0;
      }
      .report-section-heading {
        display: flex;
        align-items: center;
        gap: 9px;
        margin: 0 0 10px 0;
        padding-bottom: 8px;
        border-bottom: 1px solid #E4E7EB;
      }
      .report-section-number {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 24px;
        height: 24px;
        border-radius: 50%;
        background: #234A66;
        color: #FFFFFF;
        font-size: 11px;
        font-weight: 800;
      }
      .report-section-title {
        margin: 0;
        color: #234A66;
        font-size: 16px;
        font-weight: 800;
      }
      .report-section-note {
        margin: -3px 0 10px 33px;
        color: #718096;
        font-size: 11px;
      }
      .report-table-wrap {
        width: 100%;
        overflow-x: auto;
        border: 1px solid #E0E5EA;
        border-radius: 9px;
        background: #FFFFFF;
      }
      .report-table {
        width: 100%;
        border-collapse: collapse;
        margin: 0;
        font-size: 12px;
      }
      .report-table th {
        background: #EDF1F4;
        color: #234A66;
        border-bottom: 1px solid #D8E0E6;
        border-right: 1px solid #E1E6EA;
        padding: 9px 10px;
        text-align: center;
        white-space: nowrap;
      }
      .report-table td {
        border-bottom: 1px solid #EEF1F4;
        border-right: 1px solid #EEF1F4;
        padding: 9px 10px;
        text-align: center;
        vertical-align: top;
        white-space: nowrap;
      }
      .report-table tbody tr:nth-child(even) {
        background: #FAFBFC;
      }
      .report-table th:last-child,
      .report-table td:last-child {
        border-right: 0;
      }
      .report-ok {
        color: #19764A;
        font-weight: 800;
      }
      .report-bad {
        color: #B42318;
        font-weight: 800;
      }
      .report-nd {
        color: #718096;
        font-weight: 700;
      }
      .report-status-pill {
        display: inline-block;
        margin-left: 5px;
        padding: 2px 7px;
        border-radius: 999px;
        font-size: 9px;
        font-weight: 800;
        text-transform: uppercase;
      }
      .report-status-ok {
        color: #19764A;
        background: #DDF3E6;
      }
      .report-status-bad {
        color: #A93B32;
        background: #F7D9D5;
      }
      @media (max-width: 900px) {
        .report-kpi-grid {
          grid-template-columns: repeat(2, minmax(0,1fr));
        }
        .report-modal-header {
          display: block;
        }
        .report-download.btn {
          margin-top: 14px;
        }
      }
      .modal-filters {
        background: #F4F6F8;
        border-radius: 7px;
        padding: 14px 16px 4px 16px;
        margin-bottom: 18px;
      }
      .modal-data-button {
        margin: 4px 0 10px 0;
        text-align: right;
      }
      .modal-data-panel {
        margin: 0 0 22px 0;
        padding: 12px;
        border: 1px solid #E4E7EB;
        border-radius: 7px;
        background: #FFFFFF;
      }
      .modal-machine-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 16px;
        margin: 0 0 12px 0;
        padding: 2px 0 10px 0;
      }
      .modal-machine-title {
        color: #234A66;
        font-size: 18px;
        font-weight: 700;
        line-height: 1.25;
      }
      .modal-close-x {
        flex-shrink: 0;
        border: 0;
        background: transparent;
        color: #4A4A4A;
        font-size: 31px;
        font-weight: 400;
        line-height: 1;
        padding: 0 5px;
        cursor: pointer;
        opacity: 0.72;
      }
      .modal-close-x:hover,
      .modal-close-x:focus {
        color: #234A66;
        opacity: 1;
        outline: none;
      }
      .modal-nav-bar {
        margin-bottom: 20px;
        padding-bottom: 14px;
        border-bottom: 2px solid #E4E7EB;
      }
      .modal-nav-bar .btn-group .btn {
        font-weight: 700;
        font-size: 14px;
        padding: 9px 19px;
        color: #234A66 !important;
        background: #FFFFFF !important;
        border-color: #D6DADF !important;
        box-shadow: none !important;
      }
      .modal-nav-bar .btn-group .btn:hover,
      .modal-nav-bar .btn-group .btn:focus {
        color: #234A66 !important;
        background: #F3F4F5 !important;
        border-color: #EBBD55 !important;
      }
      .modal-nav-bar .btn-group .btn.active,
      .modal-nav-bar .btn-group .btn.active:hover,
      .modal-nav-bar .btn-group .btn.active:focus {
        color: #234A66 !important;
        background: #EBBD55 !important;
        border-color: #D4A23B !important;
        box-shadow: inset 0 1px 3px rgba(36,54,75,0.12) !important;
      }

      /* ------------------------------------------------------------------
         Vita sensori - card con valore corrente, massimo, utilizzo e residuo
         ------------------------------------------------------------------ */
      .life-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 18px;
        margin: 4px 0 18px 0;
      }
      .life-header-title {
        margin: 0;
        color: #234A66;
        font-size: 23px;
        font-weight: 700;
      }
      .life-header-subtitle {
        margin-top: 4px;
        color: #718096;
        font-size: 14px;
      }
      .life-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 16px;
        width: 100%;
      }
      .life-card {
        min-width: 0;
        background: #FFFFFF;
        border: 1px solid #DDE4EA;
        border-radius: 10px;
        padding: 15px 17px 14px 17px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.045);
      }
      .life-card-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 10px;
        padding-bottom: 10px;
        margin-bottom: 13px;
        border-bottom: 1px solid #E6EBEF;
      }
      .life-card-title {
        min-width: 0;
        color: #234A66;
        font-size: 14px;
        line-height: 1.25;
        font-weight: 700;
      }
      .life-status {
        flex-shrink: 0;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        border-radius: 999px;
        padding: 4px 10px;
        font-size: 11px;
        font-weight: 700;
      }
      .life-status-ok {
        color: #19764A;
        background: #DDF3E6;
      }
      .life-status-warning {
        color: #996400;
        background: #FFF0C8;
      }
      .life-status-over {
        color: #A93B32;
        background: #F7D9D5;
      }
      .life-row {
        margin-bottom: 16px;
      }
      .life-row:last-child {
        margin-bottom: 1px;
      }
      .life-row + .life-row {
        padding-top: 13px;
        border-top: 1px solid #EEF1F4;
      }
      .life-row-header {
        display: flex;
        justify-content: space-between;
        align-items: baseline;
        gap: 10px;
        margin-bottom: 7px;
      }
      .life-metric {
        flex-shrink: 0;
        color: #234A66;
        font-size: 13px;
        font-weight: 700;
      }
      .life-value {
        color: #234A66;
        font-size: 12px;
        font-weight: 600;
        text-align: right;
        white-space: nowrap;
      }
      .life-progress {
        position: relative;
        width: 100%;
        height: 14px;
        overflow: visible;
        background: #E4E9ED;
        border-radius: 999px;
      }
      .life-progress-fill {
        height: 100%;
        border-radius: 999px;
        background: #77B98D;
      }
      .life-progress-fill.warning {
        background: #E1B552;
      }
      .life-progress-fill.over {
        background: #D9897F;
      }
      .life-progress-marker {
        position: absolute;
        top: 50%;
        width: 10px;
        height: 10px;
        transform: translate(-50%, -50%);
        border-radius: 50%;
        background: #259762;
        box-shadow: 0 0 0 2px #FFFFFF;
      }
      .life-progress-marker.warning {
        background: #C58A17;
      }
      .life-progress-marker.over {
        background: #BB4F45;
      }
      .life-row-footer {
        display: flex;
        justify-content: space-between;
        gap: 10px;
        margin-top: 6px;
        color: #718096;
        font-size: 11px;
      }
      .life-row-footer strong {
        color: #25865B;
      }
      .life-row-footer strong.warning {
        color: #A16C08;
      }
      .life-row-footer strong.over {
        color: #A93B32;
      }
      @media (max-width: 1250px) {
        .life-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }
      @media (max-width: 760px) {
        .life-grid {
          grid-template-columns: 1fr;
        }
        .life-header {
          display: block;
        }
        .life-value {
          white-space: normal;
        }
      }

      @media (max-width: 1050px) {
        .home-stage {
          height: 620px;
          min-height: 620px;
        }
        .home-schema-layer .schema-frame {
          max-width: calc(100% - 170px);
        }
        .home-corner .card-home .home-card-label {
          width: 132px;
          min-height: 70px;
          padding: 9px 10px;
        }
      }

      @media (max-width: 700px) {
        .home-menu {
          padding-right: 15px;
        }
        .home-stage {
          display: grid;
          grid-template-columns: 1fr;
          gap: 12px;
          height: auto;
          min-height: 0;
          overflow: visible;
        }
        .home-schema-layer,
        .home-corner {
          position: static;
          width: 100%;
          height: auto;
        }
        .home-schema-layer {
          order: 1;
          pointer-events: auto;
        }
        .home-schema-layer .schema-home {
          pointer-events: auto;
        }
        .home-schema-layer .schema-frame {
          width: 100% !important;
          max-width: 100%;
        }
        .home-corner-tl {
          order: 2;
        }
        .home-corner-tr {
          order: 3;
        }
        .home-corner-bl {
          order: 4;
        }
        .home-corner-br {
          order: 5;
        }
        .home-corner .card-home {
          width: 100%;
          height: auto;
          min-height: 82px;
          padding: 10px 12px;
          border-radius: 9px;
        }
        .home-corner .card-home .home-card-label {
          position: static;
          width: 100%;
          min-height: 0;
          padding: 0;
          border: 0;
          border-radius: 0;
          background: transparent;
          box-shadow: none;
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

      /* ==============================================================
         TEMA SEMPREANORMA
         Layout invariato: si interviene solo su colori, tipografia,
         bordi, hover e gerarchia visiva.
         ============================================================== */
      :root {
        --san-blue: #234A66;
        --san-blue-2: #5F748C;
        --san-yellow: #EBBD55;
        --san-yellow-dark: #D4A23B;
        --san-bg: #F4F5F6;
        --san-panel: #FFFFFF;
        --san-panel-soft: #F0F2F3;
        --san-border: #C5CDD3;
        --san-border-soft: #DDE1E4;
        --san-text: #202326;
        --san-muted: #66717A;
      }

      body,
      .form-control,
      .btn,
      .dropdown-menu,
      .modal-content,
      .selectize-input,
      .selectize-dropdown {
        font-family: Arial, 'Helvetica Neue', Helvetica, sans-serif;
      }

      body {
        color: var(--san-text);
        background: #FFFFFF;
      }

      /* Fascia alta: il blu resta concentrato qui, come nel sito. */
      .container-fluid > h2:first-of-type {
        margin: 0 -15px;
        padding: 13px 20px 12px 20px;
        background: var(--san-blue);
        border-bottom: 4px solid var(--san-yellow);
        color: #FFFFFF;
        font-size: 29px;
        font-weight: 500;
        letter-spacing: -0.02em;
      }

      .pannello-filtri {
        margin: 0 -7px 8px -7px;
        padding: 10px 14px 1px 14px;
        background: var(--san-panel-soft);
        border: 1px solid var(--san-border-soft);
        border-top: 0;
        border-radius: 0 0 7px 7px;
        box-shadow: none;
      }
      .pannello-filtri label,
      .modal-filters label {
        color: var(--san-blue);
        font-weight: 700;
      }
      .pannello-filtri .form-control,
      .modal-filters .form-control,
      .bootstrap-select > .dropdown-toggle {
        background: #FFFFFF;
        border-color: #C8CFD4;
        color: var(--san-text);
        box-shadow: none;
      }
      .pannello-filtri .form-control:focus,
      .modal-filters .form-control:focus,
      .bootstrap-select.open > .dropdown-toggle {
        border-color: var(--san-blue-2);
        box-shadow: 0 0 0 2px rgba(95,116,140,0.10);
      }

      input[type='radio'],
      input[type='checkbox'] {
        accent-color: var(--san-yellow-dark);
      }

      /* Home: grigio/bianco al centro, giallo solo come richiamo. */
      .home-stage {
        background: var(--san-bg);
        border: 1px solid var(--san-border-soft);
      }
      .home-corner .card-home {
        background: #F7F8F8;
        border: 0 !important;
        box-shadow: inset 0 0 0 1px #AEB8C1;
        transition:
          background-color 0.16s ease,
          box-shadow 0.16s ease;
      }
      .home-corner .card-home:hover,
      .home-corner .card-home:focus {
        background: #FFFFFF;
        box-shadow:
          inset 0 0 0 2px var(--san-yellow),
          inset 0 4px 0 var(--san-yellow);
        outline: none;
      }

      .home-corner .card-home .home-card-label {
        background: #FFFFFF;
        border: 1px solid #D4D9DD;
        border-left: 4px solid var(--san-yellow);
        border-radius: 6px;
        box-shadow: 0 2px 8px rgba(35,74,102,0.06);
      }
      .home-corner .card-home:hover .home-card-label,
      .home-corner .card-home:focus .home-card-label {
        background: #FFFFFF;
        border-color: #D4D9DD;
        border-left-color: var(--san-yellow);
        box-shadow: 0 4px 12px rgba(35,74,102,0.10);
      }
      .home-corner .card-home h4 {
        color: var(--san-blue);
        font-weight: 700;
      }
      .home-corner .card-home p {
        color: var(--san-muted);
      }

      .schema-frame img {
        border: 1.5px solid var(--san-blue-2);
        border-radius: 6px;
        background: #FFFFFF;
        box-shadow: 0 4px 14px rgba(35,74,102,0.10);
      }

      /* Hotspot: blu in hover, bordo giallo come accento. */
      .sensor-hotspot:hover,
      .sensor-hotspot:focus {
        background: rgba(95,116,140,0.26);
        border-color: var(--san-yellow);
      }
      .sensor-tooltip {
        background: var(--san-blue);
        border-left: 3px solid var(--san-yellow);
      }
      .sensor-tooltip::after {
        border-color: var(--san-blue) transparent transparent transparent;
      }

      /* Modali: testata blu, contenuto bianco/grigio, selezione gialla. */
      .modal-content {
        border: 1px solid #C7CDD2;
        border-radius: 8px;
        box-shadow: 0 12px 38px rgba(29,45,58,0.18);
      }
      .modal-machine-header {
        margin: -15px -15px 14px -15px;
        padding: 14px 18px;
        background: var(--san-blue);
        border-bottom: 3px solid var(--san-yellow);
      }
      .modal-machine-title {
        color: #FFFFFF;
        font-weight: 600;
      }
      .modal-close-x,
      .modal-close-x:hover,
      .modal-close-x:focus {
        color: #FFFFFF;
      }

      .modal-filters {
        background: #F1F3F4;
        border: 1px solid #DDE1E4;
        border-radius: 6px;
      }
      .modal-nav-bar {
        border-bottom-color: #D8DDE1;
      }
      .modal-nav-bar .btn-group .btn {
        color: var(--san-blue) !important;
        background: #FFFFFF !important;
        border-color: #CDD3D8 !important;
        font-weight: 600;
      }
      .modal-nav-bar .btn-group .btn:hover,
      .modal-nav-bar .btn-group .btn:focus {
        color: var(--san-blue) !important;
        background: #F2F4F5 !important;
        border-color: var(--san-yellow) !important;
      }
      .modal-nav-bar .btn-group .btn.active,
      .modal-nav-bar .btn-group .btn.active:hover,
      .modal-nav-bar .btn-group .btn.active:focus {
        color: #202326 !important;
        background: var(--san-yellow) !important;
        border-color: var(--san-yellow-dark) !important;
        box-shadow: none !important;
      }

      .titolo-sezione,
      .life-header-title,
      .life-card-title,
      .life-metric,
      .modal-machine-title,
      .documenti-title,
      .modal-documenti-title {
        color: var(--san-blue);
      }
      .modal-machine-title {
        color: #FFFFFF;
      }

      /* Card, report e tabelle: bianchi con bordi grigi puliti. */
      .life-card,
      .modal-data-panel,
      .report-table-wrap,
      .report-panel,
      .documenti-bar {
        background: #FFFFFF;
        border-color: #D5DADF;
        box-shadow: 0 2px 8px rgba(35,74,102,0.045);
      }

      #nok_table th,
      #modal_nok_table th,
      .report-table th {
        background: var(--san-blue);
        color: #FFFFFF;
        border-bottom-color: var(--san-yellow);
      }
      #nok_table td,
      #modal_nok_table td,
      .report-table td {
        color: var(--san-text);
        border-bottom-color: #E3E6E8;
      }
      .report-table tbody tr:nth-child(even) {
        background: #F6F7F8;
      }

      .documento-download,
      .modal-data-button .btn {
        background: #FFFFFF;
        color: var(--san-blue) !important;
        border-color: #C7CED3;
      }
      .documento-download:hover,
      .documento-download:focus,
      .modal-data-button .btn:hover,
      .modal-data-button .btn:focus {
        background: #F7F8F8;
        color: var(--san-blue) !important;
        border-color: var(--san-yellow);
      }

      .report-summary {
        background: #F7F8F8;
        border-color: #D8DDE0;
        border-left-color: var(--san-yellow);
      }
      .report-section-number {
        background: var(--san-blue);
      }

      /* Piccoli elementi di evidenza: giallo, mai grandi campiture. */
      .report-period-btn:hover,
      .report-period-btn:focus {
        border-color: var(--san-yellow) !important;
      }
      .report-period-btn.active {
        background: var(--san-yellow) !important;
        border-color: var(--san-yellow-dark) !important;
        color: #202326 !important;
      }
    "))
  ),
  
  titlePanel("Dashboard 5SAN"),
  
  # Filtri in orizzontale, a piena larghezza, sempre visibili.
  # In modalita' web la macchina arriva dalla querystring e i filtri
  # Azienda/Stabilimento/Macchina non vengono mostrati.
  div(
    class = "pannello-filtri",
    fluidRow(
      if (!IS_WEB_MODE) {
        tagList(
          column(2, selectInput("azienda", "Azienda:", choices = company)),
          column(2, selectInput("stabilimento", "Stabilimento:", choices = NULL)),
          column(3, selectInput("macchina", "Macchina:", choices = NULL))
        )
      },
      column(
        width = if (IS_WEB_MODE) 4 else 3,
        dateRangeInput(
          "date",
          "Periodo:",
          start = data_start_default,
          end = data_max,
          min = data_min,
          max = data_max,
          format = "dd-mm-yyyy",
          separator = " a ",
          language = "it"
        )
      )
    )
  ),
  
  # Home unica: menu verticale a sinistra e schema della macchina a destra.
  div(
    class = "home-stage",
    
    # Se non esiste un'immagine associata al progetto selezionato,
    # renderUI restituisce NULL e la colonna destra resta vuota.
    div(
      class = "home-schema-layer",
      uiOutput("schema_sensori")
    ),
    
    div(
      class = "home-corner home-corner-tl",
      actionButton(
        "home_attivazioni",
        label = div(
          class = "home-card-label",
          h4("Conteggio attivazioni"),
          p("Grafico a barre e trend nel tempo per sensore")
        ),
        class = "card-home"
      )
    ),
    
    div(
      class = "home-corner home-corner-tr",
      actionButton(
        "home_nok",
        label = div(
          class = "home-card-label",
          h4("NOK"),
          p("KPI e andamento storico del NOK per sensore")
        ),
        class = "card-home"
      )
    ),
    
    div(
      class = "home-corner home-corner-bl",
      actionButton(
        "home_vita",
        label = div(
          class = "home-card-label",
          h4("Vita sensori"),
          p("Stato dei sensori rispetto alle soglie B10dSAN e T10d")
        ),
        class = "card-home"
      )
    ),
    
    div(
      class = "home-corner home-corner-br",
      actionButton(
        "home_allarmi",
        label = div(
          class = "home-card-label",
          h4("Allarmi e Near Miss"),
          p("Allarmi e segnalazioni Near Miss")
        ),
        class = "card-home"
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Parametri della querystring, usati soltanto in modalita' web.
  web_query <- reactive({
    search <- session$clientData$url_search
    if (is.null(search) || !nzchar(search)) {
      return(list())
    }
    parseQueryString(search)
  })
  
  # Sorgente unica della macchina per tutta la dashboard.
  # Internal -> selectInput; Web -> ?coupon=...
  macchina_attiva <- reactive({
    if (!IS_WEB_MODE) {
      req(input$macchina)
      return(as.character(input$macchina))
    }
    
    query <- web_query()
    coupon <- query[["coupon"]]
    
    req(!is.null(coupon), length(coupon) >= 1)
    coupon <- trimws(as.character(coupon[[1]]))
    req(nzchar(coupon), coupon %in% macchine_lookup$coupon)
    
    coupon
  })
  
  # Download dei DOCX presenti in 00_Data/03_Documenti.
  if (length(documenti_files) > 0) {
    for (i in seq_along(documenti_files)) {
      local({
        file_corrente <- documenti_files[[i]]
        output_id <- paste0("download_documento_", i)
        
        output[[output_id]] <- downloadHandler(
          filename = function() {
            basename(file_corrente)
          },
          content = function(file) {
            file.copy(
              file_corrente,
              file,
              overwrite = TRUE
            )
          },
          contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
      })
    }
  }
  
  # I filtri a cascata esistono solo nella dashboard interna.
  if (!IS_WEB_MODE) {
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
    
    # 2. Quando cambia lo stabilimento, aggiorno le macchine disponibili
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
  }
  
  # 3. Quando cambia la macchina, aggiorno i sensori disponibili
  
  # Tabelle dati mostrate direttamente all'interno dei modal.
  mostra_dati_attivazioni <- reactiveVal(FALSE)
  mostra_dati_trend <- reactiveVal(FALSE)
  mostra_dati_tank <- reactiveVal(FALSE)
  mostra_dati_nok <- reactiveVal(FALSE)
  mostra_dati_profilo_nok <- reactiveVal(FALSE)
  mostra_storico_utilizzo_nok <- reactiveVal(FALSE)
  mostra_outlier_nok <- reactiveVal(FALSE)
  
  # I filtri vengono applicati solo dopo una breve pausa dall'ultima scelta.
  # In questo modo una selezione multipla genera un solo aggiornamento.
  ritardo_filtri_ms <- 800
  
  normalizza_sensori <- function(x) {
    if (is.null(x)) character(0) else sort(unique(as.character(x)))
  }
  
  sensori_modal_correnti <- reactiveVal(character(0))
  date_modal_corrente <- reactiveVal(NULL)
  modal_apertura_id <- reactiveVal(0)
  granularita_attivazioni_corrente <- reactiveVal("Giorno")
  granularita_nok_corrente <- reactiveVal("Giorno")
  granularita_utilizzo_corrente <- reactiveVal("Settimana")
  report_periodo_corrente <- reactiveVal(NULL)
  
  filtri_principali <- reactive({
    req(macchina_attiva(), input$date)
    
    list(
      macchina = macchina_attiva(),
      date = as.Date(input$date),
      sensori = sensori_lookup |>
        filter(coupon == macchina_attiva()) |>
        pull(cds_name) |>
        normalizza_sensori()
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
  apri_modal <- function(
    vista_iniziale,
    sensori_iniziali = NULL,
    date_iniziali = NULL
  ) {
    if (is.null(sensori_iniziali)) {
      sensori_iniziali <- sensori_lookup |>
        filter(coupon == isolate(macchina_attiva())) |>
        pull(cds_name)
    }
    
    if (is.null(date_iniziali)) {
      date_iniziali <- isolate(as.Date(input$date))
    }
    
    sensori_modal_correnti(normalizza_sensori(sensori_iniziali))
    date_modal_corrente(as.Date(date_iniziali))
    
    macchina_corrente <- macchine_lookup |>
      filter(coupon == isolate(macchina_attiva())) |>
      slice_head(n = 1)
    
    riferimento_macchina <- if (nrow(macchina_corrente) > 0) {
      paste(
        macchina_corrente$project,
        macchina_corrente$machine_name,
        sep = " – "
      )
    } else {
      as.character(isolate(macchina_attiva()))
    }
    
    granularita_attivazioni_corrente("Giorno")
    granularita_nok_corrente("Giorno")
    granularita_utilizzo_corrente("Settimana")
    
    modal_apertura_id(isolate(modal_apertura_id()) + 1)
    
    mostra_dati_attivazioni(FALSE)
    mostra_dati_trend(FALSE)
    mostra_dati_tank(FALSE)
    mostra_dati_nok(FALSE)
    mostra_dati_profilo_nok(FALSE)
    mostra_storico_utilizzo_nok(FALSE)
    
    showModal(modalDialog(
      title = NULL,
      size = "xl",
      easyClose = TRUE,
      footer = NULL,
      
      div(
        class = "modal-machine-header",
        div(
          paste0("Macchina: ", riferimento_macchina),
          class = "modal-machine-title"
        ),
        tags$button(
          type = "button",
          class = "modal-close-x",
          `data-dismiss` = "modal",
          `aria-label` = "Chiudi",
          HTML("&times;")
        )
      ),
      
      div(
        class = "modal-nav-bar",
        radioGroupButtons(
          "vista_selezionata",
          label = NULL,
          choices = c(
            "Conteggio attivazioni" = "attivazioni",
            "NOK"                   = "nok",
            "Vita sensori"          = "vita",
            "Allarmi e Near Miss"   = "allarmi"
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
    req(macchina_attiva(), input$date)
    apri_modal("attivazioni")
  })
  
  observeEvent(input$home_vita, {
    req(macchina_attiva())
    apri_modal("vita")
  })
  
  observeEvent(input$home_nok, {
    req(macchina_attiva(), input$date)
    apri_modal("nok")
  })
  
  observeEvent(input$home_allarmi, {
    req(macchina_attiva())
    apri_modal("allarmi")
  })
  
  # Click su un CdS nello schema: seleziona quel sensore e apre la tenda
  # direttamente sulla vista Conteggio attivazioni.
  observeEvent(input$schema_sensor_click, {
    req(macchina_attiva(), input$date, input$schema_sensor_click$sensor)
    
    sensore_cliccato <- input$schema_sensor_click$sensor
    sensori_validi <- sensori_lookup |>
      filter(coupon == macchina_attiva()) |>
      pull(cds_name)
    
    req(sensore_cliccato %in% sensori_validi)
    
    apri_modal("attivazioni", sensore_cliccato)
  })
  
  # Quando si cambia vista, azzera la visibilita' delle tabelle dati
  observeEvent(input$vista_selezionata, {
    mostra_dati_attivazioni(FALSE)
    mostra_dati_trend(FALSE)
    mostra_dati_tank(FALSE)
    mostra_dati_nok(FALSE)
    mostra_dati_profilo_nok(FALSE)
    mostra_storico_utilizzo_nok(FALSE)
    mostra_outlier_nok(FALSE)
    
  }, ignoreInit = TRUE)
  
  # Contenuto del modal: si aggiorna al cambio di vista senza chiudere il dialogo
  output$modal_contenuto <- renderUI({
    req(input$vista_selezionata, macchina_attiva())
    modal_apertura_id()
    
    vista <- input$vista_selezionata
    
    sensori_macchina <- sensori_lookup |>
      filter(coupon == macchina_attiva())
    
    sensori_selezionati <- intersect(
      isolate(sensori_modal_correnti()),
      sensori_macchina$cds_name
    )
    
    date_selezionate <- isolate(date_modal_corrente())
    req(length(date_selezionate) == 2, all(!is.na(date_selezionate)))
    
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
                     "modal_date", "Periodo:",
                     start = date_selezionate[1], end = date_selezionate[2],
                     min = data_min, max = data_max,
                     format = "dd-mm-yyyy", separator = " a ", language = "it"
                   )
            ),
            column(8,
                   pickerInput(
                     "modal_sensori", "Sensori:",
                     choices = scelte_sensori, selected = sensori_selezionati,
                     multiple = TRUE, options = picker_opts
                   )
            )
          ),
          radioButtons(
            "modal_granularita_attivazioni", "Raggruppamento:",
            choices = c("Ora", "Giorno", "Settimana", "Mese", "Trimestre", "Anno"),
            selected = isolate(granularita_attivazioni_corrente()), inline = TRUE
          )
        ),
        h4("Conteggio attivazioni", class = "titolo-sezione"),
        div(
          class = "activation-scroll",
          uiOutput("modal_activationPlot_container")
        ),
        div(
          class = "modal-data-button",
          actionButton("modal_btn_dati_attivazioni", "Dati", icon = icon("table"), class = "btn-sm btn-default")
        ),
        uiOutput("modal_panel_dati_attivazioni"),
        h4("Andamento per sensore", class = "titolo-sezione"),
        br(),
        div(
          class = "activation-scroll",
          uiOutput("modal_activationTrendPlot_container")
        ),
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
            "modal_sensori", "Sensori:",
            choices = scelte_sensori, selected = sensori_selezionati,
            multiple = TRUE, options = picker_opts
          )
        ),
        div(
          class = "life-header",
          div(
            h4("Vita sensori", class = "life-header-title"),
            div(
              "Consumo attuale rispetto ai limiti massimi B10dSAN e T10d",
              class = "life-header-subtitle"
            )
          )
        ),
        uiOutput("modal_tankCards"),
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
                     "modal_date", "Periodo:",
                     start = date_selezionate[1], end = date_selezionate[2],
                     min = data_min, max = data_max,
                     format = "dd-mm-yyyy", separator = " a ", language = "it"
                   )
            ),
            column(8,
                   pickerInput(
                     "modal_sensori", "Sensori:",
                     choices = scelte_sensori, selected = sensori_selezionati,
                     multiple = TRUE, options = picker_opts
                   )
            )
          )
        ),
        div(
          style = paste0(
            "display:flex;",
            "align-items:center;",
            "justify-content:space-between;",
            "gap:16px;",
            "margin-top:10px;"
          ),
          h4(
            "Profilo utilizzo macchina",
            class = "titolo-sezione",
            style = "margin:0;"
          ),
          actionButton(
            "modal_btn_outlier_nok",
            "Attivazioni anomale",
            class = "btn-sm btn-default"
          )
        ),
        uiOutput("modal_panel_outlier_nok"),
        div(
          style = paste0(
            "margin:14px 0 20px 0;",
            "padding:15px 18px;",
            "border:1px solid #DDE4EA;",
            "border-radius:9px;",
            "background:#FAFBFC;"
          ),
          div(
            style = paste0(
              "display:flex;",
              "align-items:center;",
              "gap:18px;",
              "margin-bottom:8px;",
              "flex-wrap:wrap;"
            ),
            div(
              "Utilizzo nel periodo",
              style = paste0(
                "font-size:16px;",
                "font-weight:700;",
                "color:#4A4A4A;"
              )
            ),
            div(
              style = "margin-bottom:-15px;",
              radioButtons(
                "modal_granularita_utilizzo",
                label = NULL,
                choices = c("Settimana", "Mese"),
                selected = isolate(
                  granularita_utilizzo_corrente()
                ),
                inline = TRUE
              )
            )
          ),
          uiOutput("modal_utilizzo_macchina")
        ),
        uiOutput("modal_panel_storico_utilizzo_nok"),
        girafeOutput("modal_utilizzoNokPlot", height = "380px"),
        div(
          class = "modal-data-button",
          actionButton(
            "modal_btn_dati_profilo_nok",
            "Dati",
            icon = icon("table"),
            class = "btn-sm btn-default"
          )
        ),
        uiOutput("modal_panel_dati_profilo_nok"),
        h4("Andamento NOK nel tempo", class = "titolo-sezione"),
        radioButtons(
          "modal_granularita_nok",
          "Raggruppamento:",
          choices = c("Giorno", "Settimana", "Mese", "Trimestre", "Anno"),
          selected = isolate(granularita_nok_corrente()),
          inline = TRUE
        ),
        girafeOutput("modal_utilizzoTempoPlot", height = "380px"),
        div(
          class = "modal-data-button",
          actionButton(
            "modal_btn_dati_nok",
            "Dati",
            icon = icon("table"),
            class = "btn-sm btn-default"
          )
        ),
        uiOutput("modal_panel_dati_nok")
      )
      
    } else if (vista == "allarmi") {
      tagList(
        div(
          class = "modal-filters",
          fluidRow(
            column(
              3,
              dateRangeInput(
                "modal_date", "Periodo:",
                start = date_selezionate[1], end = date_selezionate[2],
                min = data_min, max = data_max,
                format = "dd-mm-yyyy", separator = " a ", language = "it"
              )
            ),
            column(
              6,
              pickerInput(
                "modal_sensori", "Sensori:",
                choices = scelte_sensori, selected = sensori_selezionati,
                multiple = TRUE, options = picker_opts
              )
            ),
            column(
              3,
              if (length(documenti_files) > 0) {
                div(
                  class = "modal-documenti",
                  div(
                    class = "modal-documenti-title",
                    icon("file-word"),
                    span("Documenti")
                  ),
                  div(
                    class = "documenti-links",
                    lapply(seq_along(documenti_files), function(i) {
                      downloadLink(
                        outputId = paste0("download_documento_", i),
                        label = tools::file_path_sans_ext(
                          basename(documenti_files[[i]])
                        ),
                        class = "documento-download"
                      )
                    })
                  )
                )
              }
            )
          )
        ),
        div(
          class = "report-panel",
          h4("Report anomalie", class = "titolo-sezione"),
          radioButtons(
            "report_granularita",
            "Granularità report:",
            choices = c("Settimana", "Mese"),
            selected = "Settimana",
            inline = TRUE
          ),
          uiOutput("report_period_buttons")
        ),
        div(
          class = "alarm-section",
          h4("Attivazioni anomale", class = "titolo-sezione"),
          p(
            "Anomalie nei conteggi delle attivazioni: oltre alle anomalie giornaliere, viene segnalata ogni ora in cui il conteggio del sensore e' almeno 4 volte la sua mediana oraria."
          ),
          uiOutput("modal_allarmi_attivazioni_panel")
        ),
        div(
          class = "alarm-section",
          h4("Sensori aperti o senza contatto elettrico", class = "titolo-sezione"),
          p(
            "Eventi in cui il sensore è rimasto aperto per oltre 1 ora consecutiva mentre il gateway continuava a trasmettere."
          ),
          uiOutput("modal_allarmi_sensori_aperti_panel")
        ),
        div(
          class = "alarm-section",
          h4("Valori NOK anomali", class = "titolo-sezione"),
          p(
            "Giornate in cui il NOK giornaliero del sensore è fuori dai limiti media storica ± 3σ."
          ),
          uiOutput("modal_allarmi_nok_panel")
        )
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
  
  observeEvent(input$modal_btn_dati_profilo_nok, {
    mostra_dati_profilo_nok(!mostra_dati_profilo_nok())
  })
  
  observeEvent(input$modal_btn_storico_utilizzo_nok, {
    mostra_storico_utilizzo_nok(
      !mostra_storico_utilizzo_nok()
    )
  })
  
  observeEvent(input$modal_btn_outlier_nok, {
    mostra_outlier_nok(!mostra_outlier_nok())
  })
  
  # ---------------------------------------------------------------------
  # Coda ottimizzata dei filtri modal.
  # Il debounce aspetta 800 ms dall'ultima modifica. Data e sensori del
  # modal vengono mantenuti in reactiveVal dedicati: il modal aggiorna
  # eventualmente la Home, ma la Home non riscrive gli input del modal.
  # Questo elimina la sincronizzazione circolare che generava i loop.
  # ---------------------------------------------------------------------
  sensori_modal_input <- eventReactive(input$modal_sensori, {
    list(
      apertura_id = isolate(modal_apertura_id()),
      sensori = normalizza_sensori(input$modal_sensori)
    )
  }, ignoreNULL = TRUE) |>
    debounce(millis = ritardo_filtri_ms)
  
  observeEvent(sensori_modal_input(), {
    filtri <- sensori_modal_input()
    
    if (identical(filtri$apertura_id, isolate(modal_apertura_id()))) {
      sensori_modal_correnti(filtri$sensori)
    }
  }, ignoreInit = TRUE)
  
  periodo_modal_input <- eventReactive(input$modal_date, {
    list(
      apertura_id = isolate(modal_apertura_id()),
      date = as.Date(input$modal_date)
    )
  }, ignoreNULL = TRUE) |>
    debounce(millis = ritardo_filtri_ms)
  
  observeEvent(periodo_modal_input(), {
    filtri <- periodo_modal_input()
    
    if (!identical(
      filtri$apertura_id,
      isolate(modal_apertura_id())
    )) {
      return()
    }
    
    # Unica sorgente dati del modal mentre e' aperto.
    # L'aggiornamento avviene solo dopo il debounce.
    if (!identical(
      isolate(date_modal_corrente()),
      filtri$date
    )) {
      date_modal_corrente(filtri$date)
    }
    
    # Il modal puo' riportare la scelta alla Home, ma la Home non
    # riscrive mai modal_date: in questo modo non esiste piu' il ciclo
    # input$date -> modal_date -> input$date.
    if (!identical(
      isolate(as.Date(input$date)),
      filtri$date
    )) {
      updateDateRangeInput(
        session,
        "date",
        start = filtri$date[1],
        end = filtri$date[2]
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$modal_granularita_attivazioni, {
    req(input$modal_granularita_attivazioni)
    granularita_attivazioni_corrente(input$modal_granularita_attivazioni)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$modal_granularita_nok, {
    req(input$modal_granularita_nok)
    granularita_nok_corrente(input$modal_granularita_nok)
  }, ignoreNULL = TRUE)
  
  # Il selettore Settimana/Mese vive fuori dal renderUI dinamico della card:
  # cosi' il cambio valore non distrugge e ricrea lo stesso input.
  observeEvent(input$modal_granularita_utilizzo, {
    req(input$modal_granularita_utilizzo)
    granularita_utilizzo_corrente(input$modal_granularita_utilizzo)
  }, ignoreNULL = TRUE)
  
  filtri_attivazioni_modal <- reactive({
    req(macchina_attiva())
    
    date_corrente <- date_modal_corrente()
    req(
      length(date_corrente) == 2,
      all(!is.na(date_corrente))
    )
    
    list(
      macchina = macchina_attiva(),
      date = date_corrente,
      sensori = sensori_modal_correnti(),
      granularita = granularita_attivazioni_corrente()
    )
  })
  
  filtri_vita_modal <- reactive({
    req(macchina_attiva())
    
    list(
      macchina = macchina_attiva(),
      sensori = sensori_modal_correnti()
    )
  })
  
  filtri_nok_modal <- reactive({
    req(macchina_attiva())
    
    date_corrente <- date_modal_corrente()
    req(
      length(date_corrente) == 2,
      all(!is.na(date_corrente))
    )
    
    list(
      macchina = macchina_attiva(),
      date = date_corrente,
      sensori = sensori_modal_correnti(),
      granularita = granularita_nok_corrente()
    )
  })

  
  # ---------------------------------------------------------------------
  # Report anomalie settimanali/mensili.
  # "Settimana" usa quattro fasce mensili (1-7, 8-14, 15-21, 22-fine mese),
  # cosi' un mese completo genera sempre quattro report.
  # ---------------------------------------------------------------------
  periodi_report <- reactive({
    date_corrente <- date_modal_corrente()
    req(length(date_corrente) == 2, all(!is.na(date_corrente)))
    
    inizio <- as.Date(date_corrente[1])
    fine <- as.Date(date_corrente[2])
    granularita <- if (
      is.null(input$report_granularita) ||
      !nzchar(input$report_granularita)
    ) {
      "Settimana"
    } else {
      input$report_granularita
    }
    
    mesi <- seq.Date(
      lubridate::floor_date(inizio, "month"),
      lubridate::floor_date(fine, "month"),
      by = "month"
    )
    
    periodi <- lapply(mesi, function(mese) {
      fine_mese <- as.Date(lubridate::ceiling_date(mese, "month") - lubridate::days(1))
      
      if (identical(granularita, "Mese")) {
        p_start <- max(inizio, mese)
        p_end <- min(fine, fine_mese)
        if (p_start <= p_end) {
          return(tibble(
            start = p_start,
            end = p_end,
            label = format(mese, "%B %Y")
          ))
        }
        return(NULL)
      }
      
      limiti <- list(
        c(1L, 7L),
        c(8L, 14L),
        c(15L, 21L),
        c(22L, lubridate::days_in_month(mese))
      )
      
      bind_rows(lapply(seq_along(limiti), function(i) {
        p_start <- mese + lubridate::days(limiti[[i]][1] - 1L)
        p_end <- mese + lubridate::days(limiti[[i]][2] - 1L)
        p_start <- max(inizio, as.Date(p_start))
        p_end <- min(fine, as.Date(p_end))
        
        if (p_start > p_end) return(NULL)
        
        tibble(
          start = p_start,
          end = p_end,
          label = paste0(
            "Sett. ", i, " - ",
            format(p_start, "%d/%m"),
            " - ",
            format(p_end, "%d/%m")
          )
        )
      }))
    })
    
    bind_rows(periodi)
  })
  
  output$report_period_buttons <- renderUI({
    periodi <- periodi_report()
    
    if (nrow(periodi) == 0) {
      return(div(class = "alarm-empty", "Nessun periodo disponibile."))
    }
    
    div(
      class = "report-buttons",
      lapply(seq_len(nrow(periodi)), function(i) {
        p <- periodi[i, ]
        tags$button(
          type = "button",
          class = "report-period-btn",
          onclick = sprintf(
            "Shiny.setInputValue('report_periodo_click',{start:'%s',end:'%s',label:'%s',nonce:Date.now()},{priority:'event'});",
            format(p$start, "%Y-%m-%d"),
            format(p$end, "%Y-%m-%d"),
            gsub("'", "\\'", p$label)
          ),
          p$label
        )
      })
    )
  })
  
  calcola_report_periodo <- function(inizio, fine, macchina, sensori) {
    inizio <- as.Date(inizio)
    fine <- as.Date(fine)
    durata <- as.integer(fine - inizio) + 1L
    prev_fine <- inizio - 1L
    prev_inizio <- inizio - durata
    
    macchina_info <- macchine_lookup |>
      filter(coupon == macchina) |>
      slice_head(n = 1)
    
    nome_macchina <- if (nrow(macchina_info) > 0) {
      as.character(macchina_info$machine_name)
    } else {
      macchina
    }
    
    base_giornaliera <- prepara_valori_giornalieri_nok(macchina, sensori)
    
    # NMN e limiti storici sono calcolati escludendo il periodo del report.
    storico_pulito <- base_giornaliera |>
      filter(
        !outlier_lof,
        day < inizio | day > fine
      )
    
    nmn <- storico_pulito |>
      group_by(cds_name, sensor_description) |>
      summarise(
        NMN = mean(daily_value, na.rm = TRUE),
        .groups = "drop"
      )
    
    storico_nok <- storico_pulito |>
      left_join(nmn, by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK_giornaliero = if_else(
          is.finite(daily_value) & is.finite(NMN) & NMN != 0,
          daily_value / NMN,
          NA_real_
        )
      ) |>
      filter(is.finite(NOK_giornaliero))
    
    limiti_nok <- storico_nok |>
      group_by(cds_name, sensor_description) |>
      summarise(
        media_nok_storico = mean(NOK_giornaliero, na.rm = TRUE),
        sigma_nok_storico = if (n() >= 2) sd(NOK_giornaliero, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) |>
      mutate(
        limite_inf = pmax(0, media_nok_storico - 3 * sigma_nok_storico),
        limite_sup = media_nok_storico + 3 * sigma_nok_storico
      )
    
    metriche_periodo <- function(data_start, data_end) {
      periodo <- base_giornaliera |>
        filter(day >= data_start, day <= data_end)
      
      att <- periodo |>
        group_by(cds_name, sensor_description) |>
        summarise(
          attivazioni_medie = mean(daily_count, na.rm = TRUE),
          gateway_disconnect_affected = any(
            gateway_disconnect_affected,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      nok <- periodo |>
        group_by(cds_name, sensor_description) |>
        summarise(
          NMM = mean(daily_value, na.rm = TRUE),
          .groups = "drop"
        ) |>
        left_join(nmn, by = c("cds_name", "sensor_description")) |>
        mutate(
          NOK = if_else(
            is.finite(NMM) & is.finite(NMN) & NMN != 0,
            NMM / NMN,
            NA_real_
          )
        ) |>
        select(cds_name, sensor_description, NOK)
      
      att |>
        full_join(nok, by = c("cds_name", "sensor_description"))
    }
    
    correnti <- metriche_periodo(inizio, fine) |>
      left_join(
        limiti_nok |>
          select(cds_name, sensor_description, limite_inf, limite_sup),
        by = c("cds_name", "sensor_description")
      )
    
    precedenti <- metriche_periodo(prev_inizio, prev_fine) |>
      rename(
        attivazioni_medie_precedenti = attivazioni_medie,
        NOK_precedente = NOK,
        gateway_disconnect_affected_precedente =
          gateway_disconnect_affected
      )
    
    confronto <- correnti |>
      left_join(
        precedenti,
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        variazione_attivazioni_pct = case_when(
          is.finite(attivazioni_medie_precedenti) &
            attivazioni_medie_precedenti != 0 ~
              100 * (attivazioni_medie - attivazioni_medie_precedenti) /
              abs(attivazioni_medie_precedenti),
          TRUE ~ NA_real_
        ),
        variazione_nok_pct = case_when(
          is.finite(NOK_precedente) & NOK_precedente != 0 ~
            100 * (NOK - NOK_precedente) / abs(NOK_precedente),
          TRUE ~ NA_real_
        ),
        nok_fuori_range = (
          is.finite(NOK) &
          is.finite(limite_inf) &
          is.finite(limite_sup) &
          (NOK < limite_inf | NOK > limite_sup)
        )
      ) |>
      ordina_naturale()
    
    # Metriche macchina.
    att_macchina <- sum(confronto$attivazioni_medie, na.rm = TRUE)
    att_macchina_prev <- sum(confronto$attivazioni_medie_precedenti, na.rm = TRUE)
    att_macchina_pct <- if (
      is.finite(att_macchina_prev) && att_macchina_prev != 0
    ) {
      100 * (att_macchina - att_macchina_prev) / abs(att_macchina_prev)
    } else {
      NA_real_
    }
    
    nok_macchina <- mean(confronto$NOK[is.finite(confronto$NOK)], na.rm = TRUE)
    nok_macchina_prev <- mean(
      confronto$NOK_precedente[is.finite(confronto$NOK_precedente)],
      na.rm = TRUE
    )
    if (!is.finite(nok_macchina)) nok_macchina <- NA_real_
    if (!is.finite(nok_macchina_prev)) nok_macchina_prev <- NA_real_
    
    nok_macchina_pct <- if (
      is.finite(nok_macchina_prev) && nok_macchina_prev != 0
    ) {
      100 * (nok_macchina - nok_macchina_prev) / abs(nok_macchina_prev)
    } else {
      NA_real_
    }
    
    storico_macchina <- storico_nok |>
      group_by(day) |>
      summarise(
        NOK_macchina = mean(NOK_giornaliero, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(is.finite(NOK_macchina))
    
    nok_macchina_media_storica <- if (nrow(storico_macchina) > 0) {
      mean(storico_macchina$NOK_macchina, na.rm = TRUE)
    } else NA_real_
    nok_macchina_sigma <- if (nrow(storico_macchina) >= 2) {
      sd(storico_macchina$NOK_macchina, na.rm = TRUE)
    } else NA_real_
    nok_macchina_inf <- if (is.finite(nok_macchina_media_storica) && is.finite(nok_macchina_sigma)) {
      max(0, nok_macchina_media_storica - 3 * nok_macchina_sigma)
    } else NA_real_
    nok_macchina_sup <- if (is.finite(nok_macchina_media_storica) && is.finite(nok_macchina_sigma)) {
      nok_macchina_media_storica + 3 * nok_macchina_sigma
    } else NA_real_
    nok_macchina_fuori <- (
      is.finite(nok_macchina) &&
      is.finite(nok_macchina_inf) &&
      is.finite(nok_macchina_sup) &&
      (nok_macchina < nok_macchina_inf || nok_macchina > nok_macchina_sup)
    )
    
    calcola_utilizzo <- function(data_start, data_end) {
      dati_u <- base_giornaliera |>
        filter(day >= data_start, day <= data_end) |>
        left_join(nmn, by = c("cds_name", "sensor_description")) |>
        mutate(
          NOK_giornaliero = if_else(
            is.finite(daily_value) & is.finite(NMN) & NMN != 0,
            daily_value / NMN,
            NA_real_
          )
        ) |>
        filter(is.finite(NOK_giornaliero)) |>
        group_by(cds_name, sensor_description) |>
        summarise(
          media_nok = mean(NOK_giornaliero, na.rm = TRUE),
          varianza_nok = if (n() >= 2) var(NOK_giornaliero, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) |>
        mutate(
          U_sensore = sqrt(media_nok^2 + varianza_nok)
        ) |>
        filter(is.finite(U_sensore))
      
      if (nrow(dati_u) == 0) return(NA_real_)
      mean(dati_u$U_sensore, na.rm = TRUE)
    }
    
    utilizzo_corrente <- calcola_utilizzo(inizio, fine)
    utilizzo_precedente <- calcola_utilizzo(prev_inizio, prev_fine)
    utilizzo_pct <- if (
      is.finite(utilizzo_corrente) &&
      is.finite(utilizzo_precedente) &&
      utilizzo_precedente != 0
    ) {
      100 * (utilizzo_corrente - utilizzo_precedente) /
        abs(utilizzo_precedente)
    } else NA_real_
    
    prima_data <- min(base_giornaliera$day, na.rm = TRUE)
    ultima_start <- max(base_giornaliera$day, na.rm = TRUE) - durata + 1L
    
    partenze <- if (
      is.finite(as.numeric(prima_data)) &&
      is.finite(as.numeric(ultima_start)) &&
      ultima_start >= prima_data
    ) {
      seq.Date(prima_data, ultima_start, by = "7 days")
    } else {
      as.Date(character(0))
    }
    
    valori_u_storici <- vapply(
      partenze,
      function(x) calcola_utilizzo(x, x + durata - 1L),
      numeric(1)
    )
    valori_u_storici <- valori_u_storici[is.finite(valori_u_storici)]
    
    p10_u <- if (length(valori_u_storici) > 0) {
      as.numeric(stats::quantile(valori_u_storici, 0.10, names = FALSE, na.rm = TRUE))
    } else NA_real_
    p90_u <- if (length(valori_u_storici) > 0) {
      as.numeric(stats::quantile(valori_u_storici, 0.90, names = FALSE, na.rm = TRUE))
    } else NA_real_
    
    utilizzo_stato <- case_when(
      !is.finite(utilizzo_corrente) | !is.finite(p10_u) | !is.finite(p90_u) ~ "N/D",
      utilizzo_corrente < p10_u ~ "Basso",
      utilizzo_corrente > p90_u ~ "Elevato",
      TRUE ~ "Normale"
    )
    
    # Anomalie attivazioni nel periodo.
    anomalie_giornaliere <- base_giornaliera |>
      filter(
        outlier_lof,
        day >= inizio,
        day <= fine,
        is.finite(daily_count),
        daily_count > 6
      ) |>
      transmute(
        Macchina = nome_macchina,
        Coupon = macchina,
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        Data = day,
        Ora = "-",
        Conteggio = round(daily_count),
        Descrizione = paste0(
          if_else(
            is.finite(daily_count) & daily_count == 0,
            "Zero attivazioni",
            "Anomalia statistica giornaliera"
          ),
          if_else(
            gateway_disconnect_affected,
            nota_disconnessione_gateway,
            ""
          )
        )
      )
    
    mediane_macchina <- mediane_attivazioni_orarie |>
      filter(coupon == macchina)
    
    anomalie_orarie <- dati_attivazioni_orarie_base |>
      filter(
        coupon == macchina,
        cds_name %in% sensori,
        day >= inizio,
        day <= fine
      ) |>
      left_join(
        mediane_macchina |>
          select(cds_name, sensor_description, mediana_oraria),
        by = c("cds_name", "sensor_description")
      ) |>
      filter(
        is.finite(attivazioni_orarie),
        attivazioni_orarie > 6,
        is.finite(mediana_oraria),
        mediana_oraria > 0,
        attivazioni_orarie >= 4 * mediana_oraria
      ) |>
      transmute(
        Macchina = nome_macchina,
        Coupon = macchina,
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        Data = day,
        Ora = format(hour, "%H:00", tz = "Europe/Rome"),
        Conteggio = round(attivazioni_orarie),
        Descrizione = paste0(
          "Conteggio orario >= 4 x mediana oraria (",
          round(mediana_oraria, 1),
          ")",
          if_else(
            gateway_disconnect_affected,
            nota_disconnessione_gateway,
            ""
          )
        )
      )
    
    tab_att_anomale <- bind_rows(
      anomalie_giornaliere,
      anomalie_orarie
    ) |>
      distinct() |>
      arrange(Data, Ora, Sensore)
    
    tab_nok_anomalo <- confronto |>
      filter(nok_fuori_range) |>
      transmute(
        Macchina = nome_macchina,
        Coupon = macchina,
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        Periodo = paste(
          format(inizio, "%d/%m/%Y"),
          format(fine, "%d/%m/%Y"),
          sep = " - "
        ),
        NOK = round(NOK, 3),
        Descrizione = paste0(
          case_when(
            NOK < limite_inf ~ paste0(
              "NOK sotto il limite inferiore (",
              round(limite_inf, 3),
              ")"
            ),
            NOK > limite_sup ~ paste0(
              "NOK sopra il limite superiore (",
              round(limite_sup, 3),
              ")"
            ),
            TRUE ~ "NOK fuori range"
          ),
          if_else(
            gateway_disconnect_affected,
            nota_disconnessione_gateway,
            ""
          )
        )
      )
    
    periodo_affetto_disconnessione <- base_giornaliera |>
      filter(day >= inizio, day <= fine) |>
      summarise(
        flag = any(gateway_disconnect_affected, na.rm = TRUE)
      ) |>
      pull(flag)
    
    tab_utilizzo_anomalo <- if (
      identical(utilizzo_stato, "Elevato") ||
      identical(utilizzo_stato, "Basso")
    ) {
      tibble(
        Macchina = nome_macchina,
        Coupon = macchina,
        Periodo = paste(
          format(inizio, "%d/%m/%Y"),
          format(fine, "%d/%m/%Y"),
          sep = " - "
        ),
        Utilizzo = round(utilizzo_corrente, 3),
        Descrizione = if (
          identical(utilizzo_stato, "Elevato")
        ) {
          paste0(
            "Utilizzo sopra P90 (",
            round(p90_u, 3),
            ")",
            if (isTRUE(periodo_affetto_disconnessione)) {
              nota_disconnessione_gateway
            } else {
              ""
            }
          )
        } else {
          paste0(
            "Utilizzo sotto P10 (",
            round(p10_u, 3),
            ")",
            if (isTRUE(periodo_affetto_disconnessione)) {
              nota_disconnessione_gateway
            } else {
              ""
            }
          )
        }
      )
    } else {
      tibble(
        Macchina = character(),
        Coupon = character(),
        Periodo = character(),
        Utilizzo = numeric(),
        Descrizione = character()
      )
    }
    
    list(
      inizio = inizio,
      fine = fine,
      precedente_inizio = prev_inizio,
      precedente_fine = prev_fine,
      macchina = nome_macchina,
      coupon = macchina,
      confronto = confronto,
      att_macchina = att_macchina,
      att_macchina_pct = att_macchina_pct,
      nok_macchina = nok_macchina,
      nok_macchina_pct = nok_macchina_pct,
      nok_macchina_inf = nok_macchina_inf,
      nok_macchina_sup = nok_macchina_sup,
      nok_macchina_fuori = nok_macchina_fuori,
      utilizzo = utilizzo_corrente,
      utilizzo_pct = utilizzo_pct,
      utilizzo_stato = utilizzo_stato,
      p10_utilizzo = p10_u,
      p90_utilizzo = p90_u,
      attivazioni_anomale = tab_att_anomale,
      nok_anomalo = tab_nok_anomalo,
      utilizzo_anomalo = tab_utilizzo_anomalo
    )
  }
  
  report_dati <- reactive({
    periodo <- report_periodo_corrente()
    req(!is.null(periodo))
    
    calcola_report_periodo(
      periodo$start,
      periodo$end,
      macchina_attiva(),
      sensori_modal_correnti()
    )
  })
  
  formatta_variazione_report <- function(x) {
    if (!is.finite(x)) return("N/D")
    paste0(
      ifelse(x > 0, "+", ""),
      formatC(x, format = "f", digits = 1, decimal.mark = ","),
      "%"
    )
  }
  
  testo_andamento_report <- function(x, nome) {
    if (!is.finite(x)) return(paste(nome, "non confrontabile con il periodo precedente"))
    if (abs(x) < 0.05) return(paste(nome, "sostanzialmente stabile"))
    paste(
      nome,
      ifelse(x > 0, "in aumento del", "in diminuzione del"),
      paste0(formatC(abs(x), format = "f", digits = 1, decimal.mark = ","), "%")
    )
  }

  
  testo_report_esteso <- function(r) {
    n_sensori <- nrow(r$confronto)
    n_nok_fuori <- nrow(r$nok_anomalo)
    n_att_anomale <- nrow(r$attivazioni_anomale)
    
    sensori_att_anomali <- if (n_att_anomale > 0) {
      dplyr::n_distinct(r$attivazioni_anomale$Sensore)
    } else {
      0L
    }
    
    utilizzo_range <- if (
      is.finite(r$p10_utilizzo) &&
      is.finite(r$p90_utilizzo)
    ) {
      paste0(
        "intervallo storico di riferimento da ",
        formatC(r$p10_utilizzo, format = "f", digits = 3, decimal.mark = ","),
        " a ",
        formatC(r$p90_utilizzo, format = "f", digits = 3, decimal.mark = ",")
      )
    } else {
      "intervallo storico non disponibile"
    }
    
    nok_range <- if (
      is.finite(r$nok_macchina_inf) &&
      is.finite(r$nok_macchina_sup)
    ) {
      paste0(
        "range storico da ",
        formatC(r$nok_macchina_inf, format = "f", digits = 3, decimal.mark = ","),
        " a ",
        formatC(r$nok_macchina_sup, format = "f", digits = 3, decimal.mark = ",")
      )
    } else {
      "range storico non disponibile"
    }
    
    stato_utilizzo <- if (identical(r$utilizzo_stato, "Normale")) {
      "all'interno del comportamento storico atteso"
    } else if (identical(r$utilizzo_stato, "Elevato")) {
      "sopra il livello storico di riferimento"
    } else if (identical(r$utilizzo_stato, "Basso")) {
      "sotto il livello storico di riferimento"
    } else {
      "non classificabile rispetto allo storico disponibile"
    }
    
    stato_nok <- if (isTRUE(r$nok_macchina_fuori)) {
      "fuori dal proprio intervallo storico"
    } else {
      "all'interno del proprio intervallo storico"
    }
    
    p1 <- paste0(
      "Il presente rapporto sintetizza il comportamento operativo della macchina ",
      r$macchina,
      " (coupon ",
      r$coupon,
      ") nel periodo ",
      format(r$inizio, "%d/%m/%Y"),
      " - ",
      format(r$fine, "%d/%m/%Y"),
      ". I risultati vengono letti in confronto al periodo immediatamente precedente di pari durata, ",
      format(r$precedente_inizio, "%d/%m/%Y"),
      " - ",
      format(r$precedente_fine, "%d/%m/%Y"),
      ", così da evidenziare variazioni recenti senza perdere il riferimento allo storico disponibile."
    )
    
    p2 <- paste0(
      "Sul piano dell'intensità di utilizzo, ",
      testo_andamento_report(r$utilizzo_pct, "l'indice complessivo"),
      ". Il valore del periodo è ",
      ifelse(
        is.finite(r$utilizzo),
        formatC(r$utilizzo, format = "f", digits = 3, decimal.mark = ","),
        "N/D"
      ),
      " e risulta ",
      stato_utilizzo,
      " rispetto all'",
      utilizzo_range,
      ". La variazione percentuale va quindi interpretata insieme alla posizione nel range: un incremento non costituisce automaticamente un'anomalia se il livello finale resta coerente con il comportamento storico della macchina."
    )
    
    p3 <- paste0(
      "Il NOK medio della macchina è ",
      ifelse(
        is.finite(r$nok_macchina),
        formatC(r$nok_macchina, format = "f", digits = 3, decimal.mark = ","),
        "N/D"
      ),
      "; ",
      testo_andamento_report(r$nok_macchina_pct, "rispetto al periodo precedente"),
      ". Nel complesso il NOK è ",
      stato_nok,
      " (",
      nok_range,
      "). A livello di dettaglio, ",
      n_nok_fuori,
      " sensori su ",
      n_sensori,
      " risultano fuori dai rispettivi limiti storici nel periodo considerato."
    )
    
    p4 <- if (n_att_anomale > 0) {
      paste0(
        "Sono inoltre presenti ",
        n_att_anomale,
        " segnalazioni di attivazioni anomale distribuite su ",
        sensori_att_anomali,
        " sensori. Le tabelle successive permettono di risalire al sensore, alla data e, quando disponibile, all'ora dell'evento."
      )
    } else {
      paste0(
        "Nel periodo non risultano anomalie di attivazione rilevanti. Il quadro operativo non evidenzia quindi eventi di conteggio meritevoli di segnalazione nel periodo considerato."
      )
    }
    
    c(
      paste(p1, p2),
      paste(p3, p4)
    )
  }
  
  observeEvent(input$report_periodo_click, {
    req(input$report_periodo_click$start, input$report_periodo_click$end)
    
    report_periodo_corrente(list(
      start = as.Date(input$report_periodo_click$start),
      end = as.Date(input$report_periodo_click$end),
      label = input$report_periodo_click$label
    ))
    
    showModal(modalDialog(
      title = NULL,
      size = "xl",
      easyClose = TRUE,
      footer = NULL,
      uiOutput("report_modal_content")
    ))
  })

  
  observeEvent(input$report_back_to_alarms, {
    sensori_ripristino <- isolate(sensori_modal_correnti())
    date_ripristino <- isolate(date_modal_corrente())
    
    removeModal()
    
    apri_modal(
      "allarmi",
      sensori_iniziali = sensori_ripristino,
      date_iniziali = date_ripristino
    )
  }, ignoreInit = TRUE)
  
  output$report_modal_content <- renderUI({
    r <- report_dati()
    narrativa <- testo_report_esteso(r)
    
    sensori <- r$confronto |>
      mutate(
        sensore_label = paste(cds_name, sensor_description, sep = " - ")
      )
    
    cella_nok <- function(valore, fuori) {
      classe <- if (!is.finite(valore)) {
        "report-nd"
      } else if (isTRUE(fuori)) {
        "report-bad"
      } else {
        "report-ok"
      }
      
      stato <- if (!is.finite(valore)) {
        "N/D"
      } else if (isTRUE(fuori)) {
        "Fuori range"
      } else {
        "Nel range"
      }
      
      tagList(
        tags$span(
          class = classe,
          if (is.finite(valore)) {
            formatC(
              valore,
              format = "f",
              digits = 3,
              decimal.mark = ","
            )
          } else {
            "N/D"
          }
        ),
        tags$span(
          class = paste(
            "report-status-pill",
            if (isTRUE(fuori)) "report-status-bad" else "report-status-ok"
          ),
          stato
        )
      )
    }
    
    colonne <- c(
      lapply(seq_len(nrow(sensori)), function(i) {
        srow <- sensori[i, ]
        list(
          nome = srow$sensore_label,
          att = paste0(
            formatC(
              srow$attivazioni_medie,
              format = "f",
              digits = 1,
              decimal.mark = ","
            ),
            " (",
            formatta_variazione_report(
              srow$variazione_attivazioni_pct
            ),
            ")"
          ),
          nok = cella_nok(
            srow$NOK,
            srow$nok_fuori_range
          )
        )
      }),
      list(list(
        nome = "Macchina",
        att = paste0(
          formatC(
            r$att_macchina,
            format = "f",
            digits = 1,
            decimal.mark = ","
          ),
          " (",
          formatta_variazione_report(r$att_macchina_pct),
          ")"
        ),
        nok = cella_nok(
          r$nok_macchina,
          r$nok_macchina_fuori
        )
      ))
    )
    
    tabella_sintesi <- div(
      class = "report-table-wrap",
      tags$table(
        class = "report-table",
        tags$thead(
          tags$tr(
            tags$th("Metrica"),
            lapply(colonne, function(x) tags$th(x$nome))
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Attivazioni medie")),
            lapply(colonne, function(x) tags$td(x$att))
          ),
          tags$tr(
            tags$td(tags$strong("NOK")),
            lapply(colonne, function(x) tags$td(x$nok))
          )
        )
      )
    )
    
    tabella_html <- function(df, vuoto) {
      if (nrow(df) == 0) {
        return(div(class = "alarm-empty", vuoto))
      }
      
      div(
        class = "report-table-wrap",
        tags$table(
          class = "report-table",
          tags$thead(
            tags$tr(lapply(names(df), tags$th))
          ),
          tags$tbody(
            lapply(seq_len(nrow(df)), function(i) {
              tags$tr(
                lapply(
                  df[i, , drop = FALSE],
                  function(x) tags$td(as.character(x))
                )
              )
            })
          )
        )
      )
    }
    
    utilizzo_val <- if (is.finite(r$utilizzo)) {
      formatC(
        r$utilizzo,
        format = "f",
        digits = 3,
        decimal.mark = ","
      )
    } else {
      "N/D"
    }
    
    nok_val <- if (is.finite(r$nok_macchina)) {
      formatC(
        r$nok_macchina,
        format = "f",
        digits = 3,
        decimal.mark = ","
      )
    } else {
      "N/D"
    }
    
    n_att <- nrow(r$attivazioni_anomale)
    n_nok <- nrow(r$nok_anomalo)
    
    sezione <- function(numero, titolo, nota, contenuto) {
      div(
        class = "report-section",
        div(
          class = "report-section-heading",
          span(numero, class = "report-section-number"),
          h4(titolo, class = "report-section-title")
        ),
        div(nota, class = "report-section-note"),
        contenuto
      )
    }
    
    div(
      class = "report-modal",
      div(
        class = "report-hero",
        div(
          class = "report-modal-header",
          div(
            class = "report-heading-left",
            actionButton(
              "report_back_to_alarms",
              label = NULL,
              icon = icon("arrow-left"),
              class = "report-back-btn",
              title = "Torna agli allarmi"
            ),
            div(
              h2("Rapporto anomalie e utilizzo", class = "report-title"),
              div(
                paste0(
                  r$macchina,
                  " · Coupon ",
                  r$coupon
                ),
                class = "report-subtitle"
              )
            )
          ),
          downloadButton(
            "download_report_pdf",
            "Scarica PDF",
            icon = icon("file-pdf"),
            class = "report-download"
          )
        ),
        div(
          class = "report-meta",
          span(
            icon("calendar"),
            paste(
              format(r$inizio, "%d/%m/%Y"),
              format(r$fine, "%d/%m/%Y"),
              sep = " - "
            ),
            class = "report-meta-chip"
          ),
          span(
            icon("clock"),
            paste0(
              "Confronto: ",
              format(r$precedente_inizio, "%d/%m/%Y"),
              " - ",
              format(r$precedente_fine, "%d/%m/%Y")
            ),
            class = "report-meta-chip"
          )
        )
      ),
      
      div(
        class = "report-kpi-grid",
        div(
          class = "report-kpi-card",
          div("Indice utilizzo", class = "report-kpi-label"),
          div(utilizzo_val, class = "report-kpi-value"),
          div(
            paste0(
              r$utilizzo_stato,
              " · ",
              formatta_variazione_report(r$utilizzo_pct)
            ),
            class = "report-kpi-note"
          )
        ),
        div(
          class = "report-kpi-card",
          div("NOK medio macchina", class = "report-kpi-label"),
          div(nok_val, class = "report-kpi-value"),
          div(
            paste0(
              ifelse(
                isTRUE(r$nok_macchina_fuori),
                "Fuori range",
                "Nel range"
              ),
              " · ",
              formatta_variazione_report(r$nok_macchina_pct)
            ),
            class = "report-kpi-note"
          )
        ),
        div(
          class = "report-kpi-card",
          div("Anomalie attivazioni", class = "report-kpi-label"),
          div(n_att, class = "report-kpi-value"),
          div("Eventi rilevanti nel periodo", class = "report-kpi-note")
        ),
        div(
          class = "report-kpi-card",
          div("Sensori NOK fuori range", class = "report-kpi-label"),
          div(n_nok, class = "report-kpi-value"),
          div(
            paste0("Su ", nrow(r$confronto), " sensori analizzati"),
            class = "report-kpi-note"
          )
        )
      ),
      
      div(
        class = "report-summary",
        div("Lettura del periodo", class = "report-summary-title"),
        lapply(narrativa, function(x) tags$p(x))
      ),
      
      sezione(
        "1",
        "Sintesi per sensore e macchina",
        "Le attivazioni riportano il valore medio del periodo e, tra parentesi, la variazione rispetto al periodo precedente. Il NOK è evidenziato in base alla permanenza nel proprio range storico.",
        tabella_sintesi
      ),
      
      sezione(
        "2",
        "Attivazioni anomale rilevanti",
        "Dettaglio degli eventi di attivazione anomali rilevati nel periodo.",
        tabella_html(
          r$attivazioni_anomale |>
            mutate(Data = format(Data, "%d/%m/%Y")),
          "Nessuna attivazione anomala rilevante nel periodo."
        )
      ),
      
      sezione(
        "3",
        "NOK per sensore fuori range",
        "Dettaglio dei sensori il cui NOK di periodo supera i limiti storici calcolati.",
        tabella_html(
          r$nok_anomalo,
          "Nessun NOK per sensore fuori range nel periodo."
        )
      ),
      
      sezione(
        "4",
        "Utilizzo macchina fuori range",
        "La sezione viene popolata solo quando l'indice di utilizzo complessivo è sotto P10 o sopra P90 rispetto allo storico di riferimento.",
        tabella_html(
          r$utilizzo_anomalo,
          "L'utilizzo complessivo della macchina rientra nel range storico nel periodo."
        )
      )
    )
  })
  
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      r <- report_dati()
      paste0(
        "report_anomalie_",
        r$coupon,
        "_",
        format(r$inizio, "%Y%m%d"),
        "_",
        format(r$fine, "%Y%m%d"),
        ".pdf"
      )
    },
    content = function(file) {
      r <- report_dati()
      narrativa <- testo_report_esteso(r)
      
      grDevices::pdf(
        file,
        width = 11.69,
        height = 8.27,
        onefile = TRUE
      )
      on.exit(grDevices::dev.off(), add = TRUE)
      
      navy <- "#234A66"
      slate <- "#334155"
      muted <- "#718096"
      beige <- "#F1F3F4"
      beige_dark <- "#EBBD55"
      border <- "#DDE4EA"
      green <- "#19764A"
      red <- "#B42318"
      
      titolo_base <- "Rapporto anomalie e utilizzo"
      sottotitolo <- paste0(
        r$macchina,
        " · Coupon ",
        r$coupon,
        " · ",
        format(r$inizio, "%d/%m/%Y"),
        " - ",
        format(r$fine, "%d/%m/%Y")
      )
      
      footer <- function() {
        grid::grid.text(
          paste0(
            "5SAN · Report generato il ",
            format(Sys.time(), "%d/%m/%Y %H:%M")
          ),
          x = 0.03,
          y = 0.025,
          just = c("left", "bottom"),
          gp = grid::gpar(
            fontsize = 7.5,
            col = muted
          )
        )
      }
      
      header_pagina <- function(titolo_sezione = NULL) {
        grid::grid.rect(
          x = 0.5,
          y = 0.955,
          width = 1,
          height = 0.09,
          gp = grid::gpar(
            fill = navy,
            col = NA
          )
        )
        grid::grid.text(
          "5SAN · MONITORAGGIO OPERATIVO",
          x = 0.035,
          y = 0.972,
          just = c("left", "top"),
          gp = grid::gpar(
            fontsize = 7.5,
            fontface = "bold",
            col = "#DDD6BD"
          )
        )
        grid::grid.text(
          if (is.null(titolo_sezione)) titolo_base else titolo_sezione,
          x = 0.035,
          y = 0.945,
          just = c("left", "top"),
          gp = grid::gpar(
            fontsize = 15,
            fontface = "bold",
            col = "#FFFFFF"
          )
        )
        footer()
      }
      
      kpi_card <- function(x, label, value, note, status = "normal") {
        status_col <- if (identical(status, "bad")) red else if (
          identical(status, "good")
        ) green else navy
        
        grid::grid.roundrect(
          x = x,
          y = 0.685,
          width = 0.215,
          height = 0.145,
          r = grid::unit(0.035, "snpc"),
          gp = grid::gpar(
            fill = "#FFFFFF",
            col = border,
            lwd = 1
          )
        )
        grid::grid.text(
          label,
          x = x - 0.092,
          y = 0.73,
          just = c("left", "center"),
          gp = grid::gpar(
            fontsize = 7.5,
            fontface = "bold",
            col = muted
          )
        )
        grid::grid.text(
          value,
          x = x - 0.092,
          y = 0.688,
          just = c("left", "center"),
          gp = grid::gpar(
            fontsize = 16,
            fontface = "bold",
            col = status_col
          )
        )
        grid::grid.text(
          note,
          x = x - 0.092,
          y = 0.647,
          just = c("left", "center"),
          gp = grid::gpar(
            fontsize = 7.3,
            col = muted
          )
        )
      }
      
      # Pagina 1 - Executive summary.
      grid::grid.newpage()
      grid::grid.rect(
        x = 0.5,
        y = 0.90,
        width = 1,
        height = 0.20,
        gp = grid::gpar(fill = navy, col = NA)
      )
      grid::grid.text(
        "5SAN · MONITORAGGIO OPERATIVO",
        x = 0.04,
        y = 0.965,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 8,
          fontface = "bold",
          col = "#DDD6BD"
        )
      )
      grid::grid.text(
        titolo_base,
        x = 0.04,
        y = 0.925,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 23,
          fontface = "bold",
          col = "#FFFFFF"
        )
      )
      grid::grid.text(
        sottotitolo,
        x = 0.04,
        y = 0.865,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 10,
          col = "#DDE5EC"
        )
      )
      grid::grid.text(
        paste0(
          "Confronto: ",
          format(r$precedente_inizio, "%d/%m/%Y"),
          " - ",
          format(r$precedente_fine, "%d/%m/%Y"),
          ""
        ),
        x = 0.04,
        y = 0.832,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 8,
          col = "#DDE5EC"
        )
      )
      
      utilizzo_val <- if (is.finite(r$utilizzo)) {
        formatC(r$utilizzo, format = "f", digits = 3, decimal.mark = ",")
      } else "N/D"
      nok_val <- if (is.finite(r$nok_macchina)) {
        formatC(r$nok_macchina, format = "f", digits = 3, decimal.mark = ",")
      } else "N/D"
      
      kpi_card(
        0.145,
        "INDICE UTILIZZO",
        utilizzo_val,
        paste0(r$utilizzo_stato, " · ", formatta_variazione_report(r$utilizzo_pct)),
        if (r$utilizzo_stato %in% c("Elevato", "Basso")) "bad" else "good"
      )
      kpi_card(
        0.385,
        "NOK MEDIO MACCHINA",
        nok_val,
        paste0(
          ifelse(isTRUE(r$nok_macchina_fuori), "Fuori range", "Nel range"),
          " · ",
          formatta_variazione_report(r$nok_macchina_pct)
        ),
        if (isTRUE(r$nok_macchina_fuori)) "bad" else "good"
      )
      kpi_card(
        0.625,
        "ANOMALIE ATTIVAZIONI",
        as.character(nrow(r$attivazioni_anomale)),
        "Eventi rilevanti",
        if (nrow(r$attivazioni_anomale) > 0) "bad" else "good"
      )
      kpi_card(
        0.865,
        "SENSORI NOK FUORI RANGE",
        as.character(nrow(r$nok_anomalo)),
        paste0("Su ", nrow(r$confronto), " sensori"),
        if (nrow(r$nok_anomalo) > 0) "bad" else "good"
      )
      
      grid::grid.text(
        "Lettura del periodo",
        x = 0.04,
        y = 0.57,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 13,
          fontface = "bold",
          col = navy
        )
      )
      grid::grid.rect(
        x = 0.5,
        y = 0.36,
        width = 0.92,
        height = 0.38,
        gp = grid::gpar(
          fill = beige,
          col = "#D9DDE0"
        )
      )
      
      testo_exec <- paste(
        vapply(
          narrativa,
          function(x) paste(strwrap(x, width = 155), collapse = "\n"),
          character(1)
        ),
        collapse = "\n\n"
      )
      grid::grid.text(
        testo_exec,
        x = 0.06,
        y = 0.53,
        just = c("left", "top"),
        gp = grid::gpar(
          fontsize = 9,
          col = slate,
          lineheight = 1.25
        )
      )
      footer()
      
      # Sintesi per sensore: più pagine, 7 sensori alla volta + macchina.
      sensori_summary <- r$confronto |>
        transmute(
          Sensore = cds_name,
          Attivazioni = paste0(
            formatC(attivazioni_medie, format = "f", digits = 1, decimal.mark = ","),
            " (",
            vapply(
              variazione_attivazioni_pct,
              formatta_variazione_report,
              character(1)
            ),
            ")"
          ),
          NOK = ifelse(
            is.finite(NOK),
            formatC(NOK, format = "f", digits = 3, decimal.mark = ","),
            "N/D"
          ),
          Stato = ifelse(nok_fuori_range, "FUORI", "OK")
        )
      
      idx_chunks <- split(
        seq_len(nrow(sensori_summary)),
        ceiling(seq_len(nrow(sensori_summary)) / 7)
      )
      
      if (length(idx_chunks) == 0) idx_chunks <- list(integer(0))
      
      for (chunk_i in seq_along(idx_chunks)) {
        idx <- idx_chunks[[chunk_i]]
        chunk <- sensori_summary[idx, , drop = FALSE]
        
        metriche <- c("Attivazioni medie", "NOK")
        tab <- data.frame(Metrica = metriche, check.names = FALSE)
        
        if (nrow(chunk) > 0) {
          for (i in seq_len(nrow(chunk))) {
            tab[[chunk$Sensore[i]]] <- c(
              chunk$Attivazioni[i],
              paste0(chunk$NOK[i], " · ", chunk$Stato[i])
            )
          }
        }
        
        if (chunk_i == length(idx_chunks)) {
          tab[["Macchina"]] <- c(
            paste0(
              formatC(r$att_macchina, format = "f", digits = 1, decimal.mark = ","),
              " (",
              formatta_variazione_report(r$att_macchina_pct),
              ")"
            ),
            paste0(
              nok_val,
              " · ",
              ifelse(isTRUE(r$nok_macchina_fuori), "FUORI", "OK")
            )
          )
        }
        
        grid::grid.newpage()
        header_pagina("Sintesi per sensore e macchina")
        grid::grid.text(
          paste0(
            "Valori medi del periodo; tra parentesi la variazione rispetto al periodo precedente. ",
            "NOK classificato rispetto al range storico."
          ),
          x = 0.04,
          y = 0.84,
          just = c("left", "top"),
          gp = grid::gpar(fontsize = 8.5, col = muted)
        )
        
        tg <- gridExtra::tableGrob(
          tab,
          rows = NULL,
          theme = gridExtra::ttheme_minimal(
            base_size = 8,
            core = list(
              fg_params = list(col = slate),
              bg_params = list(fill = c("#FFFFFF", "#FAFBFC"))
            ),
            colhead = list(
              fg_params = list(fontface = "bold", col = navy),
              bg_params = list(fill = "#EDF1F4")
            )
          )
        )
        grid::pushViewport(
          grid::viewport(
            x = 0.04,
            y = 0.79,
            width = 0.92,
            height = 0.60,
            just = c("left", "top")
          )
        )
        grid::grid.draw(tg)
        grid::popViewport()
      }
      
      disegna_tabella_paginata <- function(titolo_sezione, df, empty_text) {
        if (nrow(df) == 0) {
          grid::grid.newpage()
          header_pagina(titolo_sezione)
          grid::grid.roundrect(
            x = 0.5,
            y = 0.60,
            width = 0.88,
            height = 0.18,
            r = grid::unit(0.03, "snpc"),
            gp = grid::gpar(fill = beige, col = "#D9DDE0")
          )
          grid::grid.text(
            empty_text,
            x = 0.5,
            y = 0.60,
            gp = grid::gpar(
              fontsize = 10,
              col = slate
            )
          )
          return(invisible(NULL))
        }
        
        chunks <- split(
          seq_len(nrow(df)),
          ceiling(seq_len(nrow(df)) / 12)
        )
        
        for (idx in chunks) {
          grid::grid.newpage()
          header_pagina(titolo_sezione)
          
          tg <- gridExtra::tableGrob(
            as.data.frame(df[idx, , drop = FALSE]),
            rows = NULL,
            theme = gridExtra::ttheme_minimal(
              base_size = 7.4,
              core = list(
                fg_params = list(col = slate, hjust = 0, x = 0.03),
                bg_params = list(fill = rep(c("#FFFFFF", "#FAFBFC"), length.out = length(idx)))
              ),
              colhead = list(
                fg_params = list(fontface = "bold", col = navy),
                bg_params = list(fill = "#EDF1F4")
              )
            )
          )
          
          grid::pushViewport(
            grid::viewport(
              x = 0.035,
              y = 0.84,
              width = 0.93,
              height = 0.70,
              just = c("left", "top")
            )
          )
          grid::grid.draw(tg)
          grid::popViewport()
        }
      }
      
      att_pdf <- r$attivazioni_anomale |>
        mutate(Data = format(Data, "%d/%m/%Y"))
      
      disegna_tabella_paginata(
        "Attivazioni anomale rilevanti",
        att_pdf,
        "Nessuna attivazione anomala rilevante nel periodo."
      )
      
      disegna_tabella_paginata(
        "NOK per sensore fuori range",
        r$nok_anomalo,
        "Nessun NOK per sensore fuori range nel periodo."
      )
      
      disegna_tabella_paginata(
        "Utilizzo macchina fuori range",
        r$utilizzo_anomalo,
        "L'utilizzo complessivo della macchina rientra nel range storico."
      )
    },
    contentType = "application/pdf"
  )
  
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
  
  output$modal_panel_dati_profilo_nok <- renderUI({
    if (!mostra_dati_profilo_nok()) return(NULL)
    div(
      class = "modal-data-panel",
      DTOutput("modal_tabella_dati_profilo_nok")
    )
  })
  
  output$modal_panel_storico_utilizzo_nok <- renderUI({
    if (!mostra_storico_utilizzo_nok()) return(NULL)
    
    div(
      class = "modal-data-panel",
      girafeOutput(
        "modal_storicoUtilizzoPlot",
        height = "300px"
      )
    )
  })
  
  output$modal_panel_outlier_nok <- renderUI({
    if (!mostra_outlier_nok()) return(NULL)
    
    div(
      class = "modal-data-panel",
      p(
        "Attivazioni anomale rilevate nel periodo selezionato. ",
        "Sono mostrate sia le anomalie giornaliere sia le ore con attivazioni almeno 4 volte superiori alla mediana oraria del sensore. Le anomalie giornaliere continuano a essere usate per pulire l'NMN storico."
      ),
      DTOutput("modal_tabella_outlier_nok")
    )
  })
  
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
  # Il LOF viene calcolato sul CONTEGGIO GIORNALIERO delle attivazioni
  # (non sul NOK), separatamente per ciascun sensore.
  prepara_valori_giornalieri_nok <- function(macchina, sensori) {
    
    dati |>
      ungroup() |>
      filter(
        coupon == macchina,
        cds_name %in% sensori
      ) |>
      group_by(cds_name, sensor_description, day) |>
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
      mutate(
        daily_value = case_when(
          is.na(daily_uptime) | daily_uptime <= 0 ~ NA_real_,
          TRUE ~ daily_count / daily_uptime
        )
      ) |>
      group_by(cds_name, sensor_description) |>
      group_modify(~ {
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
        
        # Se il sensore ha normalmente attivita' (> 0 in media),
        # una giornata valida con 0 attivazioni viene considerata anomala.
        zero_anomalo <- (
          validi &
          x == 0 &
          is.finite(media_attivazioni_giornaliere) &
          media_attivazioni_giornaliere > 0
        )
        .x$outlier_lof[zero_anomalo] <- TRUE
        
        # Il LOF viene calcolato sui conteggi giornalieri finiti.
        if (n_validi >= 6L && dplyr::n_distinct(x[validi]) >= 3L) {
          k <- min(4L, n_validi - 1L)
          score <- dbscan::lof(
            matrix(x[validi], ncol = 1),
            minPts = k
          )
          
          x_validi <- x[validi]
          
          # Per valutare il divario di ciascun candidato uso una mediana
          # leave-one-out: il valore che sto testando non contribuisce alla
          # propria mediana di riferimento.
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
          
          lof_anomalo <- (
            !is.na(score) &
            score > 2
          )
          
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
      ungroup()
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
          "; attivazioni giornaliere medie: ", attivazioni_medie,
          "; ", etichetta_ore_aperte, ": ", ore_aperte,
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
        class = "schema-frame",
        tags$img(
          src = paste0(
            "cds-images/",
            immagine$file_name,
            "?v=",
            image_version
          ),
          alt = paste("Schema sensori del progetto", progetto)
        ),
        hotspot
      )
    )
  })
  
  valori_giornalieri_modal_nok <- reactive({
    
    filtri <- filtri_nok_modal()
    req(length(filtri$sensori) > 0)
    
    prepara_valori_giornalieri_nok(
      filtri$macchina,
      filtri$sensori
    )
  }) |>
    bindCache(filtri_nok_modal()$macchina, filtri_nok_modal()$sensori)
  
  nmn_storico_modal_nok <- reactive({
    filtri <- filtri_nok_modal()
    
    valori_giornalieri_modal_nok() |>
      filter(
        !outlier_lof,
        day < filtri$date[1] | day > filtri$date[2]
      ) |>
      group_by(cds_name, sensor_description) |>
      summarise(NMN = mean(daily_value, na.rm = TRUE), .groups = "drop")
  })
  
  # Statistiche storiche del NOK giornaliero per sensore.
  # Limiti di riferimento: media storica +/- 3 sigma.
  statistiche_nok_storiche_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    valori_giornalieri_modal_nok() |>
      filter(
        !outlier_lof,
        day < filtri$date[1] | day > filtri$date[2]
      ) |>
      left_join(
        nmn_storico_modal_nok(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK_giornaliero = case_when(
          is.na(daily_value) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ daily_value / NMN
        )
      ) |>
      filter(is.finite(NOK_giornaliero)) |>
      group_by(cds_name, sensor_description) |>
      summarise(
        media_nok_storico = mean(NOK_giornaliero, na.rm = TRUE),
        sigma_nok_storico = if (n() >= 2) {
          sd(NOK_giornaliero, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        limite_nok_inf = pmax(0, media_nok_storico - 3 * sigma_nok_storico),
        limite_nok_sup = media_nok_storico + 3 * sigma_nok_storico
      )
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
      left_join(
        statistiche_nok_storiche_modal(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK = case_when(
          is.na(NMN) | is.na(NMM) | NMM == 0 ~ NA_real_,
          TRUE ~ NMM / NMN
        ),
        stato_nok = case_when(
          !is.finite(NOK) |
            !is.finite(limite_nok_inf) |
            !is.finite(limite_nok_sup) ~ "N/D",
          NOK < limite_nok_inf | NOK > limite_nok_sup ~ "Anomalo",
          TRUE ~ "Normale"
        )
      ) |>
      ordina_naturale()
  })
  
  output$modal_nok_table <- renderTable({
    
    kpi <- kpi_nok_modal()
    
    validate(
      need(nrow(kpi) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    kpi |>
      transmute(
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        NOK = case_when(
          is.na(NOK) | is.infinite(NOK) ~
            "<span style='color:#718096;font-weight:800;'>N/D</span>",
          stato_nok == "Anomalo" ~
            paste0(
              "<span style='color:#B42318;font-weight:800;'>",
              format(round(NOK, 3), nsmall = 3),
              "</span>"
            ),
          TRUE ~
            paste0(
              "<span style='color:#19764A;font-weight:800;'>",
              format(round(NOK, 3), nsmall = 3),
              "</span>"
            )
        )
      ) |>
      pivot_wider(names_from = Sensore, values_from = NOK)
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  spacing = "s",
  align = "c",
  sanitize.text.function = function(x) x)
  
  # ---------------------------------------------------------------------
  # Allarmi automatici nel periodo selezionato.
  # 1) anomalie sulle attivazioni: giornate escluse dal LOF;
  # 2) anomalie NOK: NOK giornaliero fuori da media storica +/- 3 sigma.
  # ---------------------------------------------------------------------
  allarmi_attivazioni_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    anomalie_giornaliere <- valori_giornalieri_modal_nok() |>
      filter(
        outlier_lof,
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      transmute(
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        Data = day,
        Ora = "-",
        Attivazioni = round(daily_count),
        Motivo = paste0(
          case_when(
            is.finite(daily_count) & daily_count == 0 ~ "Zero attivazioni",
            TRUE ~ "Anomalia statistica giornaliera"
          ),
          if_else(
            gateway_disconnect_affected,
            nota_disconnessione_gateway,
            ""
          )
        )
      )
    
    anomalie_orarie <- dati_attivazioni_orarie_modal() |>
      filter(anomalia_oraria) |>
      transmute(
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        Data = day,
        Ora = format(hour, "%H:00", tz = "Europe/Rome"),
        Attivazioni = round(attivazioni_orarie),
        Motivo = paste0(
          "Attivazioni orarie >= 4 x mediana oraria (",
          round(mediana_oraria, 1),
          ")",
          if_else(
            gateway_disconnect_affected,
            nota_disconnessione_gateway,
            ""
          )
        )
      )
    
    bind_rows(
      anomalie_giornaliere,
      anomalie_orarie
    ) |>
      distinct() |>
      arrange(Data, Ora, Sensore)
  })
  
  allarmi_sensori_aperti_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    raw_data_allarmi <- readRDS(
      here("02_Output", "raw_data.rds")
    ) |>
      mutate(
        field = if_else(
          is.na(field) | trimws(field) == "",
          "(Non specificato)",
          field
        ),
        timestamp_local = with_tz(
          timestamp,
          "Europe/Rome"
        ),
        day = as.Date(
          timestamp_local,
          tz = "Europe/Rome"
        )
      ) |>
      filter(
        coupon == filtri$macchina,
        cds_name %in% filtri$sensori,
        day >= filtri$date[1],
        day <= filtri$date[2]
      )
    
    gruppi_aperti <- c(
      "company", "field", "project", "coupon",
      "machine_name", "gateway_name", "cds_name",
      "cds_description", "cds_brand", "cds_use",
      "cds_vds", "sensor_description"
    )
    
    raw_data_allarmi |>
      group_by(
        across(all_of(gruppi_aperti)),
        day
      ) |>
      arrange(
        timestamp_local,
        .by_group = TRUE
      ) |>
      mutate(
        previous_timestamp = lag(timestamp_local),
        previous_status = lag(status),
        previous_raw_count = lag(count),
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
          !lag(
            sensore_aperto_intervallo,
            default = FALSE
          )
        ),
        open_event_id = cumsum(
          nuovo_evento_aperto
        )
      ) |>
      filter(sensore_aperto_intervallo) |>
      group_by(
        across(all_of(gruppi_aperti)),
        day,
        open_event_id
      ) |>
      summarise(
        Inizio = min(
          previous_timestamp,
          na.rm = TRUE
        ),
        Fine = max(
          timestamp_local,
          na.rm = TRUE
        ),
        ore_consecutive = as.numeric(
          difftime(
            Fine,
            Inizio,
            units = "hours"
          )
        ),
        .groups = "drop"
      ) |>
      filter(
        is.finite(ore_consecutive),
        ore_consecutive > 1
      ) |>
      arrange(Inizio, cds_name) |>
      transmute(
        Sensore = paste(
          cds_name,
          sensor_description,
          sep = " - "
        ),
        Data = day,
        Inizio,
        Fine,
        `Almeno ore consecutive` = round(
          ore_consecutive,
          2
        )
      )
  })
  
  allarmi_nok_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    valori_giornalieri_modal_nok() |>
      filter(
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      left_join(
        nmn_storico_modal_nok(),
        by = c("cds_name", "sensor_description")
      ) |>
      left_join(
        statistiche_nok_storiche_modal(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK_giornaliero = case_when(
          is.na(daily_value) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ daily_value / NMN
        ),
        Direzione = case_when(
          NOK_giornaliero < limite_nok_inf ~ "Sotto limite",
          NOK_giornaliero > limite_nok_sup ~ "Sopra limite",
          TRUE ~ NA_character_
        )
      ) |>
      filter(
        is.finite(NOK_giornaliero),
        is.finite(limite_nok_inf),
        is.finite(limite_nok_sup),
        NOK_giornaliero < limite_nok_inf |
          NOK_giornaliero > limite_nok_sup
      ) |>
      arrange(day, cds_name) |>
      transmute(
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        Data = day,
        NOK = round(NOK_giornaliero, 3),
        `Limite inferiore` = round(limite_nok_inf, 3),
        `Limite superiore` = round(limite_nok_sup, 3),
        Direzione,
        Descrizione = paste0(
          "NOK ",
          tolower(Direzione),
          if_else(
            gateway_disconnect_affected,
            nota_disconnessione_gateway,
            ""
          )
        )
      )
  })
  
  output$modal_allarmi_attivazioni_panel <- renderUI({
    tabella <- allarmi_attivazioni_modal()
    
    if (nrow(tabella) == 0) {
      return(
        div(
          class = "alarm-empty",
          "Nessuna anomalia nelle attivazioni nel periodo selezionato."
        )
      )
    }
    
    DTOutput("modal_allarmi_attivazioni")
  })
  
  output$modal_allarmi_sensori_aperti_panel <- renderUI({
    tabella <- allarmi_sensori_aperti_modal()
    
    if (nrow(tabella) == 0) {
      return(
        div(
          class = "alarm-empty",
          "Nessun sensore aperto o senza contatto elettrico per oltre 1 ora consecutiva nel periodo selezionato."
        )
      )
    }
    
    DTOutput("modal_allarmi_sensori_aperti")
  })
  
  output$modal_allarmi_nok_panel <- renderUI({
    tabella <- allarmi_nok_modal()
    
    if (nrow(tabella) == 0) {
      return(
        div(
          class = "alarm-empty",
          "Nessun valore NOK anomalo nel periodo selezionato."
        )
      )
    }
    
    DTOutput("modal_allarmi_nok")
  })
  
  output$modal_allarmi_attivazioni <- renderDT({
    allarmi_attivazioni_modal() |>
      mutate(Data = format(Data, "%d-%m-%Y"))
  },
  rownames = FALSE,
  options = list(pageLength = 15, dom = "tip"))
  
  output$modal_allarmi_sensori_aperti <- renderDT({
    allarmi_sensori_aperti_modal() |>
      mutate(
        Data = format(Data, "%d-%m-%Y"),
        Inizio = format(Inizio, "%d-%m-%Y %H:%M:%S", tz = "Europe/Rome"),
        Fine = format(Fine, "%d-%m-%Y %H:%M:%S", tz = "Europe/Rome")
      )
  },
  rownames = FALSE,
  options = list(pageLength = 15, dom = "tip"))
  
  output$modal_allarmi_nok <- renderDT({
    allarmi_nok_modal() |>
      mutate(Data = format(Data, "%d-%m-%Y"))
  },
  rownames = FALSE,
  options = list(pageLength = 15, dom = "tip"))
  
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
    
    # Nelle card il nome del sensore puo' usare tutta la larghezza:
    # niente wrapping forzato in base al numero di sensori selezionati.
    base <- base |>
      mutate(
        etichetta_sensore = factor(
          etichetta_sensore,
          levels = unique(etichetta_sensore)
        )
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
  
  # ---------------------------------------------------------------------
  # Vita sensori: griglia di card.
  # Ogni card mostra:
  # - valore corrente / massimo B10dSAN (attivazioni)
  # - valore corrente / massimo T10d (anni)
  # - percentuale utilizzata e residua
  # - marker sempre visibile anche con percentuali molto piccole
  #
  # Soglie SOLO grafiche:
  # <80% = OK; 80-100% = Attenzione; >100% = Superato.
  # ---------------------------------------------------------------------
  output$modal_tankCards <- renderUI({
    
    tanks <- tank_data_modal()
    
    validate(
      need(nrow(tanks) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    tanks <- tanks |>
      mutate(
        etichetta_sensore = gsub("\n", " ", as.character(etichetta_sensore)),
        percentuale_visuale = pmax(pmin(percentuale, 100), 0),
        residuo = pmax(100 - percentuale, 0)
      )
    
    formatta_vita_numero <- function(x, tipo) {
      if (length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
        return("N/D")
      }
      
      if (identical(tipo, "B10dSAN")) {
        formatC(
          round(x),
          format = "f",
          digits = 0,
          decimal.mark = ",",
          big.mark = "."
        )
      } else {
        formatC(
          x,
          format = "f",
          digits = 1,
          decimal.mark = ",",
          big.mark = "."
        )
      }
    }
    
    formatta_vita_percentuale <- function(x) {
      if (length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
        return("N/D")
      }
      
      # Evita di mostrare 0,00% quando esiste comunque un consumo reale.
      if (x > 0 && x < 0.01) {
        return("<0,01%")
      }
      
      paste0(
        formatC(
          x,
          format = "f",
          digits = 2,
          decimal.mark = ","
        ),
        "%"
      )
    }
    
    classe_vita <- function(percentuale) {
      if (is.na(percentuale) || is.nan(percentuale) || is.infinite(percentuale)) {
        return("")
      }
      if (percentuale > 100) return("over")
      if (percentuale >= 80) return("warning")
      ""
    }
    
    crea_riga_vita <- function(riga) {
      
      pct <- riga$percentuale_visuale
      stato_css <- classe_vita(riga$percentuale)
      
      # La barra mantiene il valore reale. Il pallino ha una posizione minima
      # dell'1% solo per restare visibile con valori come 391 / 2.000.000.
      marker_pct <- if (
        is.na(pct) || is.nan(pct) || is.infinite(pct)
      ) {
        0
      } else {
        min(max(pct, 1), 100)
      }
      
      unita <- if (
        identical(riga$tipo, "B10dSAN")
      ) {
        " attivazioni"
      } else {
        " anni"
      }
      
      valore_testo <- paste0(
        formatta_vita_numero(riga$valore, riga$tipo),
        " / ",
        formatta_vita_numero(riga$massimo, riga$tipo),
        unita
      )
      
      div(
        class = "life-row",
        
        div(
          class = "life-row-header",
          span(riga$tipo, class = "life-metric"),
          span(valore_testo, class = "life-value")
        ),
        
        div(
          class = "life-progress",
          div(
            class = paste("life-progress-fill", stato_css),
            style = sprintf(
              "width: %.6f%%;",
              ifelse(is.na(pct), 0, pct)
            )
          ),
          div(
            class = paste("life-progress-marker", stato_css),
            style = sprintf("left: %.6f%%;", marker_pct)
          )
        ),
        
        div(
          class = "life-row-footer",
          span(
            "Utilizzo: ",
            strong(
              formatta_vita_percentuale(riga$percentuale),
              class = stato_css
            )
          ),
          span(
            "Residuo: ",
            strong(
              formatta_vita_percentuale(riga$residuo),
              class = stato_css
            )
          )
        )
      )
    }
    
    # Mantiene l'ordine naturale gia' prodotto da ordina_naturale()
    # nella costruzione di tank_data_modal().
    ordine_sensori <- unique(tanks$etichetta_sensore)
    
    cards <- lapply(ordine_sensori, function(nome_sensore) {
      
      df <- tanks |>
        filter(etichetta_sensore == nome_sensore) |>
        arrange(match(tipo, c("B10dSAN", "T10d")))
      
      max_pct <- suppressWarnings(max(df$percentuale, na.rm = TRUE))
      if (!is.finite(max_pct)) {
        max_pct <- NA_real_
      }
      
      stato_label <- if (is.na(max_pct)) {
        "N/D"
      } else if (max_pct > 100) {
        "Superato"
      } else if (max_pct >= 80) {
        "Attenzione"
      } else {
        "OK"
      }
      
      stato_class <- if (identical(stato_label, "Superato")) {
        "life-status life-status-over"
      } else if (identical(stato_label, "Attenzione")) {
        "life-status life-status-warning"
      } else {
        "life-status life-status-ok"
      }
      
      div(
        class = "life-card",
        div(
          class = "life-card-header",
          span(nome_sensore, class = "life-card-title"),
          span(stato_label, class = stato_class)
        ),
        lapply(
          seq_len(nrow(df)),
          function(i) crea_riga_vita(df[i, ])
        )
      )
    })
    
    div(class = "life-grid", cards)
  })
  
  # ---------------------------------------------------------------------
  # Dati per il grafico attivazioni.
  # La base e' ORARIA: per ogni giorno considero la macchina accesa dalla
  # prima ora con un segnale fino all'ora dell'ultimo segnale compresa
  # (equivalente a terminare un'ora dopo l'ultimo segnale).
  # Per ogni sensore vengono quindi create anche le ore senza attivazioni.
  # Le granularita' da Giorno ad Anno derivano da questa stessa base.
  # ---------------------------------------------------------------------
  dati_attivazioni_orarie_modal <- reactive({
    
    filtri <- filtri_attivazioni_modal()
    req(length(filtri$sensori) > 0)
    
    sensori_selezionati <- sensori_lookup |>
      filter(
        coupon == filtri$macchina,
        cds_name %in% filtri$sensori
      ) |>
      select(cds_name, sensor_description)
    
    finestre_periodo <- finestre_orarie_macchina |>
      filter(
        coupon == filtri$macchina,
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      select(day, prima_ora, ultima_ora)
    
    validate(
      need(
        nrow(finestre_periodo) > 0 &&
          nrow(sensori_selezionati) > 0,
        "Nessun dato disponibile per i filtri scelti"
      )
    )
    
    ore_macchina <- finestre_periodo |>
      rowwise() |>
      reframe(
        day = day,
        hour = seq(
          from = prima_ora,
          to = ultima_ora,
          by = "hour"
        )
      ) |>
      ungroup()
    
    conteggi_periodo <- dati_attivazioni_orarie_base |>
      filter(
        coupon == filtri$macchina,
        day >= filtri$date[1],
        day <= filtri$date[2],
        cds_name %in% filtri$sensori
      ) |>
      select(
        day,
        hour,
        cds_name,
        sensor_description,
        attivazioni_orarie,
        gateway_disconnect_affected
      )
    
    tidyr::crossing(
      ore_macchina,
      sensori_selezionati
    ) |>
      left_join(
        conteggi_periodo,
        by = c("day", "hour", "cds_name", "sensor_description")
      ) |>
      mutate(
        attivazioni_orarie = coalesce(attivazioni_orarie, 0),
        gateway_disconnect_affected = coalesce(
          gateway_disconnect_affected,
          FALSE
        )
      ) |>
      left_join(
        mediane_attivazioni_orarie |>
          filter(coupon == filtri$macchina) |>
          select(-coupon),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        anomalia_oraria = (
          is.finite(attivazioni_orarie) &
          is.finite(mediana_oraria) &
          mediana_oraria > 0 &
          attivazioni_orarie >= 4 * mediana_oraria
        )
      ) |>
      arrange(hour, cds_name)
  }) |>
    bindCache(
      filtri_attivazioni_modal()$macchina,
      filtri_attivazioni_modal()$date,
      filtri_attivazioni_modal()$sensori
    )
  
  dati_grafico_modal <- reactive({
    
    filtri <- filtri_attivazioni_modal()
    orari <- dati_attivazioni_orarie_modal()
    
    validate(
      need(nrow(orari) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    giornalieri <- orari |>
      group_by(day, cds_name, sensor_description) |>
      summarise(
        attivazioni_giornaliere = sum(attivazioni_orarie, na.rm = TRUE),
        ore_macchina = n_distinct(hour),
        gateway_disconnect_affected = any(
          gateway_disconnect_affected,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    if (identical(filtri$granularita, "Ora")) {
      risultato <- orari |>
        transmute(
          periodo = hour,
          cds_name,
          sensor_description,
          attivazioni = attivazioni_orarie,
          ore_aperte = 1,
          anomalia_oraria,
          mediana_oraria,
          gateway_disconnect_affected,
          etichetta_ore_aperte = "Ora macchina"
        )
    } else {
      risultato <- giornalieri |>
        mutate(
          periodo = periodo_bucket(day, filtri$granularita)
        ) |>
        group_by(periodo, cds_name, sensor_description) |>
        summarise(
          attivazioni = mean(attivazioni_giornaliere, na.rm = TRUE),
          ore_aperte = mean(ore_macchina, na.rm = TRUE),
          gateway_disconnect_affected = any(
            gateway_disconnect_affected,
            na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        mutate(
          anomalia_oraria = FALSE,
          mediana_oraria = NA_real_,
          etichetta_ore_aperte = if (
            identical(filtri$granularita, "Giorno")
          ) {
            "Ore macchina"
          } else {
            "Ore macchina medie/giorno"
          }
        )
    }
    
    risultato |>
      mutate(
        etichetta = cds_name,
        etichetta_completa = paste(
          cds_name,
          sensor_description,
          sep = " - "
        ),
        periodo_label = formatta_periodo_label(
          periodo,
          filtri$granularita
        )
      ) |>
      ordina_naturale() |>
      mutate(
        etichetta = factor(etichetta, levels = unique(etichetta)),
        etichetta_completa = factor(
          etichetta_completa,
          levels = unique(etichetta_completa)
        ),
        periodo_label = factor(
          periodo_label,
          levels = unique(periodo_label[order(periodo)])
        )
      )
  }) |>
    bindCache(filtri_attivazioni_modal())
  
  # CSS del tooltip, condiviso tra main e modal
  tooltip_css <- paste0(
    "background-color:#234A66;",
    "color:#FFFFFF;",
    "border-left:3px solid #EBBD55;",
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
    palette_sensori <- palette_sensori_brand(n_sensori)
    padding_x <- if (n_sensori <= 1) {
      1.60
    } else if (n_sensori == 2) {
      0.80
    } else {
      0.35
    }
    
    ggplot(
      grafico,
      aes(x = etichetta, y = attivazioni, fill = etichetta_completa)
    ) +
      geom_col_interactive(
        aes(
          tooltip = paste0(
            "<b>", etichetta_completa, "</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            if_else(
              filtri_attivazioni_modal()$granularita == "Ora",
              "Attivazioni nell'ora: ",
              "Attivazioni medie giornaliere: "
            ),
            scales::label_number(
              accuracy = 0.1,
              big.mark = "."
            )(attivazioni)
          ),
          data_id = paste(etichetta_completa, periodo_label, sep = "__")
        ),
        width = 0.62
      ) +
      scale_x_discrete(
        expand = expansion(add = padding_x)
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
        panel.spacing.x = grid::unit(10, "pt"),
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
    palette_sensori <- palette_sensori_brand(n_sensori)
    
    tooltip_punti <- paste0(
      "<b>", grafico$etichetta_completa, "</b><br/>",
      "Periodo: ", grafico$periodo_label, "<br/>",
      "Attivazioni: ",
      scales::label_number(
        accuracy = 1,
        big.mark = "."
      )(grafico$attivazioni)
    )
    
    if (identical(granularita, "Ora")) {
      return(
        ggplot(
          grafico |>
            mutate(
              giorno_linea = as.Date(
                periodo,
                tz = "Europe/Rome"
              )
            ),
          aes(
            x = periodo,
            y = attivazioni,
            color = etichetta_completa,
            group = interaction(
              etichetta_completa,
              giorno_linea
            )
          )
        ) +
          geom_line(linewidth = 0.8) +
          geom_point_interactive(
            aes(
              tooltip = tooltip_punti,
              data_id = paste(
                etichetta_completa,
                periodo_label,
                sep = "__"
              )
            ),
            size = 1.8
          ) +
          scale_x_datetime(
            breaks = sort(unique(grafico$periodo)),
            labels = function(x) {
              format(
                x,
                "%d-%m %H:00",
                tz = "Europe/Rome"
              )
            },
            minor_breaks = NULL,
            timezone = "Europe/Rome"
          ) +
          scale_y_continuous(
            breaks = scales::pretty_breaks(n = 8),
            expand = expansion(mult = c(0, 0.04))
          ) +
          scale_color_manual(
            values = palette_sensori,
            name = "Sensore"
          ) +
          labs(x = NULL, y = "Attivazioni") +
          theme_minimal(base_size = 13) +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(
              color = "#B8C4CC",
              linewidth = 0.5
            ),
            axis.text.x = element_text(
              size = 10,
              angle = 90,
              face = "bold",
              hjust = 1,
              vjust = 0.5
            ),
            axis.text.y = element_text(size = 11),
            plot.title = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 13),
            legend.text = element_text(size = 13)
          )
      )
    }
    
    breaks_periodo <- calcola_breaks_periodo(grafico$periodo)
    
    spline_att <- interpola_spline(
      grafico,
      "periodo",
      "attivazioni",
      "etichetta_completa"
    ) |>
      mutate(
        etichetta_completa = factor(
          etichetta_completa,
          levels = levels(grafico$etichetta_completa)
        )
      )
    
    ggplot() +
      geom_line(
        data = spline_att,
        aes(
          x = periodo,
          y = attivazioni,
          color = etichetta_completa
        ),
        linewidth = 1,
        na.rm = FALSE
      ) +
      geom_point_interactive(
        data = grafico,
        aes(
          x = periodo,
          y = attivazioni,
          color = etichetta_completa,
          tooltip = tooltip_punti,
          data_id = paste(
            etichetta_completa,
            periodo_label,
            sep = "__"
          )
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
        panel.grid.major.x = element_line(
          color = "#B8C4CC",
          linewidth = 0.5
        ),
        axis.text.x = element_text(
          size = 12,
          angle = 90,
          face = "bold",
          hjust = 1,
          vjust = 0.5
        ),
        axis.ticks.x = element_line(
          color = "#6B7380",
          linewidth = 0.5
        ),
        axis.text.y = element_text(size = 11),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)
      )
  }
  
  # Modal (size = "xl", ~95vw): larghezza misurata dal JS dopo l'apertura.
  # In modalita' oraria il contenitore interno ha una larghezza reale in pixel:
  # il grafico mantiene quindi la propria scala e lo scorrimento avviene nel
  # contenitore esterno, senza comprimere l'SVG per farlo stare a schermo.
  output$modal_activationPlot_container <- renderUI({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    grafico <- dati_grafico_modal()
    n_periodi <- dplyr::n_distinct(grafico$periodo)
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    
    granularita <- filtri_attivazioni_modal()$granularita
    
    plot_px <- if (identical(granularita, "Ora")) {
      # La vista oraria resta volutamente larga e scrollabile.
      max(
        w_px,
        n_periodi * max(90, n_sensori * 16)
      )
    } else if (identical(granularita, "Giorno")) {
      # La vista giornaliera puo' avere molti periodi, ma non deve diventare
      # una striscia enorme: al massimo circa due larghezze del modal.
      max(
        w_px,
        min(w_px * 2, n_periodi * 120)
      )
    } else {
      # Settimana / Mese / Trimestre / Anno: tutto compatto nel modal.
      w_px
    }
    
    div(
      class = "activation-scroll-inner",
      style = paste0(
        "width:", round(plot_px), "px;",
        "min-width:", round(plot_px), "px;"
      ),
      girafeOutput(
        "modal_activationPlot",
        width = "100%",
        height = "380px"
      )
    )
  })
  
  output$modal_activationTrendPlot_container <- renderUI({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    grafico <- dati_grafico_modal()
    n_periodi <- dplyr::n_distinct(grafico$periodo)
    
    plot_px <- if (
      identical(filtri_attivazioni_modal()$granularita, "Ora")
    ) {
      max(w_px, n_periodi * 55)
    } else {
      w_px
    }
    
    div(
      class = "activation-scroll-inner",
      style = paste0(
        "width:", round(plot_px), "px;",
        "min-width:", round(plot_px), "px;"
      ),
      girafeOutput(
        "modal_activationTrendPlot",
        width = "100%",
        height = "380px"
      )
    )
  })
  
  output$modal_activationPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    grafico <- dati_grafico_modal()
    n_periodi <- dplyr::n_distinct(grafico$periodo)
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    
    granularita <- filtri_attivazioni_modal()$granularita
    
    plot_px <- if (identical(granularita, "Ora")) {
      # La vista oraria resta volutamente larga e scrollabile.
      max(
        w_px,
        n_periodi * max(90, n_sensori * 16)
      )
    } else if (identical(granularita, "Giorno")) {
      # La vista giornaliera puo' avere molti periodi, ma non deve diventare
      # una striscia enorme: al massimo circa due larghezze del modal.
      max(
        w_px,
        min(w_px * 2, n_periodi * 120)
      )
    } else {
      # Settimana / Mese / Trimestre / Anno: tutto compatto nel modal.
      w_px
    }
    
    girafe(
      ggobj      = render_activation_bar_gg(),
      width_svg  = plot_px / 72,
      height_svg = 380 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
  output$modal_activationTrendPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    grafico <- dati_grafico_modal()
    n_periodi <- dplyr::n_distinct(grafico$periodo)
    
    plot_px <- if (
      identical(filtri_attivazioni_modal()$granularita, "Ora")
    ) {
      max(w_px, n_periodi * 55)
    } else {
      w_px
    }
    
    girafe(
      ggobj      = render_activation_trend_gg(),
      width_svg  = plot_px / 72,
      height_svg = 380 / 72,
      options    = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
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
  
  # ---------------------------------------------------------------------
  # Utilizzo macchina nel tempo.
  # Il raggruppamento vale solo per questo grafico.
  # ---------------------------------------------------------------------
  nok_temporale_confronto_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    durata_giorni <- as.integer(
      filtri$date[2] - filtri$date[1]
    ) + 1L
    
    periodo_precedente_fine <- filtri$date[1] - 1L
    periodo_precedente_inizio <- filtri$date[1] - durata_giorni
    
    base <- valori_giornalieri_modal_nok() |>

      left_join(
        nmn_storico_modal_nok(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK_giornaliero = case_when(
          is.na(daily_value) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ daily_value / NMN
        ),
        confronto = case_when(
          day >= periodo_precedente_inizio &
            day <= periodo_precedente_fine ~ "Periodo precedente",
          day >= filtri$date[1] &
            day <= filtri$date[2] ~ "Periodo selezionato",
          TRUE ~ NA_character_
        )
      ) |>
      filter(
        !is.na(confronto),
        is.finite(NOK_giornaliero)
      ) |>
      mutate(
        confronto = factor(
          confronto,
          levels = c("Periodo precedente", "Periodo selezionato")
        ),
        periodo = as.Date(
          periodo_bucket(day, filtri$granularita)
        )
      )
    
    per_sensore <- base |>
      group_by(
        confronto,
        periodo,
        cds_name,
        sensor_description
      ) |>
      summarise(
        NOK = mean(NOK_giornaliero, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(is.finite(NOK)) |>
      mutate(
        etichetta_sensore = paste(
          cds_name,
          sensor_description,
          sep = " - "
        ),
        periodo_label = formatta_periodo_label(
          periodo,
          filtri$granularita
        )
      )
    
    media_macchina <- per_sensore |>
      group_by(confronto, periodo) |>
      summarise(
        NOK = mean(NOK, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        etichetta_sensore = "Media macchina",
        periodo_label = formatta_periodo_label(
          periodo,
          filtri$granularita
        )
      )
    
    list(
      sensori = per_sensore,
      media_macchina = media_macchina
    )
  }) |>
    bindCache(
      filtri_nok_modal()$macchina,
      filtri_nok_modal()$date,
      filtri_nok_modal()$sensori,
      filtri_nok_modal()$granularita
    )
  
  # ---------------------------------------------------------------------
  # Utilizzo macchina dal NOK.
  # Usa gli stessi dati giornalieri gia' filtrati per il calcolo del NOK.
  #
  # U_sensore  = sqrt(media(NOK)^2 + varianza(NOK))
  # U_macchina = media(U_sensore)
  # ---------------------------------------------------------------------
  utilizzo_nok_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    base_completa <- valori_giornalieri_modal_nok() |>

      left_join(
        nmn_storico_modal_nok(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK_giornaliero = case_when(
          is.na(daily_value) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ daily_value / NMN
        )
      ) |>
      filter(is.finite(NOK_giornaliero))
    
    # Utilizzo del periodo selezionato: invariato.
    base <- base_completa |>
      filter(
        day >= filtri$date[1],
        day <= filtri$date[2]
      )
    
    per_sensore <- base |>
      group_by(cds_name, sensor_description) |>
      summarise(
        n_osservazioni = n(),
        media_nok = mean(NOK_giornaliero, na.rm = TRUE),
        varianza_nok = if (n() >= 2) {
          var(NOK_giornaliero, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        U_sensore = sqrt(media_nok^2 + varianza_nok),
        sd_nok = sqrt(varianza_nok),
        banda_min = pmax(0, media_nok - sd_nok),
        banda_max = media_nok + sd_nok,
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")
      ) |>
      ordina_naturale()
    
    durata_giorni <- as.integer(
      filtri$date[2] - filtri$date[1]
    ) + 1L
    
    periodo_precedente_fine <- filtri$date[1] - 1L
    periodo_precedente_inizio <- filtri$date[1] - durata_giorni
    
    per_sensore_confronto <- base_completa |>
      mutate(
        confronto = case_when(
          day >= periodo_precedente_inizio &
            day <= periodo_precedente_fine ~ "Periodo precedente",
          day >= filtri$date[1] &
            day <= filtri$date[2] ~ "Periodo selezionato",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(confronto)) |>
      mutate(
        confronto = factor(
          confronto,
          levels = c("Periodo precedente", "Periodo selezionato")
        )
      ) |>
      group_by(
        confronto,
        cds_name,
        sensor_description
      ) |>
      summarise(
        n_osservazioni = n(),
        media_nok = mean(NOK_giornaliero, na.rm = TRUE),
        varianza_nok = if (n() >= 2) {
          var(NOK_giornaliero, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        U_sensore = sqrt(media_nok^2 + varianza_nok),
        sd_nok = sqrt(varianza_nok),
        banda_min = pmax(0, media_nok - sd_nok),
        banda_max = media_nok + sd_nok,
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")
      ) |>
      ordina_naturale()
    
    valori_u <- per_sensore$U_sensore[
      is.finite(per_sensore$U_sensore)
    ]
    
    U_macchina <- if (length(valori_u) > 0) {
      mean(valori_u)
    } else {
      NA_real_
    }
    
    # Soglia P90 su finestre della stessa durata del periodo selezionato.
    # Le finestre partono ogni 7 giorni.
    # Normalmente vengono usate solo finestre completamente precedenti
    # al periodo selezionato. Se il periodo selezionato copre oltre il 50%
    # dello storico disponibile, oppure non esiste alcuna finestra completa
    # precedente per mancanza di giorni, vengono usate tutte le finestre
    # disponibili nello storico, escludendo comunque quella identica
    # al periodo corrente.
    prima_data_storica <- min(base_completa$day, na.rm = TRUE)
    ultima_data_storica <- max(base_completa$day, na.rm = TRUE)
    
    durata_giorni <- as.integer(
      filtri$date[2] - filtri$date[1]
    ) + 1L
    
    giorni_storico_totale <- as.integer(
      ultima_data_storica - prima_data_storica
    ) + 1L
    
    quota_storico_selezionata <- if (
      is.finite(giorni_storico_totale) &&
      giorni_storico_totale > 0
    ) {
      durata_giorni / giorni_storico_totale
    } else {
      0
    }
    
    manca_finestra_precedente <- (
      filtri$date[1] - durata_giorni
    ) < prima_data_storica
    
    includi_periodo_nella_soglia <- (
      quota_storico_selezionata > 0.50 ||
      manca_finestra_precedente
    )
    
    calcola_u_finestra <- function(inizio_finestra) {
      fine_finestra <- inizio_finestra + durata_giorni - 1L
      
      dati_finestra <- base_completa |>
        filter(
          day >= inizio_finestra,
          day <= fine_finestra
        ) |>
        group_by(cds_name, sensor_description) |>
        summarise(
          media_nok = mean(NOK_giornaliero, na.rm = TRUE),
          varianza_nok = if (n() >= 2) {
            var(NOK_giornaliero, na.rm = TRUE)
          } else {
            NA_real_
          },
          .groups = "drop"
        ) |>
        mutate(
          U_sensore = sqrt(media_nok^2 + varianza_nok)
        ) |>
        filter(is.finite(U_sensore))
      
      if (nrow(dati_finestra) == 0) {
        return(NA_real_)
      }
      
      mean(dati_finestra$U_sensore, na.rm = TRUE)
    }
    
    ultima_partenza_precedente <- filtri$date[1] - durata_giorni
    ultima_partenza_disponibile <- ultima_data_storica - durata_giorni + 1L
    
    ultima_partenza <- if (includi_periodo_nella_soglia) {
      ultima_partenza_disponibile
    } else {
      ultima_partenza_precedente
    }
    
    if (
      !is.finite(as.numeric(prima_data_storica)) ||
      !is.finite(as.numeric(ultima_partenza)) ||
      ultima_partenza < prima_data_storica
    ) {
      partenze_storiche <- as.Date(character(0))
      U_storici <- numeric(0)
    } else {
      partenze_storiche <- seq.Date(
        from = prima_data_storica,
        to = ultima_partenza,
        by = "7 days"
      )
      
      if (includi_periodo_nella_soglia) {
        partenze_storiche <- sort(unique(c(
          partenze_storiche,
          filtri$date[1]
        )))
      } else {
        partenze_storiche <- partenze_storiche[
          partenze_storiche != filtri$date[1]
        ]
      }
      
      U_storici_raw <- vapply(
        partenze_storiche,
        calcola_u_finestra,
        numeric(1)
      )
      
      storico_utilizzo <- tibble(
        inizio_finestra = partenze_storiche,
        U_macchina = U_storici_raw
      ) |>
        filter(is.finite(U_macchina))
      
      U_storici <- storico_utilizzo$U_macchina
    }
    
    if (!exists("storico_utilizzo")) {
      storico_utilizzo <- tibble(
        inizio_finestra = as.Date(character(0)),
        U_macchina = numeric(0)
      )
    }
    
    P10_utilizzo <- if (length(U_storici) > 0) {
      as.numeric(
        stats::quantile(
          U_storici,
          probs = 0.10,
          na.rm = TRUE,
          names = FALSE,
          type = 7
        )
      )
    } else {
      NA_real_
    }
    
    P90_utilizzo <- if (length(U_storici) > 0) {
      as.numeric(
        stats::quantile(
          U_storici,
          probs = 0.90,
          na.rm = TRUE,
          names = FALSE,
          type = 7
        )
      )
    } else {
      NA_real_
    }
    
    stato_utilizzo <- case_when(
      !is.finite(U_macchina) |
        !is.finite(P10_utilizzo) |
        !is.finite(P90_utilizzo) ~ "N/D",
      U_macchina < P10_utilizzo ~ "Basso",
      U_macchina > P90_utilizzo ~ "Elevato",
      TRUE ~ "Normale"
    )
    
    # Utilizzo macchina per settimana o mese, in base alla scelta utente.
    granularita_utilizzo <- granularita_utilizzo_corrente()
    
    storico_utilizzo_grafico <- base_completa |>
      mutate(
        inizio_finestra = as.Date(
          if (identical(granularita_utilizzo, "Mese")) {
            lubridate::floor_date(day, "month")
          } else {
            lubridate::floor_date(
              day,
              "week",
              week_start = 1
            )
          }
        )
      ) |>
      group_by(
        inizio_finestra,
        cds_name,
        sensor_description
      ) |>
      summarise(
        media_nok = mean(NOK_giornaliero, na.rm = TRUE),
        varianza_nok = if (n() >= 2) {
          var(NOK_giornaliero, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(
        U_sensore = sqrt(media_nok^2 + varianza_nok)
      ) |>
      filter(is.finite(U_sensore)) |>
      group_by(inizio_finestra) |>
      summarise(
        U_macchina = mean(U_sensore, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        fine_finestra = if (identical(granularita_utilizzo, "Mese")) {
          as.Date(
            lubridate::ceiling_date(
              inizio_finestra,
              "month"
            ) - lubridate::days(1)
          )
        } else {
          inizio_finestra + 6L
        },
        tipo = if_else(
          fine_finestra >= filtri$date[1] &
            inizio_finestra <= filtri$date[2],
          "Periodo selezionato",
          "Riferimento"
        )
      ) |>
      filter(is.finite(U_macchina)) |>
      arrange(inizio_finestra)
    
    # P10/P90 coerenti con la granularita' scelta.
    # Si usano normalmente solo i periodi completi precedenti alla selezione.
    # Se la selezione copre oltre il 50% dello storico o ci sono meno di due
    # periodi precedenti, si usa tutto lo storico disponibile.
    periodi_precedenti <- storico_utilizzo_grafico |>
      filter(fine_finestra < filtri$date[1])
    
    usa_tutti_periodi <- (
      quota_storico_selezionata > 0.50 ||
      nrow(periodi_precedenti) < 2
    )
    
    riferimento_periodo <- if (usa_tutti_periodi) {
      storico_utilizzo_grafico
    } else {
      periodi_precedenti
    }
    
    valori_periodo_rif <- riferimento_periodo$U_macchina[
      is.finite(riferimento_periodo$U_macchina)
    ]
    
    P10_periodo <- if (length(valori_periodo_rif) > 0) {
      as.numeric(
        stats::quantile(
          valori_periodo_rif,
          probs = 0.10,
          na.rm = TRUE,
          names = FALSE,
          type = 7
        )
      )
    } else {
      NA_real_
    }
    
    P90_periodo <- if (length(valori_periodo_rif) > 0) {
      as.numeric(
        stats::quantile(
          valori_periodo_rif,
          probs = 0.90,
          na.rm = TRUE,
          names = FALSE,
          type = 7
        )
      )
    } else {
      NA_real_
    }
    
    storico_utilizzo_grafico <- storico_utilizzo_grafico |>
      mutate(
        stato = case_when(
          tipo == "Riferimento" ~ "Storico",
          !is.finite(P10_periodo) |
            !is.finite(P90_periodo) ~ "N/D",
          U_macchina < P10_periodo ~ "Basso",
          U_macchina > P90_periodo ~ "Elevato",
          TRUE ~ "Normale"
        )
      )
    
    list(
      per_sensore = per_sensore,
      per_sensore_confronto = per_sensore_confronto,
      U_macchina = U_macchina,
      P10_utilizzo = P10_utilizzo,
      P90_utilizzo = P90_utilizzo,
      stato_utilizzo = stato_utilizzo,
      storico_utilizzo_grafico = storico_utilizzo_grafico,
      granularita_utilizzo = granularita_utilizzo,
      P10_periodo = P10_periodo,
      P90_periodo = P90_periodo,
      n_finestre_storiche = length(U_storici),
      n_settimane_storiche = length(U_storici),
      durata_finestra_giorni = durata_giorni,
      quota_storico_selezionata = quota_storico_selezionata,
      manca_finestra_precedente = manca_finestra_precedente,
      includi_periodo_nella_soglia = includi_periodo_nella_soglia
    )
  }) |>
    bindCache(
      filtri_nok_modal()$macchina,
      filtri_nok_modal()$date,
      filtri_nok_modal()$sensori,
      granularita_utilizzo_corrente()
    )
  
  output$modal_utilizzo_macchina <- renderUI({
    
    utilizzo <- utilizzo_nok_modal()
    granularita <- utilizzo$granularita_utilizzo
    
    periodi <- utilizzo$storico_utilizzo_grafico |>
      filter(tipo == "Periodo selezionato") |>
      arrange(inizio_finestra)
    
    soglia_bassa <- if (
      length(utilizzo$P10_periodo) == 0 ||
      !is.finite(utilizzo$P10_periodo)
    ) {
      "N/D"
    } else {
      formatC(
        utilizzo$P10_periodo,
        format = "f",
        digits = 2,
        decimal.mark = ","
      )
    }
    
    soglia_alta <- if (
      length(utilizzo$P90_periodo) == 0 ||
      !is.finite(utilizzo$P90_periodo)
    ) {
      "N/D"
    } else {
      formatC(
        utilizzo$P90_periodo,
        format = "f",
        digits = 2,
        decimal.mark = ","
      )
    }
    
    cards_periodo <- lapply(seq_len(nrow(periodi)), function(i) {
      riga <- periodi[i, ]
      
      colore <- switch(
        as.character(riga$stato),
        "Normale" = "#19764A",
        "Elevato" = "#A65A52",
        "Basso" = "#58758F",
        "#5F6F7F"
      )
      
      sfondo <- switch(
        as.character(riga$stato),
        "Normale" = "#F2FBF6",
        "Elevato" = "#FBF3F2",
        "Basso" = "#F2F6FA",
        "#F4F8FB"
      )
      
      etichetta_periodo <- if (
        identical(granularita, "Mese")
      ) {
        paste0(
          "Mese ",
          format(riga$inizio_finestra, "%m-%Y")
        )
      } else {
        paste0(
          "Sett. ",
          format(riga$inizio_finestra, "%d-%m")
        )
      }
      
      div(
        style = paste0(
          "min-width:105px;",
          "padding:7px 9px;",
          "border:1px solid ", colore, ";",
          "border-radius:8px;",
          "background:", sfondo, ";",
          "text-align:center;"
        ),
        div(
          etichetta_periodo,
          style = "font-size:11px;font-weight:700;color:#4A4A4A;"
        ),
        div(
          formatC(
            riga$U_macchina,
            format = "f",
            digits = 2,
            decimal.mark = ","
          ),
          style = paste0(
            "font-size:22px;",
            "font-weight:800;",
            "line-height:1.2;",
            "color:", colore, ";"
          )
        ),
        div(
          riga$stato,
          style = paste0(
            "font-size:10px;",
            "font-weight:800;",
            "text-transform:uppercase;",
            "color:", colore, ";"
          )
        )
      )
    })
    
    tagList(
      div(
        style = paste0(
          "display:flex;",
          "align-items:flex-start;",
          "gap:8px;",
          "overflow-x:auto;",
          "padding-bottom:4px;"
        ),
        cards_periodo
      ),
      div(
        style = "margin-top:10px;",
        actionButton(
          "modal_btn_storico_utilizzo_nok",
          "Storico utilizzo",
          class = "btn-sm btn-default"
        )
      ),
      div(
        "Indice dell’intensità di utilizzo dei sensori. Valori alti indicano uno stress della macchina più elevato o una discrepanza nell’utilizzo rispetto allo storico. Valori bassi indicano uno stress della macchina meno elevato rispetto allo storico.",
        style = "margin-top:7px;font-size:13px;color:#5F6F7F;"
      ),
      div(
        paste0(
          "Intervallo normale ",
          if (
            identical(granularita, "Mese")
          ) "mensile" else "settimanale",
          ": ",
          soglia_bassa,
          " – ",
          soglia_alta
        ),
        style = "margin-top:4px;font-size:12px;color:#718096;"
      )
    )
  })
  
  render_storico_utilizzo_nok_gg <- function() {
    
    utilizzo <- utilizzo_nok_modal()
    storico <- utilizzo$storico_utilizzo_grafico
    granularita <- utilizzo$granularita_utilizzo
    
    validate(
      need(
        nrow(storico) > 0,
        "Nessun dato storico disponibile per l'utilizzo"
      )
    )
    
    selezionato <- storico |>
      filter(tipo == "Periodo selezionato")
    
    validate(
      need(
        nrow(selezionato) > 0,
        "Nessun periodo disponibile nella selezione"
      )
    )
    
    inizio_area <- min(selezionato$inizio_finestra, na.rm = TRUE)
    fine_area <- max(selezionato$fine_finestra, na.rm = TRUE)
    
    storico <- storico |>
      mutate(
        colore_punto = case_when(
          tipo == "Riferimento" ~ "Storico",
          stato == "Normale" ~ "Normale",
          stato == "Elevato" ~ "Elevato",
          stato == "Basso" ~ "Basso",
          TRUE ~ "N/D"
        ),
        etichetta_periodo = if (
          identical(granularita, "Mese")
        ) {
          format(inizio_finestra, "%m-%Y")
        } else {
          format(inizio_finestra, "%d-%m-%Y")
        },
        tooltip_utilizzo = paste0(
          "<b>",
          ifelse(
            tipo == "Periodo selezionato",
            paste0(
              if (
                identical(granularita, "Mese")
              ) "Mese: " else "Settimana: ",
              stato
            ),
            if (
              identical(granularita, "Mese")
            ) "Mese storico" else "Settimana storica"
          ),
          "</b><br/>",
          if (
            identical(granularita, "Mese")
          ) "Periodo: " else "Settimana dal: ",
          etichetta_periodo,
          "<br/>",
          "Utilizzo macchina: ",
          round(U_macchina, 3)
        )
      )
    
    ggplot(
      storico,
      aes(x = inizio_finestra, y = U_macchina)
    ) +
      geom_rect(
        aes(
          xmin = inizio_area,
          xmax = fine_area,
          ymin = -Inf,
          ymax = Inf
        ),
        inherit.aes = FALSE,
        fill = "#F6F1D8",
        alpha = 0.75
      ) +
      geom_line(
        color = "#B7BEC5",
        linewidth = 0.9
      ) +
      geom_segment(
        data = tibble(
          soglia = c(
            utilizzo$P10_periodo,
            utilizzo$P90_periodo
          )
        ) |>
          filter(is.finite(soglia)),
        aes(
          x = inizio_area,
          xend = fine_area,
          y = soglia,
          yend = soglia,
          linetype = "Limiti normali"
        ),
        inherit.aes = FALSE,
        color = "#79A884",
        linewidth = 0.9
      ) +
      geom_point_interactive(
        aes(
          color = colore_punto,
          tooltip = tooltip_utilizzo,
          data_id = paste(
            tipo,
            inizio_finestra,
            sep = "__"
          )
        ),
        size = 3.2
      ) +
      scale_color_manual(
        values = c(
          "Storico" = "#B7BEC5",
          "Normale" = "#19764A",
          "Elevato" = "#A65A52",
          "Basso" = "#58758F",
          "N/D" = "#6E7781"
        ),
        breaks = c(
          "Storico",
          "Normale",
          "Elevato",
          "Basso"
        ),
        name = NULL
      ) +
      scale_linetype_manual(
        values = c(
          "Limiti normali" = "dashed"
        ),
        name = NULL
      ) +
      scale_x_date(
        breaks = storico$inizio_finestra,
        labels = function(x) {
          if (identical(granularita, "Mese")) {
            format(
              as.Date(x, origin = "1970-01-01"),
              "%m-%Y"
            )
          } else {
            format(
              as.Date(x, origin = "1970-01-01"),
              "%d-%m-%Y"
            )
          }
        }
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 6),
        labels = scales::label_number(
          accuracy = 0.1,
          decimal.mark = ","
        ),
        expand = expansion(mult = c(0.03, 0.06))
      ) +
      labs(
        x = NULL,
        y = "Utilizzo macchina"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(
          color = "#E1E6EA",
          linewidth = 0.4
        ),
        axis.text.x = element_text(
          size = 9,
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.justification = "left"
      )
  }
  
  output$modal_storicoUtilizzoPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    girafe(
      ggobj = render_storico_utilizzo_nok_gg(),
      width_svg = w_px / 72,
      height_svg = 260 / 72,
      options = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.85;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
  render_utilizzo_nok_gg <- function() {
    
    utilizzo <- utilizzo_nok_modal()
    profilo <- utilizzo$per_sensore_confronto |>
      filter(
        is.finite(media_nok),
        is.finite(varianza_nok)
      )
    
    validate(
      need(
        nrow(profilo) > 0,
        "Dati insufficienti per confrontare periodo precedente e periodo selezionato"
      )
    )
    
    sensori_ordinati <- profilo |>
      distinct(cds_name, sensor_description, etichetta_sensore) |>
      ordina_naturale() |>
      pull(etichetta_sensore)
    
    profilo <- profilo |>
      mutate(
        x = match(etichetta_sensore, sensori_ordinati),
        tooltip_utilizzo = paste0(
          "<b>", confronto, "</b><br/>",
          "<b>", etichetta_sensore, "</b><br/>",
          "Media NOK: ", round(media_nok, 4), "<br/>",
          "Varianza NOK: ", round(varianza_nok, 4), "<br/>",
          "Utilizzo sensore: ", round(U_sensore, 4)
        )
      )
    
    ggplot(profilo, aes(x = x)) +
      geom_ribbon(
        aes(ymin = banda_min, ymax = banda_max),
        fill = "#5F748C",
        alpha = 0.22
      ) +
      geom_line(
        aes(y = media_nok),
        color = "#234A66",
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(
          y = media_nok,
          tooltip = tooltip_utilizzo,
          data_id = paste(confronto, etichetta_sensore, sep = "__")
        ),
        color = "#234A66",
        size = 2.6
      ) +
      facet_grid(
        cols = vars(confronto),
        scales = "free_x",
        space = "free_x"
      ) +
      scale_x_continuous(
        breaks = seq_along(sensori_ordinati),
        labels = sensori_ordinati
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0.02, 0.06))
      ) +
      labs(
        x = NULL,
        y = "NOK medio"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(
          fill = "#F4F5F6",
          color = "#CDD2D6"
        ),
        strip.text = element_text(
          size = 13,
          face = "bold",
          color = "#234A66"
        ),
        axis.text.x = element_text(
          size = 10,
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        axis.text.y = element_text(size = 11)
      )
  }
  
  output$modal_utilizzoNokPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    girafe(
      ggobj = render_utilizzo_nok_gg(),
      width_svg = w_px / 72,
      height_svg = 360 / 72,
      options = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
  render_nok_tempo_gg <- function() {
    
    andamento <- nok_temporale_confronto_modal()
    sensori <- andamento$sensori
    media_macchina <- andamento$media_macchina
    granularita <- filtri_nok_modal()$granularita
    
    validate(
      need(
        nrow(sensori) > 0,
        "Nessun dato NOK disponibile per il periodo selezionato"
      )
    )
    
    ggplot() +
      geom_line(
        data = sensori,
        aes(
          x = periodo,
          y = NOK,
          group = interaction(confronto, etichetta_sensore)
        ),
        color = "#C7CDD3",
        linewidth = 0.75,
        alpha = 0.75
      ) +
      geom_point_interactive(
        data = sensori,
        aes(
          x = periodo,
          y = NOK,
          tooltip = paste0(
            "<b>", etichetta_sensore, "</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            "NOK: ", round(NOK, 3)
          ),
          data_id = paste(
            confronto,
            etichetta_sensore,
            periodo,
            sep = "__"
          )
        ),
        color = "#C7CDD3",
        size = 1.6,
        alpha = 0.75
      ) +
      geom_line(
        data = media_macchina,
        aes(
          x = periodo,
          y = NOK,
          group = confronto
        ),
        color = "#234A66",
        linewidth = 1.5
      ) +
      geom_point_interactive(
        data = media_macchina,
        aes(
          x = periodo,
          y = NOK,
          tooltip = paste0(
            "<b>Media macchina</b><br/>",
            "Periodo: ", periodo_label, "<br/>",
            "NOK medio: ", round(NOK, 3)
          ),
          data_id = paste(
            confronto,
            "media_macchina",
            periodo,
            sep = "__"
          )
        ),
        color = "#234A66",
        size = 2.8
      ) +
      geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "#9AA5B1",
        linewidth = 0.7
      ) +
      facet_grid(
        cols = vars(confronto),
        scales = "free_x",
        space = "free_x"
      ) +
      scale_x_date(
        breaks = function(limits) {
          periodi <- sort(unique(sensori$periodo))
          periodi[
            periodi >= as.Date(limits[1], origin = "1970-01-01") &
            periodi <= as.Date(limits[2], origin = "1970-01-01")
          ]
        },
        labels = function(x) {
          formatta_periodo_label(
            as.Date(x, origin = "1970-01-01"),
            granularita
          )
        }
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 7),
        labels = scales::label_number(
          accuracy = 0.1,
          decimal.mark = ","
        ),
        expand = expansion(mult = c(0.03, 0.06))
      ) +
      labs(
        x = NULL,
        y = "NOK"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(
          color = "#D7DDE2",
          linewidth = 0.4
        ),
        strip.background = element_rect(
          fill = "#F4F5F6",
          color = "#CDD2D6"
        ),
        strip.text = element_text(
          size = 13,
          face = "bold",
          color = "#234A66"
        ),
        axis.text.x = element_text(
          size = 10,
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        axis.text.y = element_text(size = 11)
      )
  }
  
  output$modal_utilizzoTempoPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    girafe(
      ggobj = render_nok_tempo_gg(),
      width_svg = w_px / 72,
      height_svg = 360 / 72,
      options = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
      )
    )
  })
  
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
    palette_sensori <- palette_sensori_brand(n_sensori)
    
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
        opts_hover(css = "opacity:0.8;cursor:pointer;"),
        opts_toolbar(
          hidden = c("selection", "zoom", "misc")
        )
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
        Attivazioni = attivazioni,
        Anomalia = if_else(anomalia_oraria, "SI", "")
      ) |>
      arrange(Periodo, Sensore)
  }, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
  
  output$modal_tabella_dati_trend <- renderDT({
    dati_grafico_modal() |>
      transmute(
        Sensore     = as.character(etichetta_completa),
        Periodo     = as.character(periodo_label),
        Attivazioni = attivazioni,
        Anomalia = if_else(anomalia_oraria, "SI", "")
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
  
  output$modal_tabella_outlier_nok <- renderDT({
    
    esclusi <- allarmi_attivazioni_modal() |>
      mutate(Data = format(Data, "%d-%m-%Y"))
    
    validate(
      need(
        nrow(esclusi) > 0,
        "Nessuna attivazione anomala nel periodo selezionato."
      )
    )
    
    esclusi
  }, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
  
  output$modal_tabella_dati_profilo_nok <- renderDT({
    
    utilizzo_nok_modal()$per_sensore_confronto |>
      transmute(
        Confronto = as.character(confronto),
        Sensore = etichetta_sensore,
        `Media NOK` = round(media_nok, 3),
        `Varianza NOK` = round(varianza_nok, 3),
        `Utilizzo sensore` = round(U_sensore, 3)
      ) |>
      arrange(Confronto, Sensore)
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
            NOK = round(NOK, 3)
          ),
        andamento$media_macchina |>
          transmute(
            Confronto = as.character(confronto),
            Periodo = periodo_label,
            Serie = "Media macchina",
            NOK = round(NOK, 3)
          )
      ) |>
        arrange(Confronto, Periodo, Serie)
    }
  },
  rownames = FALSE,
  options = list(pageLength = 25, dom = "tip"))
}

shinyApp(ui, server)