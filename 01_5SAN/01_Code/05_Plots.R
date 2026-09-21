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

# Le immagini non sono nella cartella www: le espongo a Shiny con un
# resource path dedicato. Se la cartella non esiste, l'app continua comunque
# a funzionare e nella Home non viene mostrato alcuno schema.
cds_images_dir <- here("00_Data", "02_CdS")
if (dir.exists(cds_images_dir)) {
  addResourcePath("cds-images", cds_images_dir)
}

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
data_start_default <- max(data_min, data_max - 13)

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
        color: #24364B;
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
        border-color: #7FA6C9;
      }
      .card-home .card-icona {
        font-size: 20px;
        color: #7FA6C9;
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
      .home-stage {
        position: relative;
        width: 100%;
        height: clamp(600px, calc(100vh - 190px), 760px);
        min-height: 600px;
        margin: 0 0 12px 0;
        overflow: hidden;
        background: #FFFFFF;
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
        border: 1px solid #EEF1F4;
        border-radius: 0;
        background: #FFFFFF;
        box-shadow: none;
        text-align: left;
        transition: background-color 0.15s ease, border-color 0.15s ease;
      }
      .home-corner .card-home:hover {
        transform: none;
        background: #F7FAFC;
        border-color: #DDE4EA;
        box-shadow: none;
      }

      /* Descrizione visibile nell'angolo esterno del relativo rettangolo. */
      .home-corner .card-home .home-card-label {
        position: absolute;
        width: clamp(150px, 10vw, 180px);
        min-height: 78px;
        padding: 10px 11px;
        border: 1px solid #DDE4EA;
        border-radius: 9px;
        background: #FFFFFF;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
        z-index: 2;
      }
      .home-corner .card-home:hover > div {
        border-color: #7FA6C9;
        box-shadow: 0 5px 12px rgba(0,0,0,0.09);
      }
      .home-corner .card-home .card-icona {
        font-size: 17px;
        margin-bottom: 3px;
      }
      .home-corner .card-home h4 {
        font-size: 13px;
        margin: 0 0 3px 0;
      }
      .home-corner .card-home p {
        font-size: 10.5px;
        line-height: 1.25;
        margin: 0;
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
      .modal-machine-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 16px;
        margin: 0 0 12px 0;
        padding: 2px 0 10px 0;
      }
      .modal-machine-title {
        color: #24364B;
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
        color: #24364B;
        opacity: 1;
        outline: none;
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
        color: #24364B;
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
        color: #24364B;
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
        color: #24364B;
        font-size: 13px;
        font-weight: 700;
      }
      .life-value {
        color: #24364B;
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
          icon("chart-bar", class = "card-icona"),
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
          icon("chart-line", class = "card-icona"),
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
          icon("gauge", class = "card-icona"),
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
          icon("triangle-exclamation", class = "card-icona"),
          h4("Allarmi e Near Miss"),
          p("Allarmi e segnalazioni Near Miss")
        ),
        class = "card-home"
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
  
  # Tabelle dati mostrate direttamente all'interno dei modal.
  mostra_dati_attivazioni <- reactiveVal(FALSE)
  mostra_dati_trend <- reactiveVal(FALSE)
  mostra_dati_tank <- reactiveVal(FALSE)
  mostra_dati_nok <- reactiveVal(FALSE)
  mostra_outlier_nok <- reactiveVal(FALSE)
  
  # I filtri vengono applicati solo dopo una breve pausa dall'ultima scelta.
  # In questo modo una selezione multipla genera un solo aggiornamento.
  ritardo_filtri_ms <- 800
  
  normalizza_sensori <- function(x) {
    if (is.null(x)) character(0) else sort(unique(as.character(x)))
  }
  
  sensori_modal_correnti <- reactiveVal(character(0))
  modal_apertura_id <- reactiveVal(0)
  granularita_attivazioni_corrente <- reactiveVal("Giorno")
  granularita_nok_corrente <- reactiveVal("Settimana")
  
  filtri_principali <- reactive({
    req(input$macchina, input$date)
    
    list(
      macchina = input$macchina,
      date = as.Date(input$date),
      sensori = sensori_lookup |>
        filter(coupon == input$macchina) |>
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
  apri_modal <- function(vista_iniziale, sensori_iniziali = NULL) {
    if (is.null(sensori_iniziali)) {
      sensori_iniziali <- sensori_lookup |>
        filter(coupon == isolate(input$macchina)) |>
        pull(cds_name)
    }
    
    sensori_modal_correnti(normalizza_sensori(sensori_iniziali))
    
    macchina_corrente <- macchine_lookup |>
      filter(coupon == isolate(input$macchina)) |>
      slice_head(n = 1)
    
    riferimento_macchina <- if (nrow(macchina_corrente) > 0) {
      paste(
        macchina_corrente$project,
        macchina_corrente$machine_name,
        sep = " – "
      )
    } else {
      as.character(isolate(input$macchina))
    }
    
    granularita_attivazioni_corrente("Giorno")
    granularita_nok_corrente("Settimana")
    
    modal_apertura_id(isolate(modal_apertura_id()) + 1)
    
    mostra_dati_attivazioni(FALSE)
    mostra_dati_trend(FALSE)
    mostra_dati_tank(FALSE)
    mostra_dati_nok(FALSE)
    
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
            "Vita sensori"          = "vita",
            "NOK"                   = "nok",
            "Allarmi e Near Miss"    = "allarmi"
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
  
  observeEvent(input$home_allarmi, {
    req(input$macchina)
    apri_modal("allarmi")
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
    
    apri_modal("attivazioni", sensore_cliccato)
  })
  
  # Quando si cambia vista, azzera la visibilita' delle tabelle dati
  observeEvent(input$vista_selezionata, {
    mostra_dati_attivazioni(FALSE)
    mostra_dati_trend(FALSE)
    mostra_dati_tank(FALSE)
    mostra_dati_nok(FALSE)
    mostra_outlier_nok(FALSE)
    
  }, ignoreInit = TRUE)
  
  # Contenuto del modal: si aggiorna al cambio di vista senza chiudere il dialogo
  output$modal_contenuto <- renderUI({
    req(input$vista_selezionata, input$macchina)
    modal_apertura_id()
    
    vista <- input$vista_selezionata
    
    sensori_macchina <- sensori_lookup |>
      filter(coupon == input$macchina)
    
    sensori_selezionati <- intersect(
      isolate(sensori_modal_correnti()),
      sensori_macchina$cds_name
    )
    
    date_selezionate <- isolate(as.Date(input$date))
    
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
        h4("Profilo utilizzo macchina", class = "titolo-sezione"),
        uiOutput("modal_utilizzo_macchina"),
        girafeOutput("modal_utilizzoNokPlot", height = "420px"),
        h4("Utilizzo macchina nel tempo", class = "titolo-sezione"),
        radioButtons(
          "modal_granularita_nok",
          "Raggruppamento:",
          choices = c("Settimana", "Mese", "Trimestre", "Anno"),
          selected = isolate(granularita_nok_corrente()),
          inline = TRUE
        ),
        girafeOutput("modal_utilizzoTempoPlot", height = "420px")
      )
      
    } else if (vista == "allarmi") {
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
          class = "alarm-section",
          h4("Attivazioni anomale", class = "titolo-sezione"),
          p(
            "Giornate escluse dal calcolo del NOK perché individuate come anomalie nel conteggio delle attivazioni."
          ),
          uiOutput("modal_allarmi_attivazioni_panel")
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
  
  observeEvent(input$modal_btn_outlier_nok, {
    mostra_outlier_nok(!mostra_outlier_nok())
  })
  
  # ---------------------------------------------------------------------
  # Coda ottimizzata dei filtri modal.
  # Il debounce aspetta 800 ms dall'ultima modifica; solo allora aggiorna
  # grafici, stato del modal e filtri principali. Il contenuto del modal
  # non dipende direttamente dai filtri principali, evitando il loop di
  # distruzione e ricreazione continua degli input.
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
      updatePickerInput(session, "modal_sensori", selected = filtri$sensori)
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
    
    if (
      identical(filtri$apertura_id, isolate(modal_apertura_id())) &&
      !identical(as.Date(input$date), filtri$date)
    ) {
      updateDateRangeInput(
        session,
        "date",
        start = filtri$date[1],
        end = filtri$date[2]
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$date, {
    req(input$date)
    
    date_corrente <- as.Date(input$date)
    
    if (
      !is.null(input$modal_date) &&
      !identical(as.Date(input$modal_date), date_corrente)
    ) {
      updateDateRangeInput(
        session,
        "modal_date",
        start = date_corrente[1],
        end = date_corrente[2]
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
  
  filtri_attivazioni_modal <- reactive({
    req(input$macchina, input$date)
    
    list(
      macchina = input$macchina,
      date = as.Date(input$date),
      sensori = sensori_modal_correnti(),
      granularita = granularita_attivazioni_corrente()
    )
  })
  
  filtri_vita_modal <- reactive({
    req(input$macchina)
    
    list(
      macchina = input$macchina,
      sensori = sensori_modal_correnti()
    )
  })
  
  filtri_nok_modal <- reactive({
    req(input$macchina, input$date)
    
    list(
      macchina = input$macchina,
      date = as.Date(input$date),
      sensori = sensori_modal_correnti(),
      granularita = granularita_nok_corrente()
    )
  })
  
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
  
  output$modal_panel_outlier_nok <- renderUI({
    if (!mostra_outlier_nok()) return(NULL)
    
    div(
      class = "modal-data-panel",
      p(
        "Attivazioni giornaliere escluse dal calcolo del NOK nel periodo selezionato. ",
        "Il filtro statistico viene applicato al conteggio giornaliero delle attivazioni, ",
        "separatamente per ciascun sensore."
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
        daily_count = sum(increment, na.rm = TRUE),
        daily_uptime = if (all(is.na(daily_uptime))) {
          NA_real_
        } else {
          max(daily_uptime, na.rm = TRUE)
        },
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
        
        # Con pochi dati il LOF non e' sufficientemente stabile:
        # in quel caso non viene esclusa alcuna giornata.
        if (n_validi >= 6L && dplyr::n_distinct(x[validi]) >= 3L) {
          k <- min(4L, n_validi - 1L)
          score <- dbscan::lof(
            matrix(x[validi], ncol = 1),
            minPts = k
          )
          
          mediana_attivazioni <- stats::median(x[validi], na.rm = TRUE)
          divario_ampio <- (
            x[validi] >= mediana_attivazioni * 5 &
            (x[validi] - mediana_attivazioni) >= 49
          )
          
          .x$lof_score[validi] <- score
          .x$outlier_lof[validi] <- (
            !is.na(score) &
            score > 2 &
            divario_ampio
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
    valori_giornalieri() |>
      filter(!outlier_lof) |>
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
        !outlier_lof,
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
    valori_giornalieri_modal_nok() |>
      filter(!outlier_lof) |>
      group_by(cds_name, sensor_description) |>
      summarise(NMN = mean(daily_value, na.rm = TRUE), .groups = "drop")
  })
  
  # Statistiche storiche del NOK giornaliero per sensore.
  # Limiti di riferimento: media storica +/- 3 sigma.
  statistiche_nok_storiche_modal <- reactive({
    
    valori_giornalieri_modal_nok() |>
      filter(!outlier_lof) |>
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
        limite_nok_inf = media_nok_storico - 3 * sigma_nok_storico,
        limite_nok_sup = media_nok_storico + 3 * sigma_nok_storico
      )
  })
  
  kpi_nok_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    nmm_per_sensore <- valori_giornalieri_modal_nok() |>
      filter(
        !outlier_lof,
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
    
    valori_giornalieri_modal_nok() |>
      filter(
        outlier_lof,
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      arrange(day, cds_name) |>
      transmute(
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        Data = day,
        Attivazioni = round(daily_count)
      )
  })
  
  allarmi_nok_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    valori_giornalieri_modal_nok() |>
      filter(
        !outlier_lof,
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
        Direzione
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
        !outlier_lof,
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
  utilizzo_temporale_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    base <- valori_giornalieri_modal_nok() |>
      filter(
        !outlier_lof,
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      left_join(
        nmn_storico_modal_nok(),
        by = c("cds_name", "sensor_description")
      ) |>
      mutate(
        NOK_giornaliero = case_when(
          is.na(daily_value) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ daily_value / NMN
        ),
        periodo = as.Date(
          periodo_bucket(day, filtri$granularita)
        )
      ) |>
      filter(is.finite(NOK_giornaliero))
    
    base |>
      group_by(
        periodo,
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
        U_sensore = media_nok * varianza_nok
      ) |>
      filter(is.finite(U_sensore)) |>
      group_by(periodo) |>
      summarise(
        U_macchina = mean(U_sensore, na.rm = TRUE),
        n_sensori = n_distinct(cds_name),
        .groups = "drop"
      ) |>
      filter(is.finite(U_macchina)) |>
      arrange(periodo) |>
      mutate(
        periodo_label = formatta_periodo_label(
          periodo,
          filtri$granularita
        )
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
  # U_sensore  = media(NOK) * varianza(NOK)
  # U_macchina = media(U_sensore)
  # ---------------------------------------------------------------------
  utilizzo_nok_modal <- reactive({
    
    filtri <- filtri_nok_modal()
    
    # Base comune: stessi dati filtrati e stesso NMN usati dal NOK.
    base_completa <- valori_giornalieri_modal_nok() |>
      filter(!outlier_lof) |>
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
    
    # Periodo attualmente selezionato.
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
        U_sensore = media_nok * varianza_nok,
        banda_min = pmax(0, media_nok - varianza_nok),
        banda_max = media_nok + varianza_nok,
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
    
    # -------------------------------------------------------------------
    # Riferimento storico:
    # - stessa durata del periodo selezionato
    # - nuova finestra ogni 7 giorni
    # - esclusa l'eventuale finestra identica a quella corrente
    # -------------------------------------------------------------------
    durata_giorni <- as.integer(
      filtri$date[2] - filtri$date[1]
    ) + 1L
    
    calcola_componenti_finestra <- function(inizio) {
      fine <- inizio + durata_giorni - 1L
      
      base_completa |>
        filter(
          day >= inizio,
          day <= fine
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
          U_sensore = media_nok * varianza_nok,
          inizio_finestra = inizio
        ) |>
        filter(
          is.finite(media_nok),
          is.finite(varianza_nok),
          is.finite(U_sensore)
        )
    }
    
    prima_data <- if (nrow(base_completa) > 0) {
      min(base_completa$day, na.rm = TRUE)
    } else {
      as.Date(NA)
    }
    
    ultima_data <- if (nrow(base_completa) > 0) {
      max(base_completa$day, na.rm = TRUE)
    } else {
      as.Date(NA)
    }
    
    ultima_partenza <- ultima_data - durata_giorni + 1L
    
    if (
      is.na(prima_data) ||
      is.na(ultima_data) ||
      ultima_partenza < prima_data
    ) {
      componenti_storici <- tibble()
      U_storici <- numeric(0)
    } else {
      partenze_storiche <- seq.Date(
        from = prima_data,
        to = ultima_partenza,
        by = "7 days"
      )
      
      partenze_storiche <- partenze_storiche[
        partenze_storiche != filtri$date[1]
      ]
      
      lista_componenti <- lapply(
        partenze_storiche,
        calcola_componenti_finestra
      )
      
      componenti_storici <- bind_rows(lista_componenti)
      
      U_storici <- componenti_storici |>
        group_by(inizio_finestra) |>
        summarise(
          U_macchina = mean(U_sensore, na.rm = TRUE),
          .groups = "drop"
        ) |>
        filter(is.finite(U_macchina)) |>
        pull(U_macchina)
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
      !is.finite(U_macchina) | !is.finite(P90_utilizzo) ~ "N/D",
      U_macchina > P90_utilizzo ~ "Anomalo",
      TRUE ~ "Normale"
    )
    
    # -------------------------------------------------------------------
    # Attribuzione della causa dell'anomalia.
    # Per U = media * varianza, la differenza rispetto al riferimento
    # storico viene scomposta esattamente in due contributi simmetrici:
    # uno dovuto alla media NOK e uno dovuto alla varianza NOK.
    # -------------------------------------------------------------------
    causa_anomalia <- NA_character_
    contributo_media <- NA_real_
    contributo_varianza <- NA_real_
    
    if (
      identical(stato_utilizzo, "Anomalo") &&
      nrow(componenti_storici) > 0
    ) {
      riferimento_sensori <- componenti_storici |>
        group_by(cds_name, sensor_description) |>
        summarise(
          media_nok_rif = mean(media_nok, na.rm = TRUE),
          varianza_nok_rif = mean(varianza_nok, na.rm = TRUE),
          .groups = "drop"
        )
      
      decomposizione <- per_sensore |>
        inner_join(
          riferimento_sensori,
          by = c("cds_name", "sensor_description")
        ) |>
        filter(
          is.finite(media_nok),
          is.finite(varianza_nok),
          is.finite(media_nok_rif),
          is.finite(varianza_nok_rif)
        ) |>
        mutate(
          contributo_media = (
            media_nok - media_nok_rif
          ) * (
            varianza_nok + varianza_nok_rif
          ) / 2,
          contributo_varianza = (
            varianza_nok - varianza_nok_rif
          ) * (
            media_nok + media_nok_rif
          ) / 2
        )
      
      if (nrow(decomposizione) > 0) {
        contributo_media <- mean(
          decomposizione$contributo_media,
          na.rm = TRUE
        )
        contributo_varianza <- mean(
          decomposizione$contributo_varianza,
          na.rm = TRUE
        )
        
        pos_media <- max(contributo_media, 0)
        pos_varianza <- max(contributo_varianza, 0)
        totale_positivo <- pos_media + pos_varianza
        
        if (totale_positivo > 0) {
          quota_media <- pos_media / totale_positivo
          
          causa_anomalia <- case_when(
            quota_media >= 0.70 ~ "aumento dell'utilizzo",
            quota_media <= 0.30 ~ "aumento della varianza",
            TRUE ~ "aumento dell'utilizzo e della varianza"
          )
        }
      }
    }
    
    list(
      per_sensore = per_sensore,
      U_macchina = U_macchina,
      P90_utilizzo = P90_utilizzo,
      stato_utilizzo = stato_utilizzo,
      causa_anomalia = causa_anomalia,
      contributo_media = contributo_media,
      contributo_varianza = contributo_varianza,
      n_finestre_storiche = length(U_storici),
      durata_giorni = durata_giorni
    )
  }) |>
    bindCache(
      filtri_nok_modal()$macchina,
      filtri_nok_modal()$date,
      filtri_nok_modal()$sensori
    )
  
  output$modal_utilizzo_macchina <- renderUI({
    
    utilizzo <- utilizzo_nok_modal()
    
    valore <- if (
      length(utilizzo$U_macchina) == 0 ||
      !is.finite(utilizzo$U_macchina)
    ) {
      "N/D"
    } else {
      formatC(
        utilizzo$U_macchina,
        format = "f",
        digits = 2,
        decimal.mark = ","
      )
    }
    
    soglia <- if (
      length(utilizzo$P90_utilizzo) == 0 ||
      !is.finite(utilizzo$P90_utilizzo)
    ) {
      "N/D"
    } else {
      formatC(
        utilizzo$P90_utilizzo,
        format = "f",
        digits = 2,
        decimal.mark = ","
      )
    }
    
    stato <- utilizzo$stato_utilizzo
    
    colore <- switch(
      stato,
      "Normale" = "#19764A",
      "Anomalo" = "#B42318",
      "#5F6F7F"
    )
    
    sfondo <- switch(
      stato,
      "Normale" = "#F2FBF6",
      "Anomalo" = "#FFF5F4",
      "#F4F8FB"
    )
    
    div(
      style = paste0(
        "margin:14px 0 20px 0;",
        "padding:15px 18px;",
        "border:2px solid ", colore, ";",
        "border-radius:9px;",
        "background:", sfondo, ";"
      ),
      span(
        "Utilizzo periodo: ",
        style = "font-size:16px;font-weight:700;color:#4A4A4A;"
      ),
      span(
        valore,
        style = paste0(
          "font-size:28px;font-weight:800;color:",
          colore,
          ";"
        )
      ),
      span(
        paste0("  ", stato),
        style = paste0(
          "margin-left:10px;",
          "padding:4px 10px;",
          "border-radius:999px;",
          "font-size:14px;",
          "font-weight:800;",
          "color:#FFFFFF;",
          "background:",
          colore,
          ";"
        )
      ),
      div(
        "Indice dell’intensità di utilizzo rispetto al comportamento storico dei sensori. Valori alti indicano uno stress della macchina più elevato o una discrepanza nell’utilizzo rispetto allo storico.",
        style = "margin-top:7px;font-size:13px;color:#5F6F7F;"
      ),
      div(
        {
          n_finestre <- utilizzo$n_finestre_storiche
          testo_finestre <- if (identical(n_finestre, 1L)) {
            "1 finestra storica"
          } else {
            paste0(n_finestre, " finestre storiche")
          }
          
          causa <- if (
            identical(stato, "Anomalo") &&
            !is.null(utilizzo$causa_anomalia) &&
            length(utilizzo$causa_anomalia) > 0 &&
            !is.na(utilizzo$causa_anomalia)
          ) {
            paste0(
              ", anomalia dovuta a un ",
              utilizzo$causa_anomalia
            )
          } else {
            ""
          }
          
          paste0(
            "Soglia di anomalia: ",
            soglia,
            " — riferimento calcolato su ",
            testo_finestre,
            " di ",
            utilizzo$durata_giorni,
            " giorni",
            causa,
            "."
          )
        },
        style = "margin-top:4px;font-size:12px;color:#718096;"
      )
    )
  })
  
  render_utilizzo_nok_gg <- function() {
    
    utilizzo <- utilizzo_nok_modal()
    profilo <- utilizzo$per_sensore |>
      filter(
        is.finite(media_nok),
        is.finite(varianza_nok)
      )
    
    validate(
      need(nrow(profilo) > 0, "Dati insufficienti per calcolare media e varianza del NOK")
    )
    
    profilo <- profilo |>
      mutate(
        x = row_number(),
        tooltip_utilizzo = paste0(
          "<b>", etichetta_sensore, "</b><br/>",
          "Media NOK: ", round(media_nok, 4), "<br/>",
          "Varianza NOK: ", round(varianza_nok, 4), "<br/>",
          "U sensore: ", round(U_sensore, 4)
        )
      )
    
    ggplot(profilo, aes(x = x)) +
      geom_ribbon(
        aes(ymin = banda_min, ymax = banda_max),
        fill = "#7FA6C9",
        alpha = 0.22
      ) +
      geom_line(
        aes(y = media_nok),
        color = "#2C3E50",
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(
          y = media_nok,
          tooltip = tooltip_utilizzo,
          data_id = etichetta_sensore
        ),
        color = "#2C3E50",
        size = 2.6
      ) +
      scale_x_continuous(
        breaks = profilo$x,
        labels = profilo$etichetta_sensore
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
        axis.text.x = element_text(
          size = 11,
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
        opts_hover(css = "opacity:0.8;cursor:pointer;")
      )
    )
  })
  
  render_utilizzo_tempo_gg <- function() {
    
    andamento <- utilizzo_temporale_modal()
    granularita <- filtri_nok_modal()$granularita
    
    validate(
      need(
        nrow(andamento) > 0,
        "Dati insufficienti per calcolare l'utilizzo macchina con il raggruppamento selezionato"
      )
    )
    
    breaks_periodo <- calcola_breaks_periodo(andamento$periodo)
    max_utilizzo <- max(andamento$U_macchina, na.rm = TRUE)
    
    ggplot(
      andamento,
      aes(x = periodo, y = U_macchina)
    ) +
      geom_line(
        color = "#2C3E50",
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(
          tooltip = paste0(
            "<b>", periodo_label, "</b><br/>",
            "Utilizzo macchina: ", round(U_macchina, 2), "<br/>",
            "Sensori valutati: ", n_sensori
          ),
          data_id = as.character(periodo)
        ),
        color = "#2C3E50",
        size = 2.8
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
          sort(unique(c(
            b,
            if (is.finite(max_utilizzo)) max_utilizzo
          )))
        },
        expand = expansion(mult = c(0.03, 0.06))
      ) +
      labs(
        x = NULL,
        y = "Utilizzo macchina"
      ) +
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
        axis.text.y = element_text(size = 11)
      )
  }
  
  output$modal_utilizzoTempoPlot <- renderGirafe({
    w_px <- if (!is.null(input$modal_px_width) && input$modal_px_width > 0)
      input$modal_px_width else 1100
    
    girafe(
      ggobj = render_utilizzo_tempo_gg(),
      width_svg = w_px / 72,
      height_svg = 360 / 72,
      options = list(
        opts_tooltip(css = tooltip_css, use_fill = FALSE),
        opts_hover(css = "opacity:0.8;cursor:pointer;")
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
  
  output$modal_tabella_outlier_nok <- renderDT({
    
    filtri <- filtri_nok_modal()
    
    esclusi <- valori_giornalieri_modal_nok() |>
      filter(
        outlier_lof,
        day >= filtri$date[1],
        day <= filtri$date[2]
      ) |>
      arrange(day, cds_name) |>
      transmute(
        Sensore = paste(cds_name, sensor_description, sep = " - "),
        Data = format(day, "%d-%m-%Y"),
        Attivazioni = round(daily_count)
      )
    
    validate(
      need(
        nrow(esclusi) > 0,
        "Nessuna attivazione esclusa nel periodo selezionato."
      )
    )
    
    esclusi
  }, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
  
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