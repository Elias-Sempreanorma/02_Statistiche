library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
library(RColorBrewer)
library(here)
library(stringr)

dati <- readRDS(here("02_Output", "sensor_count_increment.rds"))

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

# Lista aziende per il primo filtro
company <- dati |>
  distinct(company) |>
  arrange(company) |>
  pull(company)

data_min <- min(dati$day, na.rm = TRUE)
data_max <- max(dati$day, na.rm = TRUE)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
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
    "))
  ),
  
  titlePanel("Conteggio attivazioni"),
  
  # Filtri in orizzontale, a piena larghezza
  div(
    class = "pannello-filtri",
    fluidRow(
      column(2, selectInput("azienda", "Azienda:", choices = company)),
      column(2, selectInput("stabilimento", "Stabilimento:", choices = NULL)),
      column(3, selectInput("macchina", "Macchina:", choices = NULL)),
      column(
        3,
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
      column(
        2,
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
      "Grafico attivazioni",
      
      fluidRow(
        column(12, plotOutput("activationPlot", height = "550px"))
      ),
      
      fluidRow(
        column(
          12,
          h4("Stato sensori", class = "titolo-sezione"),
          plotOutput("tankPlot", height = "260px")
        )
      )
    ),
    
    tabPanel(
      "Storico NOK",
      
      fluidRow(
        column(
          12,
          h4("NOK per sensore", class = "titolo-sezione"),
          tableOutput("nok_table")
        )
      ),
      
      fluidRow(
        column(12, plotOutput("nokHistoryPlot", height = "550px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # 1. Quando cambia l'azienda, aggiorno gli stabilimenti disponibili
  observeEvent(input$azienda, {
    
    req(input$azienda)
    
    stabilimenti <- dati |>
      filter(company == input$azienda) |>
      distinct(field) |>
      arrange(field) |>
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
    
    macchine_filtrate <- dati |>
      filter(
        company == input$azienda,
        field == input$stabilimento
      ) |>
      distinct(coupon, project, machine_name) |>
      arrange(project, machine_name)
    
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
    
    sensori_macchina <- dati |>
      filter(coupon == input$macchina) |>
      distinct(cds_name, .keep_all = TRUE) |>
      select(cds_name, sensor_description) |>
      arrange(cds_name)
    
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
  
  kpi_nok <- reactive({
    
    req(input$macchina, input$date, input$sensori)
    
    dati_base <- dati |>
      ungroup() |>
      filter(
        coupon == input$macchina,
        cds_name %in% input$sensori
      )
    
    # NMN: media giornaliera di aperture orarie per sensore su tutto lo storico disponibile
    nmn_per_sensore <- dati_base |>
      group_by(cds_name, sensor_description, day) |>
      summarise(daily_NMN = sum(increment) / unique(daily_uptime), .groups = "drop") |>
      group_by(cds_name, sensor_description) |>
      summarise(NMN = mean(daily_NMN, na.rm = TRUE), .groups = "drop")
    
    # NMM: media giornaliera di aperture orarie per sensore ristretta al periodo selezionato
    nmm_per_sensore <- dati_base |>
      filter(
        day >= input$date[1],
        day <= input$date[2]
      ) |>
      group_by(cds_name, sensor_description, day) |>
      summarise(daily_NMM = sum(increment) / unique(daily_uptime), .groups = "drop") |>
      group_by(cds_name, sensor_description) |>
      summarise(NMM = mean(daily_NMM, na.rm = TRUE), .groups = "drop")
    
    nmn_per_sensore |>
      left_join(nmm_per_sensore, by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK = case_when(
          is.na(NMN) | is.na(NMM) | NMM == 0 ~ NA_real_,
          TRUE ~ NMN / NMM
        ),
        # Ordinamento naturale: prefisso alfabetico + numero,
        # cosi' FCM2 viene prima di FCM10
        .prefisso = str_extract(cds_name, "^[^0-9]+"),
        .numero = as.numeric(str_extract(cds_name, "[0-9]+$"))
      ) |>
      arrange(.prefisso, .numero) |>
      select(-.prefisso, -.numero)
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
      mutate(
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - "),
        .prefisso = str_extract(cds_name, "^[^0-9]+"),
        .numero = as.numeric(str_extract(cds_name, "[0-9]+$"))
      ) |>
      arrange(.prefisso, .numero) |>
      mutate(
        etichetta_sensore = factor(etichetta_sensore, levels = unique(etichetta_sensore))
      )
    
    tank_attivazioni <- base |>
      transmute(
        etichetta_sensore,
        tipo = "Attivazioni",
        valore = count,
        massimo = cds_vds
      )
    
    tank_durata <- base |>
      transmute(
        etichetta_sensore,
        tipo = "Vita utile",
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
  
  output$tankPlot <- renderPlot({
    
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
        width = 0.45
      ) +
      # livello di riempimento effettivo, in percentuale sul massimo
      geom_col(
        data = tanks,
        aes(x = tipo, y = percentuale_capped, fill = stato),
        width = 0.45
      ) +
      geom_text(
        data = tanks,
        aes(
          x = tipo,
          y = pmax(percentuale_capped, 10),
          label = paste0(round(valore, 1), " / ", massimo)
        ),
        vjust = -0.4,
        size = 3.6,
        fontface = "bold",
        color = "#4A4A4A"
      ) +
      facet_wrap(~etichetta_sensore, nrow = 1) +
      scale_fill_manual(
        values = c(ok = "#9DC3A0", over = "#D99B94"),
        guide = "none"
      ) +
      scale_y_continuous(limits = c(0, 112), expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold"),
        strip.text = element_text(size = 11, face = "bold", color = "#4A4A4A"),
        strip.background = element_blank(),
        panel.spacing.x = unit(14, "pt")
      )
  })
  
  dati_grafico <- reactive({
    
    req(input$macchina, input$date, input$sensori)
    
    dati |>
      filter(
        coupon == input$macchina,
        cds_name %in% input$sensori,
        day >= input$date[1],
        day <= input$date[2]
      ) |>
      group_by(
        day,
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
        .prefisso = str_extract(cds_name, "^[^0-9]+"),
        .numero = as.numeric(str_extract(cds_name, "[0-9]+$"))
      ) |>
      arrange(day, .prefisso, .numero) |>
      mutate(
        etichetta = factor(etichetta, levels = unique(etichetta)),
        etichetta_completa = factor(etichetta_completa, levels = unique(etichetta_completa))
      ) |>
      select(-.prefisso, -.numero)
  })
  
  # Storico giornaliero del NOK per sensore: ogni giorno del periodo
  # selezionato viene confrontato con la media storica (NMN) dello
  # stesso sensore, calcolata su tutto lo storico disponibile
  kpi_nok_storico <- reactive({
    
    req(input$macchina, input$date, input$sensori)
    
    dati_base <- dati |>
      ungroup() |>
      filter(
        coupon == input$macchina,
        cds_name %in% input$sensori
      )
    
    # Valore giornaliero per ogni giorno disponibile (tutto lo storico)
    valori_giornalieri <- dati_base |>
      group_by(cds_name, sensor_description, day) |>
      summarise(daily_value = sum(increment) / unique(daily_uptime), .groups = "drop")
    
    # NMN: media storica di riferimento per sensore (tutto lo storico)
    nmn_per_sensore <- valori_giornalieri |>
      group_by(cds_name, sensor_description) |>
      summarise(NMN = mean(daily_value, na.rm = TRUE), .groups = "drop")
    
    valori_giornalieri |>
      filter(
        day >= input$date[1],
        day <= input$date[2]
      ) |>
      left_join(nmn_per_sensore, by = c("cds_name", "sensor_description")) |>
      mutate(
        NOK_giorno = case_when(
          is.na(daily_value) | is.na(NMN) | NMN == 0 ~ NA_real_,
          TRUE ~ daily_value / NMN
        ),
        etichetta_sensore = paste(cds_name, sensor_description, sep = " - ")
      ) |>
      arrange(day)
  })
  
  output$nokHistoryPlot <- renderPlot({
    
    storico <- kpi_nok_storico()
    
    validate(
      need(nrow(storico) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    n_sensori <- dplyr::n_distinct(storico$etichetta_sensore)
    palette_sensori <- colorRampPalette(
      brewer.pal(8, "Set2")
    )(n_sensori)
    
    ggplot(
      storico,
      aes(x = day, y = NOK_giorno, color = etichetta_sensore)
    ) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.8) +
      geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "#9AA5B1"
      ) +
      scale_color_manual(
        values = palette_sensori,
        name = "Sensore"
      ) +
      scale_x_date(
        date_labels = "%d-%m-%Y"
      ) +
      labs(
        title = "Andamento storico del NOK",
        x = NULL,
        y = "NOK"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 90),
        plot.title = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
  })
  
  output$activationPlot <- renderPlot({
    
    grafico <- dati_grafico()
    
    validate(
      need(nrow(grafico) > 0, "Nessun dato disponibile per i filtri scelti")
    )
    
    # Palette tenue e professionale, generata dinamicamente in base
    # al numero di sensori (etichetta completa) presenti nei dati filtrati
    n_sensori <- dplyr::n_distinct(grafico$etichetta_completa)
    palette_sensori <- colorRampPalette(
      brewer.pal(8, "Set2")
    )(n_sensori)
    
    ggplot(
      grafico,
      aes(x = etichetta, y = attivazioni, fill = etichetta_completa)
    ) +
      geom_col(
        width = 0.8
      ) +
      facet_grid(
        cols = vars(day),
        scales = "free_x",
        space = "free_x",
        switch = "x"
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.05))
      ) +
      scale_fill_manual(
        values = palette_sensori,
        name = "Sensore"
      ) +
      labs(
        title = "Conteggio attivazioni",
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(size = 9, angle = 90),
        plot.title = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
  })
}

shinyApp(ui, server)