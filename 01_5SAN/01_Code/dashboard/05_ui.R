dashboard_ui <- fluidPage(
  
  dashboard_assets(),
  
  div(
    class = "san-dashboard-header",
    tags$img(class = "san-dashboard-title-img", src = "san-header/dashboard_5san_title.png", alt = "Dashboard 5SAN"),
    tags$img(class = "san-dashboard-logo", src = "san-header/5san_header_emblem.png", alt = "Logo 5SAN")
  ),
  
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
          language = "it", weekstart = 1
        )
      ),
      column(
        width = if (IS_WEB_MODE) 8 else 2,
        div(
          class = "san-guide-action",
          actionButton(
            "home_guida", "Guida alla dashboard", icon = icon("circle-question"),
            class = "san-guide-button",
            title = "Apri la guida alla dashboard"
          )
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
          h4("Conteggio manovre"),
          p("Quante manovre ha effettuato ciascun componente nel tempo")
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
          h4("Utilizzo storico"),
          p("Come vengono usati i componenti rispetto al passato")
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
          h4("Vita componenti"),
          p("Manovre e tempo di utilizzo rispetto ai valori di riferimento")
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
          h4("Allarmi e segnalazioni"),
          p("Eventi insoliti e situazioni da verificare")
        ),
        class = "card-home"
      )
    )
  )
)


ui <- if (IS_WEB_MODE) {
  fluidPage(
    uiOutput("web_gate")
  )
} else {
  dashboard_ui
}

