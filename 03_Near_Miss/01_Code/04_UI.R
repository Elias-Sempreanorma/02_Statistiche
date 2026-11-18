# Interfaccia operativa: tutti i campi del preliminare, con rami condizionali.
scelta <- function(id, titolo, opzioni, obbligatorio=TRUE, valore=character(0)) {
  shiny::radioButtons(id, paste0(titolo, if(obbligatorio) " *" else ""),
                      choices=opzioni, selected=valore)
}

interfaccia <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    shiny::tags$style(shiny::HTML("
      body {background:#f4f7f9; color:#243b50; font-family:system-ui,Arial,sans-serif;}
      .container-fluid{max-width:1160px;margin:auto;padding:26px;}
      .pannello{background:white;border:1px solid #dce5eb;border-radius:12px;
        padding:24px;margin:15px 0;box-shadow:0 4px 14px rgba(16,45,70,.04);}
      h1{font-size:29px;font-weight:750;color:#1d485f}
      h3{color:#1d485f;font-size:19px}
      .info{background:#e9f1f6;border-left:4px solid #42758d;padding:13px;margin-bottom:15px}
      .shiny-input-container{max-width:none}
      .btn-primary{background:#246181;border-color:#246181}
      .azione{border:1px solid #e1e5e8;padding:16px;margin:12px 0;border-radius:8px}
      .avviso{padding:12px;background:#fff5df;border-left:4px solid #e9a133;margin:12px 0}
    "))
  ),
  shiny::titlePanel("5SAN · Verifica preliminare delle segnalazioni"),
  shiny::uiOutput("schermata")
)

interfaccia_accesso <- function() shiny::div(class="pannello",
  shiny::h3("Accesso riservato"),
  shiny::textInput("account","Account"),
  shiny::passwordInput("password","Password"),
  shiny::actionButton("accedi","Accedi",class="btn-primary"),
  shiny::p("Ogni operatore può consultare soltanto le macchine a lui autorizzate.")
)

interfaccia_lavoro <- function() shiny::tagList(
  shiny::div(class="pannello",shiny::fluidRow(
    shiny::column(9,shiny::strong(shiny::textOutput("utente_attivo",inline=TRUE))),
    shiny::column(3,shiny::actionButton("esci","Esci"))),
    shiny::h3("Segnalazioni 5SAN"),
    shiny::textInput("ricerca","Ricerca per codice o nome macchina"),
    shiny::actionButton("cerca","Aggiorna elenco"),
    DT::DTOutput("elenco")),
  shiny::uiOutput("dettaglio_modulo")
)

modulo_ui <- function(segnalazione, ultima) {
  if (is.null(segnalazione)) return(NULL)
  a <- segnalazione
  shiny::tagList(
    shiny::div(class="pannello",
      shiny::h3("A01–A07 · Dati della segnalazione (sola lettura)"),
      shiny::div(class="info",
        shiny::p(shiny::strong("A01 · ID: "),a$alert_id),
        shiny::p(shiny::strong("A02 · Periodo osservato: "),
          format(a$period_start,"%d/%m/%Y %H:%M",tz="Europe/Rome")," – ",
          format(a$period_end,"%d/%m/%Y %H:%M",tz="Europe/Rome")),
        shiny::p(shiny::strong("A03 · Generazione: "),
          format(a$generated_at,"%d/%m/%Y %H:%M",tz="Europe/Rome")),
        shiny::p(shiny::strong("A04 · Macchina: "),a$machine_code," · ",a$machine_name),
        shiny::p(shiny::strong("A07 · Origine: "),a$source)),
      shiny::p(shiny::strong("Tipo: "),a$alert_type," · ",a$signal_description),
      shiny::p(shiny::strong("Componenti: "),a$sensor_code," · ",a$sensor_description),
      shiny::div(class="avviso","A09 · Primo invio da 5SAN a san.it: non disponibile. Verrà integrato in seguito; non viene sostituito con la data di compilazione."),
      if (!is.null(ultima)) shiny::p("Ultima revisione registrata: ",ultima$revisione,
                                      " · ",format(ultima$data_registrazione,"%d/%m/%Y %H:%M",tz="Europe/Rome"))),
    shiny::div(class="pannello",
      shiny::h3("A08 · Responsabile della presa in carico"),
      shiny::selectInput("responsabile_funzione","Funzione aziendale *",
                         c("Seleziona"="", "RSPP / HSE"="RSPP_HSE",
                           "Responsabile della manutenzione"="MAINTENANCE",
                           "Altro"="OTHER"),selected=vecchio_campo(ultima,"responsabile_funzione")),
      shiny::textInput("responsabile_nome","Nome e cognome *",value=vecchio_campo(ultima,"responsabile_nome")),
      shiny::textInput("responsabile_ruolo","Ruolo aziendale *",value=vecchio_campo(ultima,"responsabile_ruolo")),
      shiny::p("Il responsabile dichiarato può essere diverso dall'account che registra la verifica.")),
    shiny::div(class="pannello",
      shiny::h3("A10 · Esito della verifica preliminare"),
      scelta("esito","Seleziona un solo percorso",setNames(names(esiti),paste0(names(esiti),". ",unname(esiti))),valore=vecchio_campo(ultima,"esito")),
      shiny::uiOutput("percorso")),
    shiny::div(class="pannello",
      shiny::h3("Riepilogo e invio"),
      shiny::uiOutput("riepilogo"),
      shiny::actionButton("invia","Invia modulo",class="btn-primary"),
      shiny::p("L'invio salva una nuova revisione nel database. Non invia notifiche, non apre automaticamente un'indagine e non certifica l'efficacia delle misure.")))
}

interfaccia_percorso <- function(esito, n_azioni=1L, ultima=NULL) {
  if (is.null(esito) || !esito %in% names(esiti)) return(NULL)
  shiny::tagList(
    shiny::h3(paste0("A11 · ",domande_a11[[esito]])),
    shiny::textAreaInput("motivazione","Riscontro e motivazione *",rows=4,width="100%",value=vecchio_campo(ultima,"motivazione",esito)),
    if (esito=="4") shiny::tagList(
      shiny::h3("Condizioni critiche, pericoli e fonti"),
      shiny::checkboxGroupInput("criticita","Condizioni critiche riscontrate *",choices=criticita,selected=vecchia_lista(ultima,"criticita",esito)),
      shiny::uiOutput("altra_criticita"),
      shiny::checkboxGroupInput("pericoli","Pericoli individuati *",choices=pericoli,selected=vecchia_lista(ultima,"pericoli",esito)),
      shiny::uiOutput("altro_pericolo"),
      shiny::checkboxGroupInput("fonti","Elementi utilizzati per la verifica *",choices=fonti,selected=vecchia_lista(ultima,"fonti",esito)),
      shiny::textAreaInput("fonti_nota","Riferimento al riscontro",rows=2,width="100%",value=vecchio_campo(ultima,"fonti_nota",esito)),
      shiny::h3("Qualificazione dell'accaduto"),
      scelta("episodio","È stato verificato anche un episodio effettivamente accaduto?",si_no_ignoto,valore=vecchio_campo(ultima,"episodio",esito)),
      shiny::uiOutput("domande_episodio"),
      shiny::uiOutput("proposta_qualificazione"),
      shiny::uiOutput("chiarimenti_ui"),
      shiny::h3("Risposta aziendale dichiarata"),
      scelta("misure","Sono state adottate o disposte misure?",
             c("Sì"="YES","Nessuna misura dichiarata"="NONE","Informazione non disponibile"="UNKNOWN")),
      shiny::uiOutput("misure_ui"),
      shiny::selectInput("comunicazioni","Comunicazioni / escalation *",
                        choices=c("Seleziona"="",comunicazioni),selected=vecchio_campo(ultima,"comunicazioni",esito)),
      shiny::textAreaInput("nota_comunicazioni","Comunicazioni e riferimenti *",rows=2,width="100%",value=vecchio_campo(ultima,"nota_comunicazioni",esito)),
      shiny::selectInput("stato_gestione","Stato dichiarato della gestione aziendale *",
                        choices=c("Seleziona"="",stati_gestione))),
    if(esito!="4") shiny::tagList(
      shiny::checkboxGroupInput("evidenze","Riferimenti ed evidenze aziendali (facoltativi)",
                                 choices=evidenze,selected=vecchia_lista(ultima,"evidenze",esito)),
      shiny::textAreaInput("azione_immediata","Eventuale azione immediata già adottata",rows=2,width="100%",value=vecchio_campo(ultima,"azione_immediata",esito))),
    shiny::textAreaInput("riferimento","Riferimento aziendale libero (facoltativo)",rows=2,width="100%",value=vecchio_campo(ultima,"riferimento",esito)),
    if(esito %in% c("2","4")) scelta("indagine","Richiedere un'indagine 5SAN?",si_no,valore=if(!is.null(ultima) && as.character(ultima$esito[[1]])==esito) if(isTRUE(ultima$indagine_richiesta[[1]])) "YES" else "NO" else character(0)),
    if(esito=="3") shiny::div(class="info","Indagine richiesta dal percorso A10.3. La richiesta sarà salvata; l'apertura effettiva non è ancora integrata."),
    if(esito=="5") shiny::div(class="info","Percorso tecnico 5SAN. Non viene creato alcun ticket."),
    if(esito=="1") shiny::div(class="info","Chiusura motivata della segnalazione senza indagine ordinaria.")
  )
}

azione_ui <- function(i, ultima=NULL) {
  az <- tryCatch(decodifica_json(ultima$azioni[[1]])[[i]],error=function(e) NULL)
  v <- function(k) if(is.null(az) || is.null(az[[k]])) "" else as.character(az[[k]])
  shiny::div(class="azione",
  shiny::h4(paste("Misura",i)),
  shiny::selectInput(paste0("azione_tipo_",i),"Tipo di misura *",choices=c("Seleziona"="",tipi_misura),selected=v("tipo")),
  shiny::selectInput(paste0("azione_stato_",i),"Stato dichiarato *",choices=c("Seleziona"="",stati_misura),selected=v("stato")),
  shiny::textAreaInput(paste0("azione_descrizione_",i),"Descrizione concreta *",rows=2,width="100%",value=v("descrizione")))
}

# Un valore precedentemente registrato si ripropone soltanto per lo stesso ramo.
vecchio_campo <- function(ultima, nome, esito=NULL) {
  if (is.null(ultima) || !nome %in% names(ultima)) return("")
  if (!is.null(esito) && as.character(ultima$esito[[1]])!=esito) return("")
  val <- ultima[[nome]][[1]]
  if (is.null(val) || length(val)==0 || is.na(val)) "" else as.character(val)
}

vecchia_lista <- function(ultima, nome, esito=NULL) {
  if (is.null(ultima) || !nome %in% names(ultima) ||
      (!is.null(esito) && as.character(ultima$esito[[1]])!=esito)) return(character())
  tryCatch(unlist(decodifica_json(ultima[[nome]][[1]]),use.names=FALSE),
           error=function(e) character())
}
