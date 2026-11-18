# Interfaccia Near Miss: elenco sempre visibile, modulo guidato in finestra.
# Riproduce i passaggi del prototipo HTML v0.3; nessun dato dimostrativo.
opzioni_menu <- function(catalogo) stats::setNames(names(catalogo), unname(catalogo))

scelta <- function(id, titolo, opzioni, valore=character(0)) {
  shiny::radioButtons(id, paste0(titolo, " *"), choices=opzioni_menu(opzioni),
                      selected=if(length(valore) && nzchar(valore)) valore else character(0))
}

vecchio_campo <- function(ultima, nome, esito=NULL) {
  if(is.null(ultima) || !nome %in% names(ultima)) return("")
  if(!is.null(esito) && as.character(ultima$esito[[1]])!=esito) return("")
  valore <- ultima[[nome]][[1]]
  if(is.null(valore) || length(valore)!=1L || is.na(valore)) "" else as.character(valore)
}

vecchia_lista <- function(ultima, nome, esito=NULL) {
  if(is.null(ultima) || !nome %in% names(ultima) ||
     (!is.null(esito) && as.character(ultima$esito[[1]])!=esito)) return(character())
  tryCatch(as.character(unlist(decodifica_json(ultima[[nome]][[1]]),use.names=FALSE)),
           error=function(e) character())
}

interfaccia <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$meta(name="viewport",content="width=device-width,initial-scale=1"),
    shiny::tags$style(shiny::HTML("
      body{background:#f2f6f8;color:#152c3d;font-family:'Segoe UI',Arial,sans-serif;}
      .container-fluid{max-width:1400px;margin:auto;padding:25px;}
      h1{font-weight:750;color:#102b3e}
      h3{font-size:20px;color:#102b3e}
      .pannello{background:white;border:1px solid #d6e1e7;border-radius:13px;
        padding:23px;margin:12px 0;box-shadow:0 4px 16px rgba(16,43,62,.05);}
      .shiny-input-container{max-width:none}
      .btn-primary{background:#075e64;border-color:#075e64;}
      .btn-primary:hover,.btn-primary:focus{background:#064b50;border-color:#064b50;}
      .avviso{padding:12px;background:#fff5e3;border:1px solid #ecd7b2;
        border-radius:8px;color:#835315;margin:12px 0;}
      .info{padding:12px;background:#eaf4f3;border:1px solid #cde3e1;
        border-radius:8px;color:#244b53;margin:12px 0;}
      .modal-dialog{width:min(1120px,96vw);max-width:1120px;margin:2vh auto;}
      .modal-content{border-radius:15px;overflow:hidden}
      .modal-header{background:#102b3e;color:white;padding:18px 25px}
      .modal-header .close{color:white;opacity:.9}
      .modal-body{max-height:calc(100vh - 215px);overflow:auto;padding:22px 27px}
      .modal-footer{padding:14px 26px;border-top:1px solid #d6e1e7}
      .wizard-layout{display:grid;grid-template-columns:188px minmax(0,1fr);gap:22px}
      .wizard-nav{background:#f5f8f8;border-radius:10px;padding:16px;align-self:start}
      .wizard-nav div{padding:10px 8px;border-bottom:1px solid #d6e1e7;font-size:13px}
      .wizard-nav div.attivo{color:#075e64;background:#e1efed;font-weight:700;
        border-radius:7px;border-bottom:0}
      .wizard-body h3{margin:0 0 14px}
      .wizard-body h4{margin-top:19px;color:#075e64}
      .wizard-body .shiny-input-container{margin-bottom:17px}
      .wizard-body fieldset{padding:0}
      .wizard-body details{border:1px solid #d6e1e7;padding:11px 14px;
        border-radius:9px;margin:9px 0}
      .wizard-body details summary{cursor:pointer;font-weight:600}
      .wizard-body .checkbox,.wizard-body .radio{margin:8px 0}
      .wizard-dati{display:grid;grid-template-columns:1fr 1fr;gap:12px}
      .wizard-dati > div{background:#f3f6f8;padding:11px;border-radius:7px;overflow-wrap:anywhere}
      .wizard-dati small{display:block;color:#526777}
      .azione{background:#fafcfd;border:1px solid #d6e1e7;padding:13px;border-radius:10px;
        margin:12px 0}
      .wizard-footer-note{margin-top:13px;font-size:12px;color:#526777}
      .modal .btn{margin-right:6px}
      @media(max-width:700px){.wizard-layout{grid-template-columns:1fr}
        .wizard-nav{display:flex;overflow-x:auto;padding:8px}
        .wizard-nav div{min-width:130px;border-bottom:0}
        .wizard-dati{grid-template-columns:1fr}
        .modal-body{padding:15px;max-height:calc(100vh - 175px)}}
    "))
  ),
  shiny::titlePanel("5SAN · Verifica preliminare delle segnalazioni"),
  shiny::uiOutput("schermata")
)

interfaccia_lavoro <- function(modalita="internal", intervallo=Sys.Date()+c(-30,0)) {
  shiny::tagList(shiny::div(class="pannello",
    shiny::strong(shiny::textOutput("utente_attivo",inline=TRUE)),
    shiny::h3("Segnalazioni 5SAN"),
    if(modalita=="internal") shiny::fluidRow(
      shiny::column(4,shiny::selectInput("azienda","Azienda:",choices=NULL)),
      shiny::column(4,shiny::selectInput("stabilimento","Stabilimento:",choices=NULL)),
      shiny::column(4,shiny::selectInput("macchina","Macchina:",choices=NULL))
    ),
    shiny::dateRangeInput("date","Periodo:",start=intervallo[1],end=intervallo[2],
      min=intervallo[1],max=intervallo[2],format="dd-mm-yyyy",
      separator=" a ",language="it",weekstart=1),
    shiny::p("Seleziona una segnalazione dalla lista per aprire il modulo completo.",
      style="color:#526777;"),
    shiny::uiOutput("messaggio_elenco"),
    DT::DTOutput("elenco")
  ))
}

sezione_campo <- function(titolo, valore) {
  shiny::div(shiny::tags$small(titolo),shiny::strong(
    if(is.null(valore) || !length(valore) || is.na(valore[[1]]) ||
       !nzchar(as.character(valore[[1]]))) "Non disponibile" else as.character(valore[[1]])))
}

formato_data <- function(data) {
  if(is.null(data) || !length(data) || is.na(data[[1]])) return("Non disponibile")
  format(data[[1]],"%d/%m/%Y %H:%M",tz="Europe/Rome")
}

# Tutte le pagine sono create una volta sola: passando da una fase all'altra
# Shiny non distrugge gli input e non perde le risposte.
finestra_modulo <- function(a,ultima=NULL) {
  esito_salvato <- vecchio_campo(ultima,"esito")
  scelti <- vecchia_lista(ultima,"pericoli","4")
  gruppo_pericoli <- function(id,titolo,codici) shiny::tags$details(
    shiny::tags$summary(titolo),
    shiny::checkboxGroupInput(id,NULL,choices=opzioni_menu(pericoli[codici]),
      selected=intersect(scelti,codici))
  )
  shiny::modalDialog(
    title=shiny::div(
      shiny::strong("Verifica preliminare · ",as.character(a$alert_id[[1]])),
      shiny::div(style="font-size:12px;color:#c3d5df;",
                 "Modello A · cinque percorsi di verifica · 5SAN")
    ),
    shiny::div(class="wizard-layout",
      shiny::div(class="wizard-nav",shiny::uiOutput("indice_modulo")),
      shiny::div(class="wizard-body",
        shiny::uiOutput("errori_modulo"),
        shiny::tabsetPanel(id="passo_modulo",type="hidden",selected="presa",
          shiny::tabPanel("Presa in carico",value="presa",
            shiny::h3("Presa in carico della segnalazione"),
            shiny::p("I dati di 5SAN sono riportati automaticamente e non possono essere modificati."),
            shiny::div(class="wizard-dati",
              sezione_campo("A01 · ID segnalazione",a$alert_id),
              sezione_campo("A02 · Inizio osservazione",formato_data(a$period_start)),
              sezione_campo("A02 · Fine osservazione",formato_data(a$period_end)),
              sezione_campo("A03 · Generazione segnalazione",formato_data(a$generated_at)),
              sezione_campo("A04 · Codice macchina",a$machine_code),
              sezione_campo("A04 · Nome macchina",a$machine_name),
              sezione_campo("A07 · Origine",a$source),
              sezione_campo("Componenti coinvolti",a$sensor_code)
            ),
            shiny::div(class="info",shiny::strong("Descrizione segnalazione"),
              shiny::p(a$signal_description),shiny::p(a$signal_data_text)),
            shiny::div(class="avviso",shiny::strong("A09 · Primo click di invio 5SAN"),
              shiny::p("Non disponibile fino all'integrazione con il portale. Non viene inventata una data.")),
            shiny::h4("A08 · Responsabile della presa in carico"),
            shiny::textInput("compilatore_nome","Nome e cognome di chi compila *",
              value=vecchio_campo(ultima,"operatore_account")),
            shiny::selectInput("responsabile_funzione","Funzione aziendale *",
              choices=c("Seleziona"="", "RSPP / HSE"="RSPP_HSE",
                "Responsabile della manutenzione"="MAINTENANCE",
                "Altro"="OTHER"),
              selected=vecchio_campo(ultima,"responsabile_funzione")),
            shiny::textInput("responsabile_nome","Nome e cognome del responsabile *",
              value=vecchio_campo(ultima,"responsabile_nome")),
            shiny::textInput("responsabile_ruolo","Ruolo del responsabile *",
              value=vecchio_campo(ultima,"responsabile_ruolo")),
            shiny::p("Chi compila e il responsabile indicato possono essere persone diverse.",
              style="color:#526777;font-size:12px;")
          ),
          shiny::tabPanel("Esito",value="esito",
            shiny::h3("A10 · Qual è l'esito della verifica?"),
            shiny::p("Scegli un solo percorso: le sezioni successive cambieranno di conseguenza."),
            scelta("esito","Esito della verifica preliminare",esiti,esito_salvato),
            shiny::div(class="info",
              "Se condizione e pericolo sono già riconosciuti, seleziona il percorso 4 anche se le cause restano da approfondire.")
          ),
          shiny::tabPanel("Verifica",value="verifica",
            shiny::h3("A11 · Riscontro della verifica"),
            shiny::uiOutput("domanda_a11"),
            shiny::textAreaInput("motivazione","Descrivi i fatti e la motivazione *",
              rows=5,width="100%",value=vecchio_campo(ultima,"motivazione",esito_salvato)),
            shiny::conditionalPanel("input.esito === '4'",
              shiny::h4("Condizioni critiche riscontrate"),
              shiny::p("Seleziona solo quanto effettivamente riscontrato. I codici CR sono riferimenti interni 5SAN."),
              shiny::checkboxGroupInput("criticita",NULL,choices=opzioni_menu(criticita),
                selected=vecchia_lista(ultima,"criticita","4")),
              shiny::conditionalPanel("input.criticita && input.criticita.indexOf('CR25') !== -1",
                shiny::textAreaInput("criticita_altro","Specifica l'altra criticità *",
                  rows=2,value=vecchio_campo(ultima,"criticita_altro","4"))),
              shiny::h4("Pericoli individuati"),
              shiny::p("Puoi selezionare pericoli appartenenti a più famiglie."),
              gruppo_pericoli("pericoli_meccanici","Meccanici",
                c("CRUSH","SHEAR","CUT","ENTANGLEMENT","DRAW_IN","IMPACT","PROJECTION","FALL")),
              gruppo_pericoli("pericoli_elettrici","Elettrici",c("ELECTRIC_CONTACT","ARC")),
              gruppo_pericoli("pericoli_termici","Termici",c("HEAT","COLD")),
              gruppo_pericoli("pericoli_sostanze","Sostanze e materiali",
                c("INHALATION","SUBSTANCE_CONTACT")),
              gruppo_pericoli("pericoli_altri","Altre famiglie",
                c("NOISE","VIBRATION","RADIATION","ERGONOMIC","FIRE","OTHER")),
              shiny::conditionalPanel("input.pericoli_altri && input.pericoli_altri.indexOf('OTHER') !== -1",
                shiny::textAreaInput("pericolo_altro","Specifica l'altro pericolo *",rows=2,
                  value=vecchio_campo(ultima,"pericolo_altro","4"))),
              shiny::h4("Elementi utilizzati per la verifica"),
              shiny::checkboxGroupInput("fonti","Seleziona almeno una fonte *",
                choices=opzioni_menu(fonti),selected=vecchia_lista(ultima,"fonti","4")),
              shiny::textAreaInput("fonti_nota","Riferimento al riscontro (facoltativo)",
                rows=2,value=vecchio_campo(ultima,"fonti_nota","4"))
            ),
            shiny::conditionalPanel("input.esito !== '4'",
              shiny::tags$details(shiny::tags$summary("Riferimenti ed evidenze aziendali (facoltativi)"),
                shiny::checkboxGroupInput("evidenze",NULL,choices=opzioni_menu(evidenze),
                  selected=vecchia_lista(ultima,"evidenze",esito_salvato)))),
            shiny::conditionalPanel("input.esito !== '4'",
              shiny::tags$details(shiny::tags$summary("Azione immediata già adottata (facoltativa)"),
                shiny::textAreaInput("azione_immediata","Azione e riferimenti",
                  value=vecchio_campo(ultima,"azione_immediata",esito_salvato),rows=2))),
            shiny::textAreaInput("riferimento","Riferimento aziendale libero (facoltativo)",
              value=vecchio_campo(ultima,"riferimento",esito_salvato),rows=2)
          ),
          shiny::tabPanel("Qualificazione",value="qualificazione",
            shiny::h3("Qualifica soltanto ciò che è stato verificato"),
            shiny::div(class="info","La qualificazione deriva dai fatti, non dalla decisione sull'indagine."),
            scelta("episodio","È stato verificato anche un episodio effettivamente accaduto?",
              si_no_ignoto,vecchio_campo(ultima,"episodio","4")),
            shiny::conditionalPanel("input.episodio === 'YES'",
              scelta("danno","Dall'episodio è derivato un danno fisico a una persona?",
                si_no_ignoto,vecchio_campo(ultima,"danno","4"))),
            shiny::conditionalPanel("input.episodio === 'YES' && input.danno === 'NO'",
              scelta("potenziale","L'episodio avrebbe potuto causare un danno fisico?",
                si_no_ignoto,vecchio_campo(ultima,"potenziale","4"))),
            shiny::uiOutput("qualificazione_proposta"),
            shiny::conditionalPanel(
              "input.episodio === 'UNKNOWN' || (input.episodio === 'YES' && (input.danno === 'UNKNOWN' || (input.danno === 'NO' && input.potenziale !== 'YES')))",
              shiny::textAreaInput("chiarimenti","Limite della ricostruzione / chiarimento *",
                value=vecchio_campo(ultima,"chiarimenti","4"),rows=3))
          ),
          shiny::tabPanel("Risposta e seguito",value="seguito",
            shiny::h3("Risposta aziendale e decisione sul seguito"),
            shiny::conditionalPanel("input.esito === '4'",
              shiny::div(class="avviso","La compilazione non sostituisce gli interventi aziendali urgenti."),
              scelta("misure","Sono state adottate o disposte misure?",
                c("YES"="Sì","NONE"="Nessuna misura dichiarata",
                  "UNKNOWN"="Informazione non disponibile"),
                vecchio_campo(ultima,"misure","4")),
              shiny::conditionalPanel("input.misure === 'YES'",
                shiny::uiOutput("azioni_modulo"),
                shiny::actionButton("aggiungi_misura","+ Aggiungi una misura"),
                shiny::uiOutput("rimuovi_misura")),
              shiny::conditionalPanel("input.misure === 'NONE' || input.misure === 'UNKNOWN'",
                shiny::textAreaInput("nota_misure","Spiegazione / limite delle informazioni *",
                  value=vecchio_campo(ultima,"nota_misure","4"),rows=2)),
              shiny::selectInput("comunicazioni","Comunicazioni / escalation aziendale *",
                choices=c("Seleziona"="",opzioni_menu(comunicazioni)),
                selected=vecchio_campo(ultima,"comunicazioni","4")),
              shiny::textAreaInput("nota_comunicazioni","Comunicazioni e riferimenti *",rows=2,
                value=vecchio_campo(ultima,"nota_comunicazioni","4")),
              shiny::selectInput("stato_gestione","Stato della gestione aziendale *",
                choices=c("Seleziona"="",opzioni_menu(stati_gestione)),
                selected=vecchio_campo(ultima,"stato_gestione","4"))
            ),
            shiny::conditionalPanel("input.esito === '2' || input.esito === '4'",
              scelta("indagine","Richiedere un'indagine 5SAN?",si_no,
                if(!is.null(ultima) && esito_salvato %in% c("2","4"))
                  if(isTRUE(ultima$indagine_richiesta[[1]])) "YES" else "NO"
                else character(0))),
            shiny::conditionalPanel("input.esito === '3'",
              shiny::div(class="info","L'indagine è richiesta dal percorso A10.3. L'apertura reale sarà integrata in seguito.")),
            shiny::conditionalPanel("input.esito === '1'",
              shiny::div(class="info","Chiusura motivata della segnalazione, senza indagine ordinaria.")),
            shiny::conditionalPanel("input.esito === '5'",
              shiny::div(class="info","Percorso tecnico 5SAN; nessun ticket viene creato automaticamente."))
          ),
          shiny::tabPanel("Riepilogo",value="riepilogo",
            shiny::h3("Rileggi i dati prima dell'invio"),
            shiny::uiOutput("riepilogo_dettagliato"),
            shiny::div(class="info", "L'invio registra una nuova revisione di tutti i dati del percorso attivo.",
              " Non equivale a una certificazione della sicurezza della macchina.")
          )
        )
      )
    ),
    footer=shiny::div(
      shiny::actionButton("chiudi_modulo","Chiudi"),
      shiny::actionButton("indietro_modulo","Indietro"),
      shiny::conditionalPanel("input.passo_modulo !== 'riepilogo'",
        shiny::actionButton("avanti_modulo","Continua",class="btn-primary")),
      shiny::conditionalPanel("input.passo_modulo === 'riepilogo'",
        shiny::actionButton("invia","Invia modulo",class="btn-primary"))
    ),
    size="l",easyClose=FALSE
  )
}

azione_ui <- function(i,ultima=NULL,correnti=list()) {
  salvata <- tryCatch(decodifica_json(ultima$azioni[[1]])[[i]],
                      error=function(e) NULL)
  valore <- function(campo) {
    id <- paste0("azione_",campo,"_",i)
    corrente <- correnti[[id]]
    if(!is.null(corrente) && length(corrente) && nzchar(as.character(corrente)))
      return(as.character(corrente))
    if(!is.null(salvata) && !is.null(salvata[[campo]]))
      return(as.character(salvata[[campo]]))
    ""
  }
  shiny::div(class="azione",
    shiny::h4(paste0("Misura ",i)),
    shiny::selectInput(paste0("azione_tipo_",i),"Tipo di misura *",
      choices=c("Seleziona"="",opzioni_menu(tipi_misura)),selected=valore("tipo")),
    shiny::selectInput(paste0("azione_stato_",i),"Stato dichiarato *",
      choices=c("Seleziona"="",opzioni_menu(stati_misura)),selected=valore("stato")),
    shiny::textAreaInput(paste0("azione_descrizione_",i),"Descrizione concreta *",rows=2,
      value=valore("descrizione"))
  )
}
