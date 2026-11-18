# La sessione conserva solo stato temporaneo. I dati ufficiali sono nel DB.
server <- function(input, output, session) {
  # Modalità interna uguale alla dashboard: accesso gestito dalla rete/IP,
  # non da una seconda schermata di login. Docker resta su 127.0.0.1.
  # Modalità web: bloccata finché il portale non passa un'identità verificata.
  modalita <- tolower(Sys.getenv("APP_MODE", "internal"))
  selezionata <- shiny::reactiveVal(NULL)
  precedente <- shiny::reactiveVal(NULL)
  errore <- shiny::reactiveVal(NULL)
  numero_azioni <- shiny::reactiveVal(1L)
  lista <- shiny::reactiveVal(data.frame())
  errore_lista <- shiny::reactiveVal(NULL)
  con <- tryCatch(connessione_db(),error=function(e) {
    warning(conditionMessage(e)); NULL
  })
  session$onSessionEnded(function() {
    if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  })
  anagrafica <- if (identical(modalita, "internal") && !is.null(con)) {
    tryCatch(anagrafica_macchine(con), error=function(e) {
      warning(conditionMessage(e)); data.frame()
    })
  } else data.frame()
  coupon_interni <- if (nrow(anagrafica)) as.character(anagrafica$coupon) else character()
  utente <- shiny::reactiveVal(if (identical(modalita, "internal") &&
                                 !is.null(con) && length(coupon_interni)) {
    list(account="accesso_interno",coupon=coupon_interni)
  } else NULL)
  intervallo <- if (!is.null(con) && length(coupon_interni))
    tryCatch(intervallo_segnalazioni(con),error=function(e) Sys.Date()+c(-30,0))
  else Sys.Date()+c(-30,0)
  output$schermata <- shiny::renderUI({
    if (is.null(utente())) return(shiny::div(class="pannello",
      shiny::h3("Applicazione non disponibile"),
      shiny::p("Connessione assente o accesso web non autorizzato.")))
    interfaccia_lavoro(modalita,intervallo)
  })
  output$utente_attivo <- shiny::renderText({
    req(utente())
    paste0("Accesso interno · ",length(utente()$coupon)," macchine disponibili")
  })
  shiny::observeEvent(input$azienda, {
    req(nrow(anagrafica),input$azienda)
    stabilimenti <- sort(unique(anagrafica$stabilimento[anagrafica$azienda==input$azienda]))
    shiny::updateSelectInput(session,"stabilimento",choices=stabilimenti,
      selected=if(input$stabilimento %in% stabilimenti) input$stabilimento else stabilimenti[1])
  })
  shiny::observeEvent(list(input$azienda,input$stabilimento),{
    req(nrow(anagrafica),input$azienda,input$stabilimento)
    d <- anagrafica[anagrafica$azienda==input$azienda &
                    anagrafica$stabilimento==input$stabilimento,,drop=FALSE]
    choices <- stats::setNames(d$coupon,paste(d$progetto,d$nome_macchina,sep=" - "))
    shiny::updateSelectInput(session,"macchina",choices=choices,
      selected=if(input$macchina %in% d$coupon) input$macchina else d$coupon[1])
  })
  shiny::observeEvent(utente(),{
    if (!is.null(utente()) && nrow(anagrafica)) {
      shiny::updateSelectInput(session,"azienda",
        choices=sort(unique(anagrafica$azienda)))
    }
  })
  filtri_correnti <- shiny::reactive({
    req(utente(),input$date)
    if (modalita=="internal") {
      req(input$azienda,input$stabilimento,input$macchina)
      req(nrow(anagrafica)>0)
      req(any(anagrafica$coupon==input$macchina &
              anagrafica$azienda==input$azienda &
              anagrafica$stabilimento==input$stabilimento))
      coupon <- input$macchina
    } else {
      req(FALSE) # Modalità web disabilitata finché il portale non autentica il coupon
    }
    list(coupon=coupon,date=input$date)
  })
  shiny::observeEvent(filtri_correnti(),{
    f <- filtri_correnti()
    tryCatch({
      d <- elenco_segnalazioni(con,f$coupon,f$date)
      lista(d)
      errore_lista(NULL)
    }, error=function(e) {
      errore_lista(conditionMessage(e))
      lista(data.frame())
      warning(conditionMessage(e))
    })
  })
  output$messaggio_elenco <- shiny::renderUI({
    if (!is.null(errore_lista()))
      shiny::div(class="avviso",paste("Errore nel caricamento:",errore_lista()))
  })

  output$elenco <- DT::renderDT({
    d <- lista()
    if (!nrow(d)) return(DT::datatable(data.frame(Messaggio="Nessuna segnalazione nel periodo selezionato"),rownames=FALSE,options=list(dom="t")))
    v <- d[,c("alert_id","coupon","machine_name","generated_at",
             "alert_type","signal_description","ultima_revisione")]
    names(v) <- c("ID segnalazione","Coupon","Macchina","Generata",
                  "Tipo","Descrizione","Revisione")
    v$Generata <- format(v$Generata,"%d/%m/%Y %H:%M",tz="Europe/Rome")
    DT::datatable(v,selection="single",rownames=FALSE,
      options=list(pageLength=10,scrollX=TRUE))
  },server=TRUE)

  shiny::observeEvent(input$elenco_rows_selected,{
    req(utente(),con)
    d <- lista()
    indice <- input$elenco_rows_selected
    if (length(indice)!=1 || indice<1 || indice>nrow(d)) return()
    id <- as.character(d$alert_id[[indice]])
    tryCatch({
      a <- leggi_segnalazione(con,id,utente()$coupon)
      p <- ultima_verifica(con,id)
      numero_azioni(if(is.null(p)) 1L else max(1L,length(decodifica_json(p$azioni[[1]]))))
      precedente(p)
      selezionata(a)
      errore(NULL)
    },error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
  })

  output$dettaglio_modulo <- shiny::renderUI({
    req(utente())
    a <- selezionata()
    if (is.null(a)) return(shiny::div(class="pannello","Seleziona una segnalazione dall'elenco per compilare o aggiornare la verifica."))
    modulo_ui(a[1,],precedente())
  })
  output$percorso <- shiny::renderUI({
    req(selezionata())
    interfaccia_percorso(input$esito,ultima=precedente())
  })
  output$altra_criticita <- shiny::renderUI({
    if ("CR25" %in% input$criticita)
      shiny::textAreaInput("criticita_altro","Specifica l'altra criticità *",rows=2,width="100%")
  })
  output$altro_pericolo <- shiny::renderUI({
    if ("OTHER" %in% input$pericoli)
      shiny::textAreaInput("pericolo_altro","Specifica l'altro pericolo *",rows=2,width="100%")
  })
  output$domande_episodio <- shiny::renderUI({
    if (identical(input$episodio,"YES"))
      shiny::tagList(
        scelta("danno","Dall'episodio è derivato un danno fisico a una persona?",
               si_no_ignoto,valore=vecchio_campo(precedente(),"danno","4")),
        shiny::uiOutput("potenziale_ui"))
  })
  output$potenziale_ui <- shiny::renderUI({
    if (identical(input$episodio,"YES") && identical(input$danno,"NO"))
      scelta("potenziale","L'episodio avrebbe potuto causare un danno fisico?",
             si_no_ignoto,valore=vecchio_campo(precedente(),"potenziale","4"))
  })
  output$proposta_qualificazione <- shiny::renderUI({
    if (is.null(input$episodio) || !nzchar(input$episodio)) return(NULL)
    q <- qualifica(input$episodio,
                  if(is.null(input$danno)) "" else input$danno,
                  if(is.null(input$potenziale)) "" else input$potenziale)
    shiny::div(class="info",paste("Qualificazione proposta:",q),
      shiny::p("La qualificazione deriva dai fatti dichiarati e non dalla scelta di aprire un'indagine."))
  })
  output$chiarimenti_ui <- shiny::renderUI({
    if (is.null(input$episodio) || !nzchar(input$episodio)) return(NULL)
    q <- qualifica(input$episodio,
                   if(is.null(input$danno)) "" else input$danno,
                   if(is.null(input$potenziale)) "" else input$potenziale)
    if(q %in% c("UNDETERMINED","REVIEW_NEEDED"))
      shiny::textAreaInput("chiarimenti","Limite della ricostruzione / chiarimento necessario *",rows=2,width="100%",value=vecchio_campo(precedente(),"chiarimenti","4"))
  })
  output$misure_ui <- shiny::renderUI({
    if (identical(input$misure,"YES"))
      shiny::tagList(
        lapply(seq_len(numero_azioni()),function(i) azione_ui(i,precedente(),shiny::isolate(shiny::reactiveValuesToList(input)))),
        shiny::actionButton("aggiungi_misura","+ Aggiungi una misura"),
        if(numero_azioni()>1L) shiny::actionButton("togli_misura","Rimuovi l'ultima misura"))
    else if (isTRUE(input$misure %in% c("NONE","UNKNOWN")))
      shiny::textAreaInput("nota_misure","Spiegazione / limite delle informazioni *",rows=2,width="100%",value=vecchio_campo(precedente(),"nota_misure","4"))
  })
  shiny::observeEvent(input$aggiungi_misura,numero_azioni(min(20L,numero_azioni()+1L)))
  shiny::observeEvent(input$togli_misura,numero_azioni(max(1L,numero_azioni()-1L)))
  output$riepilogo <- shiny::renderUI({
    req(selezionata())
    if (is.null(input$esito) || !input$esito %in% names(esiti))
      return(shiny::p("Completa A08, A10 e A11 prima di inviare."))
    shiny::tagList(
      shiny::p(shiny::strong("Segnalazione: "),selezionata()$alert_id),
      shiny::p(shiny::strong("Esito: "),unname(esiti[[input$esito]])),
      if (input$esito %in% c("2","4")) shiny::p("Indagine richiesta: ",
        if (identical(input$indagine,"YES")) "Sì" else if(identical(input$indagine,"NO")) "No" else "Da indicare"),
      if (input$esito=="3") shiny::p("Indagine richiesta automaticamente dal percorso."),
      shiny::p("Una nuova revisione sostituisce la precedente nelle analisi correnti, conservandola nello storico.")
    )
  })
  estrai <- function() {
    campo <- function(nome) {
      v <- input[[nome]]
      if (is.null(v) || !length(v)) "" else v
    }
    n <- if(identical(campo("misure"),"YES")) numero_azioni() else 0L
    azioni <- lapply(seq_len(n),function(i) list(
      tipo=campo(paste0("azione_tipo_",i)),
      stato=campo(paste0("azione_stato_",i)),
      descrizione=campo(paste0("azione_descrizione_",i))))
    x <- list(
      compilatore_nome=campo("compilatore_nome"),
      responsabile_funzione=campo("responsabile_funzione"),
      responsabile_nome=campo("responsabile_nome"),responsabile_ruolo=campo("responsabile_ruolo"),
      esito=campo("esito"), motivazione=campo("motivazione"),
      indagine=campo("indagine"), criticita=vettore(campo("criticita")),
      criticita_altro=campo("criticita_altro"),
      pericoli=vettore(campo("pericoli")),pericolo_altro=campo("pericolo_altro"),
      fonti=vettore(campo("fonti")),fonti_nota=campo("fonti_nota"),
      evidenze=vettore(campo("evidenze")),riferimento=campo("riferimento"),
      azione_immediata=campo("azione_immediata"),
      episodio=campo("episodio"),danno=campo("danno"),potenziale=campo("potenziale"),
      chiarimenti=campo("chiarimenti"),misure=campo("misure"),azioni=azioni,
      nota_misure=campo("nota_misure"),comunicazioni=campo("comunicazioni"),
      nota_comunicazioni=campo("nota_comunicazioni"),stato_gestione=campo("stato_gestione")
    )
    ripulisci_percorso(x)
  }
  shiny::observeEvent(input$invia,{
    req(utente(),selezionata(),con)
    dati <- estrai()
    tryCatch({
      valida_modulo(dati)
      attesa <- if(is.null(precedente())) 0L else as.integer(precedente()$revisione[[1]])
      risultato <- salva_verifica(con,selezionata(),dati$compilatore_nome,utente()$coupon,attesa,dati)
      precedente(ultima_verifica(con,as.character(selezionata()$alert_id[[1]])))
      carica_lista()
      shiny::showModal(shiny::modalDialog(
        title="Modulo registrato",paste("Verifica salvata. Revisione",risultato$revisione[[1]],
        "· ID registrazione",risultato$id[[1]]),easyClose=TRUE))
    },error=function(e) shiny::showModal(shiny::modalDialog(
      title="Impossibile inviare",shiny::tags$pre(conditionMessage(e)),easyClose=TRUE)))
  })
}

decodifica_json <- function(x) {
  if(is.null(x) || !nzchar(as.character(x))) return(list())
  jsonlite::fromJSON(as.character(x),simplifyVector=FALSE)
}
