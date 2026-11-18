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

  # Apertura da dashboard: ?id_segnalazione=...
  # Solo in modalità interna già autorizzata; la querystring non assegna permessi.
  id_aperto <- shiny::reactiveVal("")
  shiny::observeEvent(utente(), {
    if (is.null(utente()) || modalita!="internal") return()
    query <- shiny::parseQueryString(if (is.null(session$clientData$url_search)) "" else session$clientData$url_search)
    id <- query[["id_segnalazione"]]
    if (is.null(id) || length(id)!=1L || !nzchar(id) || nchar(id)>40L) return()
    if (identical(id_aperto(),id)) return()
    tryCatch({
      a <- leggi_segnalazione(con,id,utente()$coupon)
      p <- ultima_verifica(con,id)
      precedente(p)
      numero_azioni(if(is.null(p)) 1L else max(1L,length(decodifica_json(p$azioni[[1]]))))
      selezionata(a)
      id_aperto(id)
      # Allinea la vista senza alterare la selezione della segnalazione.
      info <- anagrafica[anagrafica$coupon==as.character(a$coupon[[1]]),,drop=FALSE]
      if(nrow(info)) {
        shiny::updateSelectInput(session,"azienda",selected=info$azienda[1])
        shiny::updateSelectInput(session,"stabilimento",selected=info$stabilimento[1])
        shiny::updateSelectInput(session,"macchina",selected=info$coupon[1])
      }
    },error=function(e) warning(conditionMessage(e)))
  },once=TRUE)


  output$elenco <- DT::renderDT({
    d <- lista()
    if (!nrow(d)) return(DT::datatable(
      data.frame(Messaggio="Nessuna segnalazione nel periodo selezionato"),
      rownames=FALSE,options=list(dom="t")))
    v <- d[,c("alert_id","machine_name","period_start","period_end",
             "alert_type","signal_description","ultima_revisione")]
    names(v) <- c("ID segnalazione","Macchina","Inizio","Fine","Tipo","Descrizione","Revisione")
    for(k in c("Inizio","Fine"))
      v[[k]] <- format(v[[k]],"%d/%m/%Y %H:%M",tz="Europe/Rome")
    DT::datatable(v,selection="single",rownames=FALSE,
      options=list(pageLength=10,scrollX=TRUE,language=list(
        search="Cerca nella lista:",lengthMenu="Mostra _MENU_ segnalazioni",
        info="Segnalazioni _START_–_END_ di _TOTAL_",
        zeroRecords="Nessuna corrispondenza",paginate=list(
          previous="Precedente",next="Successiva"))))
  },server=TRUE)

  passi <- c("presa","esito","verifica","qualificazione","seguito","riepilogo")
  indice_passo <- shiny::reactiveVal(1L)
  numero_azioni <- shiny::reactiveVal(1L)
  azioni_salvate_sessione <- shiny::reactiveVal(list())
  segnalazione_aperta <- shiny::reactiveVal(NULL)

  apri_modulo <- function(id) {
    req(utente(),con)
    tryCatch({
      a <- leggi_segnalazione(con,id,utente()$coupon)
      p <- ultima_verifica(con,id)
      precedente(p)
      selezionata(a)
      azioni_precedenti <- tryCatch(decodifica_json(p$azioni[[1]]),
                                     error=function(e) list())
      numero_azioni(max(1L,length(azioni_precedenti)))
      azioni_salvate_sessione(list())
      indice_passo(1L)
      segnalazione_aperta(id)
      shiny::showModal(finestra_modulo(a[1,,drop=FALSE],p))
    },error=function(e)
      shiny::showNotification(conditionMessage(e),type="error",duration=NULL))
  }

  shiny::observeEvent(input$elenco_rows_selected,{
    d <- lista()
    i <- input$elenco_rows_selected
    if(length(i)==1L && i>=1 && i<=nrow(d)) apri_modulo(as.character(d$alert_id[i]))
  })

  # Apertura per ID proveniente da dashboard, con controllo autorizzazione server.
  shiny::observeEvent(utente(), {
    if(is.null(utente()) || modalita!="internal") return()
    q <- shiny::parseQueryString(
      if(is.null(session$clientData$url_search)) "" else session$clientData$url_search)
    id <- q[["id_segnalazione"]]
    if(is.null(id) || length(id)!=1L || !nzchar(id) || nchar(id)>40L) return()
    apri_modulo(id)
  },once=TRUE)

  output$indice_modulo <- shiny::renderUI({
    e <- input$esito
    nomi <- c("1 · Presa in carico","2 · Esito A10",
              "3 · Fatti e riscontri", "4 · Qualificazione",
              "5 · Risposta e seguito","6 · Riepilogo")
    nomi <- nomi[c(TRUE,TRUE,TRUE,identical(e,"4"),TRUE,TRUE)]
    step <- indice_passo()
    shiny::tagList(shiny::h4("Passaggi"),
      lapply(seq_along(nomi),function(i)
        shiny::div(class=if(i==step)"attivo" else "",nomi[i])))
  })
  passi_attivi <- function() {
    if(identical(input$esito,"4")) passi else passi[passi!="qualificazione"]
  }
  output$domanda_a11 <- shiny::renderUI({
    if(is.null(input$esito) || !input$esito %in% names(domande_a11)) return(NULL)
    shiny::div(class="info",domande_a11[[input$esito]])
  })
  output$qualificazione_proposta <- shiny::renderUI({
    if(is.null(input$episodio) || !nzchar(input$episodio)) return(NULL)
    q <- qualifica(input$episodio, if(is.null(input$danno)) "" else input$danno,
      if(is.null(input$potenziale)) "" else input$potenziale)
    etichette <- c(NC="Non conformità / situazione di pericolo",
      NEAR_MISS="Near miss - mancato infortunio",
      INJURY_EVENT="Evento con danno alla persona",
      REVIEW_NEEDED="Criticità da riesaminare",
      UNDETERMINED="Criticità riconosciuta, episodio non determinabile")
    shiny::div(class="info",shiny::strong("Qualificazione proposta: "),
      unname(etichette[[q]]))
  })

  cattura_azioni <- function() {
    current <- azioni_salvate_sessione()
    for (i in seq_len(numero_azioni())) {
      if(length(current)<i) current[[i]] <- list()
      for(k in c("tipo","stato","descrizione")) {
        v <- input[[paste0("azione_",k,"_",i)]]
        if(!is.null(v)) current[[i]][[k]] <- v
      }
    }
    current
  }
  output$azioni_modulo <- shiny::renderUI({
    n <- numero_azioni()
    p <- precedente()
    local <- azioni_salvate_sessione()
    shiny::tagList(lapply(seq_len(n),function(i) {
      valori <- list()
      if(length(local)>=i && length(local[[i]])) {
        for(k in c("tipo","stato","descrizione"))
          valori[[paste0("azione_",k,"_",i)]] <- local[[i]][[k]]
      }
      azione_ui(i,p,valori)
    }))
  })
  output$rimuovi_misura <- shiny::renderUI({
    if(numero_azioni()>1L) shiny::actionButton("togli_misura","Rimuovi ultima misura")
  })
  shiny::observeEvent(input$aggiungi_misura,{
    azioni_salvate_sessione(cattura_azioni())
    numero_azioni(min(20L,numero_azioni()+1L))
  })
  shiny::observeEvent(input$togli_misura,{
    local <- cattura_azioni()
    if(length(local)) local <- head(local,-1L)
    azioni_salvate_sessione(local)
    numero_azioni(max(1L,numero_azioni()-1L))
  })

  campo <- function(nome) {
    v <- input[[nome]]
    if(is.null(v) || !length(v)) "" else v
  }
  estrai <- function() {
    gruppi <- c("pericoli_meccanici","pericoli_elettrici","pericoli_termici",
                "pericoli_sostanze","pericoli_altri")
    pericoli_scelti <- unique(unlist(lapply(gruppi,function(k)campo(k)),
      use.names=FALSE))
    n <- if(identical(campo("misure"),"YES") && identical(campo("esito"),"4"))
      numero_azioni() else 0L
    azioni <- lapply(seq_len(n),function(i) list(
      tipo=campo(paste0("azione_tipo_",i)),
      stato=campo(paste0("azione_stato_",i)),
      descrizione=campo(paste0("azione_descrizione_",i))))
    x <- list(
      compilatore_nome=campo("compilatore_nome"),
      responsabile_funzione=campo("responsabile_funzione"),
      responsabile_nome=campo("responsabile_nome"),
      responsabile_ruolo=campo("responsabile_ruolo"),
      esito=campo("esito"),motivazione=campo("motivazione"),
      indagine=campo("indagine"),criticita=as.character(campo("criticita")),
      criticita_altro=campo("criticita_altro"),
      pericoli=as.character(pericoli_scelti),pericolo_altro=campo("pericolo_altro"),
      fonti=as.character(campo("fonti")),fonti_nota=campo("fonti_nota"),
      evidenze=as.character(campo("evidenze")),riferimento=campo("riferimento"),
      azione_immediata=campo("azione_immediata"),
      episodio=campo("episodio"),danno=campo("danno"),potenziale=campo("potenziale"),
      chiarimenti=campo("chiarimenti"),misure=campo("misure"),azioni=azioni,
      nota_misure=campo("nota_misure"),comunicazioni=campo("comunicazioni"),
      nota_comunicazioni=campo("nota_comunicazioni"),
      stato_gestione=campo("stato_gestione"))
    ripulisci_percorso(x)
  }

  validazione_passo <- function(passo) {
    x <- estrai()
    if(passo=="presa") {
      campi <- c(compilatore_nome="Chi compila",responsabile_nome="Nome responsabile",
        responsabile_ruolo="Ruolo responsabile",responsabile_funzione="Funzione aziendale")
      for(n in names(campi)) if(!nzchar(testo(x[[n]])))
        stop("Compila: ",unname(campi[n]),call.=FALSE)
    }
    if(passo=="esito" && !x$esito %in% names(esiti))
      stop("Seleziona uno dei cinque esiti.",call.=FALSE)
    if(passo=="verifica") {
      if(!nzchar(testo(x$motivazione))) stop("Compila A11: motivazione.",call.=FALSE)
      if(x$esito=="4" && (!length(x$criticita) || !length(x$pericoli) ||
                         !length(x$fonti)))
        stop("Indica condizioni critiche, pericoli e fonti del riscontro.",call.=FALSE)
      if(x$esito=="4" && "CR25" %in% x$criticita &&
         !nzchar(testo(x$criticita_altro))) stop("Specifica l'altra criticità.",call.=FALSE)
      if(x$esito=="4" && "OTHER" %in% x$pericoli &&
         !nzchar(testo(x$pericolo_altro))) stop("Specifica l'altro pericolo.",call.=FALSE)
    }
    if(passo=="qualificazione" && x$esito=="4") {
      if(!valido(x$episodio,names(si_no_ignoto))) stop("Indica se è avvenuto un episodio.",call.=FALSE)
      if(x$episodio=="YES" && !valido(x$danno,names(si_no_ignoto)))
        stop("Indica se vi è stato danno.",call.=FALSE)
      if(x$episodio=="YES" && x$danno=="NO" &&
         !valido(x$potenziale,names(si_no_ignoto)))
        stop("Indica il potenziale lesivo.",call.=FALSE)
      if(qualifica(x$episodio,x$danno,x$potenziale) %in%
         c("UNDETERMINED","REVIEW_NEEDED") && !nzchar(testo(x$chiarimenti)))
        stop("Spiega il limite della ricostruzione.",call.=FALSE)
    }
    if(passo=="seguito") valida_modulo(x)
    TRUE
  }
  messaggio_errore <- shiny::reactiveVal(NULL)
  output$errori_modulo <- shiny::renderUI({
    if(!is.null(messaggio_errore()))
      shiny::div(class="avviso",shiny::strong("Prima di continuare"),
        shiny::tags$pre(messaggio_errore()))
  })

  shiny::observeEvent(input$avanti_modulo,{
    percorso <- passi_attivi()
    indice <- indice_passo()
    if(indice>=length(percorso)) return()
    tryCatch({
      validazione_passo(percorso[indice])
      messaggio_errore(NULL)
      indice_passo(indice+1L)
      shiny::updateTabsetPanel(session,"passo_modulo",selected=percorso[indice+1L])
    },error=function(e) messaggio_errore(conditionMessage(e)))
  })
  shiny::observeEvent(input$indietro_modulo,{
    percorso <- passi_attivi()
    indice <- indice_passo()
    if(indice<=1L) return()
    indice_passo(indice-1L)
    messaggio_errore(NULL)
    shiny::updateTabsetPanel(session,"passo_modulo",selected=percorso[indice-1L])
  })
  shiny::observeEvent(input$chiudi_modulo,{
    shiny::removeModal()
    selezionata(NULL)
    precedente(NULL)
    segnalazione_aperta(NULL)
    messaggio_errore(NULL)
    indice_passo(1L)
  })

  output$riepilogo_dettagliato <- shiny::renderUI({
    req(selezionata())
    x <- estrai()
    elenco <- list(
      "Segnalazione"=as.character(selezionata()$alert_id[[1]]),
      "Macchina"=as.character(selezionata()$machine_name[[1]]),
      "Compilatore"=testo(x$compilatore_nome),
      "Responsabile"=testo(x$responsabile_nome),
      "Esito"=if(x$esito %in% names(esiti)) unname(esiti[[x$esito]]) else "",
      "Motivazione"=testo(x$motivazione),
      "Qualificazione"=testo(x$qualificazione),
      "Condizioni critiche"=paste(x$criticita,collapse=", "),
      "Pericoli"=paste(x$pericoli,collapse=", "),
      "Fonti"=paste(x$fonti,collapse=", "),
      "Misure"=testo(x$misure),
      "Numero azioni"=as.character(length(x$azioni)),
      "Comunicazioni"=testo(x$comunicazioni),
      "Stato gestione"=testo(x$stato_gestione),
      "Indagine richiesta"=if(isTRUE(x$indagine_richiesta)) "Sì" else "No")
    shiny::tagList(lapply(names(elenco),function(k) shiny::div(
      style="padding:7px;border-bottom:1px solid #edf0f2;",
      shiny::strong(paste0(k,": ")),if(nzchar(elenco[[k]])) elenco[[k]] else "Non applicabile")))
  })

  shiny::observeEvent(input$invia,{
    req(utente(),selezionata(),con)
    tryCatch({
      if(!identical(passi_attivi()[indice_passo()],"riepilogo"))
        stop("Raggiungi il riepilogo prima dell'invio.")
      dati <- estrai()
      valida_modulo(dati)
      attesa <- if(is.null(precedente())) 0L else as.integer(precedente()$revisione[[1]])
      risultato <- salva_verifica(con,selezionata(),dati$compilatore_nome,
                                 utente()$coupon,attesa,dati)
      precedente(ultima_verifica(con,as.character(selezionata()$alert_id[[1]])))
      lista(elenco_segnalazioni(con,filtri_correnti()$coupon,filtri_correnti()$date))
      shiny::removeModal()
      segnalazione_aperta(NULL)
      shiny::showModal(shiny::modalDialog(
        title="Verifica registrata",
        paste("Modulo salvato. Revisione",risultato$revisione[[1]],
              "· Registrazione",risultato$id[[1]]),easyClose=TRUE))
    },error=function(e) messaggio_errore(conditionMessage(e)))
  })
}

decodifica_json <- function(x) {
  if(is.null(x) || !length(x) || is.na(x[[1]])) return(list())
  jsonlite::fromJSON(as.character(x[[1]]),simplifyVector=FALSE)
}
