# Regole di business del prototipo v0.3. UNKNOWN non equivale a NO.
codifica_json <- function(x) as.character(jsonlite::toJSON(
  x, auto_unbox = FALSE, null = "null", na = "null", POSIXt = "ISO8601", digits = NA))

testo <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) "" else trimws(as.character(x[[1]]))
vettore <- function(x) if (is.null(x)) character() else as.character(x)
valido <- function(x, scelte) length(x) == 1L && !is.na(x) && x %in% scelte

qualifica <- function(episodio, danno, potenziale) {
  if (episodio == "NO") return("NC")
  if (episodio != "YES") return("UNDETERMINED")
  if (danno == "YES") return("INJURY_EVENT")
  if (danno != "NO") return("UNDETERMINED")
  if (potenziale == "YES") return("NEAR_MISS")
  if (potenziale == "NO") return("REVIEW_NEEDED")
  "UNDETERMINED"
}

valida_modulo <- function(x) {
  errori <- character()
  obbligo <- function(valore, etichetta) {
    if (!nzchar(testo(valore))) errori <<- c(errori, paste0("Compila: ", etichetta))
  }
  obbligo(x$compilatore_nome, "A08 · Nome compilatore")
  obbligo(x$responsabile_nome, "A08 · Nome responsabile")
  obbligo(x$responsabile_ruolo, "A08 · Ruolo responsabile")
  obbligo(x$responsabile_funzione, "A08 · Funzione scelta")
  obbligo(x$motivazione, "A11 · Motivazione")
  if (!valido(x$esito, names(esiti))) errori <- c(errori, "Seleziona uno dei cinque esiti A10.")
  e <- testo(x$esito)
  if (e %in% c("2","4") && !valido(x$indagine, c("YES","NO")))
    errori <- c(errori, "Indica se richiedere un'indagine.")
  if (e == "4") {
    if (!length(x$criticita) || !all(x$criticita %in% names(criticita))) errori <- c(errori,"Seleziona almeno una criticità valida.")
    if ("CR25" %in% x$criticita) obbligo(x$criticita_altro,"Altra criticità")
    if (!length(x$pericoli) || !all(x$pericoli %in% names(pericoli))) errori <- c(errori,"Seleziona almeno un pericolo valido.")
    if ("OTHER" %in% x$pericoli) obbligo(x$pericolo_altro,"Altro pericolo")
    if (!length(x$fonti) || !all(x$fonti %in% names(fonti))) errori <- c(errori,"Seleziona almeno una fonte valida.")
    if (!valido(x$episodio, names(si_no_ignoto))) errori <- c(errori,"Indica se è avvenuto un episodio.")
    if (x$episodio == "YES" && !valido(x$danno, names(si_no_ignoto))) errori <- c(errori,"Indica se vi è stato danno fisico.")
    if (x$episodio == "YES" && x$danno == "NO" && !valido(x$potenziale, names(si_no_ignoto)))
      errori <- c(errori,"Indica il potenziale lesivo.")
    if (qualifica(x$episodio,x$danno,x$potenziale) %in% c("UNDETERMINED","REVIEW_NEEDED"))
      obbligo(x$chiarimenti,"Limite della ricostruzione")
    if (!valido(x$misure,c("YES","NONE","UNKNOWN"))) errori <- c(errori,"Indica se esistono misure dichiarate.")
    if (x$misure == "YES") {
      if (!length(x$azioni)) errori <- c(errori,"Inserisci almeno una misura.")
      for (a in x$azioni) {
        if (!valido(a$tipo,names(tipi_misura)) || !valido(a$stato,names(stati_misura)) || !nzchar(testo(a$descrizione)))
          errori <- c(errori,"Completa tipo, stato e descrizione delle misure.")
      }
    } else if (x$misure %in% c("NONE","UNKNOWN")) obbligo(x$nota_misure,"Spiegazione misure")
    if (!valido(x$comunicazioni,names(comunicazioni))) errori <- c(errori,"Seleziona lo stato delle comunicazioni.")
    obbligo(x$nota_comunicazioni,"Riferimento alle comunicazioni")
    if (!valido(x$stato_gestione,names(stati_gestione))) errori <- c(errori,"Indica lo stato della gestione.")
    if (nzchar(x$episodio) && valido(x$episodio,names(si_no_ignoto))) {
      if (!identical(x$qualificazione,qualifica(x$episodio,x$danno,x$potenziale)))
        errori <- c(errori,"Qualificazione incoerente con i fatti dichiarati.")
    }
  }
  if (e=="3" && !isTRUE(x$indagine_richiesta)) errori <- c(errori,"Il percorso A10.3 richiede la richiesta d'indagine.")
  if (e %in% c("1","5") && isTRUE(x$indagine_richiesta)) errori <- c(errori,"Il percorso selezionato non prevede indagine.")
  if (e %in% c("2","4") && !identical(x$indagine_richiesta,x$indagine=="YES")) errori <- c(errori,"Scelta d'indagine incoerente.")
  if (length(errori)) stop(paste(unique(errori),collapse="\n"), call.=FALSE)
  invisible(TRUE)
}

ripulisci_percorso <- function(x) {
  e <- x$esito
  x$indagine_richiesta <- if (e=="3") TRUE else e %in% c("2","4") && x$indagine=="YES"
  if (e!="4") {
    for (k in c("criticita","pericoli","fonti","azioni")) x[[k]] <- list()
    for (k in c("criticita_altro","pericolo_altro","fonti_nota","episodio","danno",
                "potenziale","chiarimenti","misure","nota_misure","comunicazioni",
                "nota_comunicazioni","stato_gestione","qualificazione")) x[[k]] <- NULL
  } else {
    x$qualificazione <- qualifica(x$episodio,x$danno,x$potenziale)
    x$azione_immediata <- NULL
    if (x$episodio!="YES") { x$danno <- NULL; x$potenziale <- NULL }
    else if (x$danno!="NO") x$potenziale <- NULL
    if (x$misure!="YES") x$azioni <- list()
    else x$nota_misure <- NULL
    if (!"CR25" %in% x$criticita) x$criticita_altro <- NULL
    if (!"OTHER" %in% x$pericoli) x$pericolo_altro <- NULL
  }
  if (e!="2") x$evidenze <- list()
  if (e %in% c("1","3","5")) x$indagine <- NULL
  x
}
