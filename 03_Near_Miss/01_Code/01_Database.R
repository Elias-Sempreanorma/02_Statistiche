# Accesso ai dati e persistenza. Nessun ETL: public.alerts è già alimentata da 5SAN.
# Le credenziali sono esclusivamente variabili d'ambiente lato server.
connessione_db <- function() {
  richieste <- c("PG_HOST", "PG_PORT", "PG_DB_STATS", "PG_USER", "PG_PASSWORD")
  assenti <- richieste[!nzchar(Sys.getenv(richieste))]
  if (length(assenti)) stop("Configurazione PostgreSQL incompleta: ", paste(assenti, collapse = ", "))
  DBI::dbConnect(RPostgres::Postgres(),
    host = Sys.getenv("PG_HOST"), port = as.integer(Sys.getenv("PG_PORT")),
    dbname = Sys.getenv("PG_DB_STATS"), user = Sys.getenv("PG_USER"),
    password = Sys.getenv("PG_PASSWORD"), timezone = "Europe/Rome")
}

# Una sola tabella; una riga per revisione, dati strutturati + insiemi in JSONB.
# La migrazione è esplicita, mai eseguita automaticamente all'apertura dell'app.
sql_creazione <- "
CREATE TABLE IF NOT EXISTS public.verifiche_near_miss (
  id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  id_segnalazione VARCHAR(40) NOT NULL,
  revisione INTEGER NOT NULL CHECK (revisione > 0),
  data_registrazione TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  operatore_account TEXT NOT NULL,
  responsabile_nome TEXT NOT NULL,
  responsabile_ruolo TEXT NOT NULL,
  responsabile_funzione TEXT NOT NULL,
  coupon TEXT NOT NULL,
  codice_macchina TEXT,
  nome_macchina TEXT,
  periodo_inizio TIMESTAMPTZ NOT NULL,
  periodo_fine TIMESTAMPTZ NOT NULL,
  data_segnalazione TIMESTAMPTZ NOT NULL,
  origine TEXT NOT NULL,
  data_primo_invio TIMESTAMPTZ,
  esito INTEGER NOT NULL CHECK (esito BETWEEN 1 AND 5),
  motivazione TEXT NOT NULL,
  qualificazione TEXT,
  episodio TEXT,
  danno TEXT,
  potenziale TEXT,
  criticita JSONB NOT NULL DEFAULT '[]',
  criticita_altro TEXT,
  pericoli JSONB NOT NULL DEFAULT '[]',
  pericolo_altro TEXT,
  fonti JSONB NOT NULL DEFAULT '[]',
  fonti_nota TEXT,
  evidenze JSONB NOT NULL DEFAULT '[]',
  riferimento TEXT,
  azione_immediata TEXT,
  chiarimenti TEXT,
  misure TEXT,
  azioni JSONB NOT NULL DEFAULT '[]',
  nota_misure TEXT,
  comunicazioni TEXT,
  nota_comunicazioni TEXT,
  stato_gestione TEXT,
  indagine_richiesta BOOLEAN NOT NULL DEFAULT FALSE,
  snapshot_segnalazione JSONB NOT NULL,
  CONSTRAINT verifica_revisioni_uniche UNIQUE (id_segnalazione, revisione),
  CONSTRAINT verifica_motivazione_non_vuota CHECK (length(btrim(motivazione)) > 0),
  CONSTRAINT verifica_responsabile_non_vuoto CHECK (
    length(btrim(responsabile_nome)) > 0 AND length(btrim(responsabile_ruolo)) > 0)
);
CREATE INDEX IF NOT EXISTS verifiche_near_miss_coupon_idx
  ON public.verifiche_near_miss (coupon, id_segnalazione, revisione DESC);
"

inizializza_database <- function() {
  con <- connessione_db()
  on.exit(DBI::dbDisconnect(con))
  for (istruzione in strsplit(sql_creazione, ";", fixed = TRUE)[[1]]) {
    if (nzchar(trimws(istruzione))) DBI::dbExecute(con, istruzione)
  }
  invisible(TRUE)
}

# Anagrafica reale che usa anche la dashboard: una sola riga per coupon.
# Non si creano copie in 02_Output e non si aggiungono tabelle.
anagrafica_macchine <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT a.coupon,
      COALESCE(NULLIF(BTRIM(p.azienda), ''), '(Non specificata)') AS azienda,
      COALESCE(NULLIF(BTRIM(p.stabilimento), ''), '(Non specificato)') AS stabilimento,
      COALESCE(NULLIF(BTRIM(p.progetto), ''), '(Non specificato)') AS progetto,
      COALESCE(NULLIF(BTRIM(a.machine_name), ''), a.coupon) AS nome_macchina
    FROM (
      SELECT coupon, MAX(machine_name) AS machine_name
      FROM public.alerts
      WHERE coupon IS NOT NULL
      GROUP BY coupon
    ) a
    LEFT JOIN (
      SELECT coupon,
             MAX(azienda) AS azienda,
             MAX(stabilimento) AS stabilimento,
             MAX(progetto) AS progetto
      FROM public.progetti_componenti_b10d_san
      GROUP BY coupon
    ) p ON p.coupon = a.coupon
    ORDER BY azienda, stabilimento, progetto, nome_macchina
  ")
}

intervallo_segnalazioni <- function(con, coupon = NULL) {
  if (is.null(coupon)) {
    r <- DBI::dbGetQuery(con, "
      SELECT MIN(period_start) AS inizio, MAX(period_end) AS fine
      FROM public.alerts")
  } else {
    r <- DBI::dbGetQuery(con, "
      SELECT MIN(period_start) AS inizio, MAX(period_end) AS fine
      FROM public.alerts WHERE coupon = $1", params = list(as.character(coupon)))
  }
  if (is.na(r$inizio[1]) || is.na(r$fine[1])) {
    oggi <- as.Date(Sys.time(), tz = "Europe/Rome")
    return(c(oggi, oggi))
  }
  c(as.Date(r$inizio[1], tz="Europe/Rome"),
    as.Date(r$fine[1], tz="Europe/Rome"))
}

# Parametri scalari e filtri sul periodo effettivo della segnalazione;
# estremi inclusivi nel fuso Europe/Rome, come nella dashboard.
elenco_segnalazioni <- function(con, coupon, date) {
  if (length(coupon) != 1L || !nzchar(coupon) ||
      length(date) != 2L || anyNA(date)) {
    return(data.frame())
  }
  DBI::dbGetQuery(con, "
    SELECT a.alert_id, a.coupon, a.machine_code, a.machine_name,
           a.period_start, a.period_end, a.generated_at,
           a.alert_type, a.signal_description, a.sensor_description,
           a.sensor_code, COALESCE(v.revisione, 0) AS ultima_revisione
    FROM public.alerts a
    LEFT JOIN LATERAL (
      SELECT revisione FROM public.verifiche_near_miss
      WHERE id_segnalazione = a.alert_id
      ORDER BY revisione DESC LIMIT 1
    ) v ON TRUE
    WHERE a.coupon = $1
      AND (a.period_end AT TIME ZONE 'Europe/Rome')::date >= $2::date
      AND (a.period_start AT TIME ZONE 'Europe/Rome')::date <= $3::date
    ORDER BY a.period_start DESC, a.alert_id DESC
  ", params=list(as.character(coupon),
                 format(as.Date(date[1]), "%Y-%m-%d"),
                 format(as.Date(date[2]), "%Y-%m-%d")))
}

# La verifica del coupon è sempre sul server. Mai fidarsi del solo URL.
leggi_segnalazione <- function(con, id_segnalazione, coupon_autorizzati) {
  if (!length(coupon_autorizzati)) stop("Nessuna macchina autorizzata.")
  d <- DBI::dbGetQuery(con, "
    SELECT * FROM public.alerts
    WHERE alert_id = $1 AND coupon = ANY(
      SELECT jsonb_array_elements_text($2::jsonb)
    )",
    params=list(as.character(id_segnalazione),
      as.character(jsonlite::toJSON(as.character(coupon_autorizzati),
                                   auto_unbox=FALSE))))
  if (nrow(d) != 1L) stop("Segnalazione non disponibile per questo accesso.")
  d[1, , drop=FALSE]
}

ultima_verifica <- function(con, id_segnalazione) {
  d <- DBI::dbGetQuery(con, "
    SELECT * FROM public.verifiche_near_miss
    WHERE id_segnalazione = $1 ORDER BY revisione DESC LIMIT 1",
    params = list(id_segnalazione))
  if (!nrow(d)) NULL else d[1, , drop = FALSE]
}

# Concorrenza: blocco della riga sorgente e revisioni monotone all'interno
# della stessa transazione. Revisione attesa evita sovrascritture silenziose.
salva_verifica <- function(con, segnalazione, operatore, coupon_autorizzati,
                           revisione_attesa, dati) {
  id <- as.character(segnalazione$alert_id[[1]])
  if (!as.character(segnalazione$coupon[[1]]) %in% coupon_autorizzati)
    stop("Accesso alla macchina non autorizzato.")
  valida_modulo(dati)
  DBI::dbWithTransaction(con, {
    seg <- leggi_segnalazione(con, id, coupon_autorizzati)
    DBI::dbGetQuery(con, "SELECT alert_id FROM public.alerts WHERE alert_id = $1 FOR UPDATE",
                    params = list(id))
    numero <- DBI::dbGetQuery(con, "
      SELECT COALESCE(MAX(revisione),0) AS numero
      FROM public.verifiche_near_miss WHERE id_segnalazione=$1",
      params = list(id))$numero[[1]]
    if (numero != revisione_attesa)
      stop("La verifica è stata aggiornata da un altro operatore. Ricarica prima di salvare.")
    valori <- list(
      id_segnalazione=id, revisione=as.integer(numero+1L),
      operatore_account=operatore, responsabile_nome=dati$responsabile_nome,
      responsabile_ruolo=dati$responsabile_ruolo,
      responsabile_funzione=dati$responsabile_funzione,
      coupon=as.character(seg$coupon[[1]]),
      codice_macchina=seg$machine_code[[1]], nome_macchina=seg$machine_name[[1]],
      periodo_inizio=seg$period_start[[1]], periodo_fine=seg$period_end[[1]],
      data_segnalazione=seg$generated_at[[1]], origine=seg$source[[1]],
      esito=as.integer(dati$esito), motivazione=dati$motivazione,
      qualificazione=dati$qualificazione, episodio=dati$episodio,
      danno=dati$danno, potenziale=dati$potenziale,
      criticita=codifica_json(dati$criticita), criticita_altro=dati$criticita_altro,
      pericoli=codifica_json(dati$pericoli), pericolo_altro=dati$pericolo_altro,
      fonti=codifica_json(dati$fonti), fonti_nota=dati$fonti_nota,
      evidenze=codifica_json(dati$evidenze), riferimento=dati$riferimento,
      azione_immediata=dati$azione_immediata, chiarimenti=dati$chiarimenti,
      misure=dati$misure, azioni=codifica_json(dati$azioni),
      nota_misure=dati$nota_misure, comunicazioni=dati$comunicazioni,
      nota_comunicazioni=dati$nota_comunicazioni,
      stato_gestione=dati$stato_gestione,
      indagine_richiesta=isTRUE(dati$indagine_richiesta),
      snapshot_segnalazione=codifica_json(as.list(seg[1, , drop=FALSE]))
    )
    # Parametri SQL scalari: NULL R non equivale a SQL NULL nel binding.
    valori <- lapply(valori, function(v) {
      if (is.null(v) || length(v) == 0L) return(NA_character_)
      if (length(v) == 1L && is.character(v) && !nzchar(v)) return(NA_character_)
      v
    })
    campi <- names(valori)
    # DBI parametrizzato, nessun dato dell'utente interpolato nella query.
    json <- c("criticita","pericoli","fonti","evidenze","azioni","snapshot_segnalazione")
    placeholder <- paste0("$",seq_along(campi),ifelse(campi %in% json,"::jsonb",""))
    query <- paste0("INSERT INTO public.verifiche_near_miss (",
      paste(DBI::dbQuoteIdentifier(con, campi), collapse=","),
      ") VALUES (",paste(placeholder,collapse=","),") RETURNING id, revisione")
    DBI::dbGetQuery(con, query, params = unname(valori))
  })
}
