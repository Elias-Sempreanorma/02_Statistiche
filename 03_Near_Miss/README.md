# 03_Near_Miss — Verifica preliminare 5SAN

Progetto R indipendente dal prototipo HTML v0.3 (26/10/2026).
Lettura diretta da public.alerts; nessun ETL ulteriore.
00_Data: cataloghi; 01_Code: codice R; 02_Output: nessun file necessario; app.R: avvio.

## Prima installazione
Configurare fuori da Git PG_HOST, PG_PORT, PG_DB_STATS, PG_USER e PG_PASSWORD.\nAPP_MODE=internal non presenta un secondo login: consentire accesso soltanto da rete/IP autorizzati.\nAPP_MODE=web resta bloccata fino all’integrazione con autenticazione e permessi del portale.\nIl nome dell’operatore è dichiarato nel modulo e NON è un’identità autenticata.
Creare la tabella una sola volta, in ambiente di staging, con:
Rscript -e 'library(DBI);library(RPostgres);library(jsonlite);source("00_Data/cataloghi.R");source("01_Code/02_Regole.R");source("01_Code/01_Database.R");inizializza_database()'
La tabella è public.verifiche_near_miss, con una riga per revisione e unico
ID segnalazione/revisione. Non vengono create tabelle aggiuntive.
Avvio container: docker compose up -d --build.
Applicare HTTPS, IP allowlist e permessi di rete al reverse proxy:
la porta locale è 127.0.0.1:3842. Non esporre il container direttamente.

## Regole
A01–A04/A07 letti da DB e non editabili. A05/A06 assenti. A09 NULL,
non valorizzato fino alla futura integrazione. A08/A10/A11 obbligatori.
Cinque percorsi e qualificazione guidata; UNKNOWN diverso da NO.
La richiesta di indagine resta una dichiarazione memorizzata, senza
apertura di un'indagine reale; nessuna notifica, ticket o validazione normativa.
Ogni nuovo invio aggiunge una revisione e conserva le precedenti.

## Stato
Codice di prima implementazione da testare con PostgreSQL e Shiny reali.
Nessun rilascio sul server AWS eseguito.
