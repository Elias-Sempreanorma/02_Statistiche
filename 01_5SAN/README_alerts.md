# Segnalazioni persistenti

Il primo ETL riuscito dopo le 07:00 Europe/Rome consolida il giorno precedente.
L'ora si configura con `ALERT_EVALUATION_HOUR` (0–23); resta valido il cron
Lightsail esistente. I giorni saltati vengono recuperati in ordine cronologico.

Il raggruppamento usa coupon, tipo, periodo esatto e comportamento codificato
(aumento/diminuzione), indipendentemente da intensificatori come "molte". Il testo e
il JSON conservano sensori, gateway e misure. Comportamenti superiori e inferiori
all'atteso restano separati. Un gruppo resta visibile filtrando qualsiasi suo
sensore.

`alert_evaluation_days` registra anche i giorni senza segnalazioni. Inserimenti
e checkpoint condividono una transazione; un lock PostgreSQL impedisce job
concorrenti. Un conflitto su `event_key` non modifica la segnalazione esistente.

Al primo avvio una migrazione raggruppa gli alert sensore storici completi:
gli ID singoli vengono conservati, i gruppi ricevono nuovi ID con riferimenti
originali nel JSON. Le righe sensore del giorno ancora incompleto vengono
scartate e valutate a giornata conclusa. Le disconnessioni e il loro stato
email vengono conservati. La migrazione non si ripete nei lanci successivi.

`public.alerts` e' la sorgente della dashboard. `alerts.rds` ne e' una copia
integrale sostituita atomicamente. Shiny la controlla ogni 60 secondi, senza
ricalcolare segnalazioni. Le email rimangono limitate alle disconnessioni
recenti; il recupero di vecchie interruzioni non genera notifiche arretrate.

## Aggiornamento Lightsail

Dalla cartella dell'applicazione, sul branch `01_5SAN`:

```bash
git pull origin 01_5SAN
docker compose build shiny
docker compose run --rm etl
docker compose up -d shiny shiny_web web_auth
```

Il primo ETL applica la migrazione automaticamente. Non occorre ricostruire i
raw data. Il deploy e la migrazione sul DB operativo richiedono questi comandi
sul server; il commit da solo non li esegue.

## Test offline

```bash
Rscript tests/test_alerts_incremental.R
```

Verifica raggruppamenti, periodi/comportamenti distinti, tracciabilita' degli ID,
ora legale e isolamento da giorni successivi. Simula il DB per verificare retry,
checkpoint, orario mattutino, giorni senza alert, recupero interruzioni e rollback.
Non si collega al DB operativo e non invia email.

La migrazione v3 aggiorna anche i gruppi v2 salvati: unisce i comportamenti
equivalenti senza ricalcolo statistico. Gli identificativi tecnici restano nel
JSON e nelle colonne interne; la descrizione mostra codice e nome del sensore.

## Ricostruzione storico con le regole correnti

La ricostruzione ricalcola manovre giornaliere/orarie, componenti aperti e utilizzo
settimanale/mensile dal primo dato disponibile a ieri. Elimina gli allarmi NOK
obsoleti e ricrea i checkpoint. Non modifica i raw data e non cancella le
disconnessioni gateway o il loro stato email. Durante la ricostruzione non invia email.

Il lock esistente impedisce ETL concorrenti. Backup di alerts e checkpoint e tutte
le sostituzioni sono racchiusi in una transazione: un errore conserva lo stato
precedente. A fine operazione vengono stampati i nomi delle tabelle backup.
alerts.rds viene sostituito atomicamente solo dopo il completamento.

Eseguire UNA VOLTA dopo il deploy:

```bash
git pull origin 01_5SAN
docker compose build shiny
docker compose run --rm -e ALERT_REBUILD_HISTORY=1 etl
docker compose up -d shiny shiny_web web_auth
```

Il parametro vale solo per questa esecuzione; il cron ordinario resta incrementale.
La ricostruzione puo' richiedere tempo: attendere il messaggio finale prima di
riavviare Shiny. Ogni domenica si valuta la settimana conclusa; a fine mese anche
il mese di calendario. Il confronto richiede 7 periodi precedenti dello stesso tipo:
7 settimane o 7 mesi. Grafici e segnalazioni usano il medesimo valutatore.
