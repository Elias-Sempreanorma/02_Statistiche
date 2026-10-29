# Segnalazioni persistenti

Il primo ETL riuscito dopo le 07:00 Europe/Rome consolida il giorno precedente.
L'ora si configura con `ALERT_EVALUATION_HOUR` (0–23); resta valido il cron
Lightsail esistente. I giorni saltati vengono recuperati in ordine cronologico.

Il raggruppamento usa coupon, tipo, periodo esatto e comportamento. Il testo e
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
