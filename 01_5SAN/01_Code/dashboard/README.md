# Dashboard 5SAN

`../05_Plots.R` resta il punto di ingresso per `app.R` e per gli avvii esistenti.
I file sono parti della stessa applicazione, non app autonome.

| File | Responsabilità |
| --- | --- |
| `01_accesso.R` | Pacchetti, autenticazione e percorsi delle risorse |
| `02_dati.R` | Lettura dati, vita componenti e lettore degli alert |
| `03_grafica.R` | Palette, tema, lookup e redistribuzione grafica |
| `04_mappe.R` | Immagini macchina e coordinate dei componenti |
| `05_ui.R` | Struttura della pagina e accesso web |
| `06_assets.R`, `07_script.R`, `08_stile.R` | Composizione del head, JavaScript e CSS |
| `10_sessione.R` | Accesso, download, filtri e stato della sessione |
| `11_finestre.R` | Apertura e navigazione delle finestre, filtri dedicati |
| `12_report_calcoli.R`, `13_report_vista.R` | Calcoli dei report e loro presentazione |
| `14_home.R` | NOK della Home e schema interattivo |
| `15_nok.R` | Dati e indicatori NOK delle finestre |
| `16_segnalazioni.R` | Lettura reattiva e visualizzazione degli alert |
| `17_vita.R` | Vita dei componenti e serbatoi |
| `18_manovre.R` | Aggregazioni, barre e andamento delle manovre |
| `19_nok_storico.R` | Preparazione delle serie storiche NOK |
| `20_utilizzo.R` | Calcoli e grafici di Utilizzo e andamento NOK |
| `21_disponibilita.R` | Disponibilità macchina e relativo grafico |
| `22_tabelle.R` | Tabelle dei dati dei grafici |

## Ambienti e ordine

Le parti iniziali vengono caricate nell'ambiente dell'applicazione. Le parti
server vengono analizzate una volta all'avvio e valutate, nell'ordine dichiarato
in `05_Plots.R`, nell'ambiente della singola chiamata a `server()`. Le funzioni
e le reactive condividono quindi lo stato della propria sessione, senza
condividerlo con gli altri utenti. Non spostare questa valutazione in `.GlobalEnv`
né in una funzione che crei un ambiente distinto per ogni parte.

La divisione conserva le espressioni R e le stringhe HTML/CSS/JavaScript
precedenti. Non introduce nuovi pacchetti o modifiche alle formule. I nomi
pubblici `ui` e `server` e gli ID Shiny rimangono invariati.

## Verifiche

Dalla cartella `01_5SAN`:

```sh
Rscript tests/test_dashboard_parts.R
Rscript tests/test_redistribuzione_grafica.R
Rscript tests/test_anomalie_orarie_utilizzo.R
Rscript tests/test_gateway_disconnections.R
```

I test che estraggono funzioni senza avviare l'app devono leggere la relativa
parte, non cercare le definizioni nel file di ingresso.

### Verifica della prima estrazione

Confronto con il commit `80c6ced`: ricomponendo le parti, tutte le espressioni
R della dashboard (inclusi corpo server e stringhe degli asset) sono identiche
all'originale. In `04_Counter_Analysis.R` l'unica espressione rimossa è
l'assegnazione inutilizzata di `open_intervals`.

Verifica offline con R 4.5.0: superati i test delle parti/sessioni, della
redistribuzione grafica, delle anomalie orarie/utilizzo, delle disconnessioni
e dei modelli probabilistici. Il container operativo usa R 4.5.3; non è stata
eseguita una verifica visuale con i dati operativi né un deploy.

Due test falliscono anche sul commit originale, nello stesso ambiente:
`test_alerts_incremental.R` nell'asserzione `nrow(hourly) == 2L` e
`Test/test_machine_availability.R` per l'anagrafica
`progetti_componenti_b10d_san` non definita. Questi problemi preesistenti
restano separati dalla riorganizzazione.
