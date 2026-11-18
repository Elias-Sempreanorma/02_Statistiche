# Cataloghi del prototipo 5SAN v0.3 (26/10/2026).
# Codici interni 5SAN: NON sono codici ufficiali INAIL.
versione_catalogo_cr <- "0.2"

esiti <- c(
  "1" = "Anomalia giustificata",
  "2" = "Condizione già analizzata e in trattamento",
  "3" = "Condizione da approfondire",
  "4" = "Criticità di sicurezza riconosciuta nella verifica preliminare",
  "5" = "Segnalazione non significativa per causa tecnica 5SAN"
)

domande_a11 <- c(
  "1" = "Perché l'anomalia è giustificata?",
  "2" = "Che cosa è già stato analizzato e quale trattamento è in corso?",
  "3" = "Che cosa non è ancora ricostruibile e richiede l'indagine?",
  "4" = "Che cosa è stato effettivamente riscontrato?",
  "5" = "Quale causa tecnica spiega la segnalazione?"
)

criticita <- c(
  "CR01" = "Errore procedurale nell'attività sulla macchina",
  "CR02" = "Problema di comunicazione relativo all'attività sulla macchina",
  "CR03" = "Procedure operative assenti o inadeguate",
  "5SAN_INFO_MACHINE" = "Informazioni e avvertenze di sicurezza della macchina assenti o inadeguate (descrittore CR11)",
  "CR04" = "Protezioni della macchina assenti",
  "CR05" = "Protezioni della macchina inadeguate o inefficaci",
  "CR06" = "Anomalia o guasto in avviamento, arresto o esercizio",
  "CR07" = "Unica attrezzatura disponibile non idonea alla lavorazione",
  "CR08" = "Attrezzatura o ausilio idoneo non disponibile",
  "CR10" = "Caratteristiche o trasformazioni pericolose del materiale lavorato",
  "CR13" = "Illuminazione della zona operativa non idonea o assente",
  "CR15" = "Spazi di accesso, utilizzo o intervento sulla macchina inadeguati",
  "CR17" = "Presenza imprevista di liquidi presso la macchina",
  "CR18" = "Presenza imprevista di gas o vapori presso la macchina",
  "CR19" = "Sistema ausiliario necessario alla sicurezza della macchina assente, inefficace o malfunzionante",
  "CR20" = "Elementi elettrici pericolosi accessibili",
  "CR21" = "Rumorosità della macchina riconosciuta come inadeguata",
  "CR25" = "Altra condizione critica della macchina - specificare"
)

pericoli <- c(
  "CRUSH" = "Schiacciamento", "SHEAR" = "Cesoiamento",
  "CUT" = "Taglio / sezionamento", "ENTANGLEMENT" = "Impigliamento",
  "DRAW_IN" = "Trascinamento", "IMPACT" = "Urto",
  "PROJECTION" = "Proiezione di materiali", "FALL" = "Caduta di persone o materiali",
  "ELECTRIC_CONTACT" = "Contatto elettrico", "ARC" = "Arco elettrico",
  "HEAT" = "Contatto con elementi caldi", "COLD" = "Contatto con elementi freddi",
  "INHALATION" = "Inalazione di sostanze pericolose",
  "SUBSTANCE_CONTACT" = "Contatto con sostanze pericolose",
  "NOISE" = "Rumore", "VIBRATION" = "Vibrazioni", "RADIATION" = "Radiazioni",
  "ERGONOMIC" = "Sforzi, posture o movimenti critici",
  "FIRE" = "Incendio / esplosione", "OTHER" = "Altro - specificare"
)

fonti <- c(
  "OBSERVATION" = "Osservazione diretta / sopralluogo",
  "TEST" = "Prova o controllo tecnico",
  "INTERVIEW" = "Colloquio / dichiarazione",
  "DOCUMENT" = "Documentazione aziendale",
  "DATA_5SAN" = "Dati 5SAN",
  "OTHER" = "Altro riscontro"
)

evidenze <- c(
  "PREVIOUS_ANALYSIS" = "Analisi precedente disponibile",
  "ACTIONS_DEFINED" = "Azioni / misure definite o intraprese",
  "RECORDS" = "Evidenze aziendali disponibili"
)

tipi_misura <- c(
  "STOP" = "Arresto macchina",
  "ACCESS" = "Interdizione dell'accesso",
  "LIMIT" = "Limitazione dell'utilizzo",
  "TECHNICAL" = "Intervento tecnico",
  "OTHER" = "Altra misura"
)

stati_misura <- c(
  "DONE" = "Attuata", "IN_PROGRESS" = "In corso di attuazione",
  "ORDERED" = "Disposta, non ancora attuata", "PLANNED" = "Programmata"
)

comunicazioni <- c(
  "DONE" = "Comunicazione effettuata",
  "PENDING" = "Comunicazione da effettuare",
  "NONE" = "Nessuna comunicazione dichiarata",
  "UNKNOWN" = "Informazione non disponibile"
)

stati_gestione <- c(
  "IN_PROGRESS" = "Gestione aziendale in corso",
  "CLOSED_DECLARED" = "Gestione dichiarata conclusa",
  "UNKNOWN" = "Stato non noto"
)

si_no <- c("YES" = "Sì", "NO" = "No")
si_no_ignoto <- c(si_no, "UNKNOWN" = "Non determinabile")
