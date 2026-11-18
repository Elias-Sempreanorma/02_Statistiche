# Test delle regole di qualificazione del prototipo v0.3.
source("00_Data/cataloghi.R")
source("01_Code/02_Regole.R")
stopifnot(
  identical(qualifica("NO","",""),"NC"),
  identical(qualifica("UNKNOWN","",""),"UNDETERMINED"),
  identical(qualifica("YES","YES",""),"INJURY_EVENT"),
  identical(qualifica("YES","UNKNOWN",""),"UNDETERMINED"),
  identical(qualifica("YES","NO","YES"),"NEAR_MISS"),
  identical(qualifica("YES","NO","UNKNOWN"),"UNDETERMINED"),
  identical(qualifica("YES","NO","NO"),"REVIEW_NEEDED")
)
base <- list(
  responsabile_nome="Mario Rossi",responsabile_ruolo="Manutentore",
  responsabile_funzione="MAINTENANCE",esito="1",
  motivazione="Anomalia giustificata e verificata.",indagine="",
  indagine_richiesta=FALSE,criticita=list(),pericoli=list(),fonti=list(),
  azioni=list(),evidenze=list(),episodio="",danno="",potenziale="")
stopifnot(isTRUE(valida_modulo(ripulisci_percorso(base))))
base$motivazione <- ""
stopifnot(inherits(try(valida_modulo(ripulisci_percorso(base)),silent=TRUE),"try-error"))
base$motivazione <- "Fatti verificati"
base$esito <- "3"
stopifnot(isTRUE(valida_modulo(ripulisci_percorso(base))))
base$esito <- "4"
base$indagine <- "NO"
base$criticita <- c("CR04")
base$pericoli <- c("DRAW_IN")
base$fonti <- c("OBSERVATION")
base$episodio <- "YES"
base$danno <- "NO"
base$potenziale <- "YES"
base$misure <- "NONE"
base$nota_misure <- "Nessuna misura dichiarata"
base$comunicazioni <- "UNKNOWN"
base$nota_comunicazioni <- "Non disponibili"
base$stato_gestione <- "UNKNOWN"
stopifnot(isTRUE(valida_modulo(ripulisci_percorso(base))))
cat("Test delle regole Near Miss superati.\n")
