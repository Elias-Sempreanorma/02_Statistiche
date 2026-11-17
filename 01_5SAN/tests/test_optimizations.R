library(dplyr)
library(lubridate)
library(shiny)
assignments <- function(path, names, env) {
  for (x in parse(path)) if (is.call(x) && identical(x[[1]], as.name("<-")) &&
    is.symbol(x[[2]]) && as.character(x[[2]]) %in% names) eval(x, env)
}
source("01_Code/dashboard_life.R")
x <- tibble(coupon=c("A","A","B","A"), cds_name="S",
  timestamp=as.POSIXct("2026-01-01",tz="UTC") + c(2,1,3,4),
  cds_vds=c(NA,100,200,110), cds_t10d=c(5,NA,6,NA),
  count=c(20,10,NA,15), offset=c(1,2,NA,0), cds_elapsed_years=c(1,0,NA,2))
y <- prepara_vita_dashboard(x)
stopifnot(nrow(y)==2,y$count[1]==22,is.na(y$count[2]),y$cds_vds[1]==110,
 y$cds_t10d[1]==5,y$lifetime[1]==2,identical(y,prepara_vita_dashboard(x[c(4,3,1,2),])),
 nrow(prepara_vita_dashboard(x[0,]))==0)
x$cds_elapsed_years <- NULL
stopifnot(all(is.na(prepara_vita_dashboard(x)$lifetime)))
# SQL: confini dei blocchi, ordine e tipi dei parametri, ID e JSON.
e <- new.env()
assignments("01_Code/07_Alerts.R",c("insert_alert_sql","inserisci_segnalazioni"),e)
calls <- list()
e$dbExecute <- function(con,sql,params) {calls[[length(calls)+1L]] <<- list(sql=sql,params=params)}
columns <- c("type_code","generated_at","event_key","alert_type","alert_scope",
 "period_start","period_end","project","machine_serial_number","machine_name",
 "coupon","gateway_id","gateway_name","sensor_id","cds_name","sensor_description",
 "functional_unit","signal_description","signal_data_text","signal_data_json","alert_id","created_at")
a <- as_tibble(setNames(lapply(columns,function(n) paste0(n,seq_len(1001))),columns))
for(n in c("generated_at","period_start","period_end","created_at"))
 a[[n]] <- as.POSIXct("2026-01-01",tz="Europe/Rome")+seq_len(nrow(a))
a$signal_data_json <- '{"value":1}'; a$alert_id[2] <- NA_character_
e$inserisci_segnalazioni(NULL,a)
stopifnot(length(calls)==3,identical(lengths(lapply(calls,`[[`,"params")),c(11000L,11000L,22L)))
for(i in seq_along(calls)) {
 z <- calls[[i]]
 stopifnot(grepl("ON CONFLICT (event_key) DO NOTHING",z$sql,fixed=TRUE),grepl("::jsonb",z$sql,fixed=TRUE))
 refs <- as.integer(sub("$","",regmatches(z$sql,gregexpr("\\$[0-9]+",z$sql))[[1]],fixed=TRUE))
 stopifnot(identical(sort(unique(refs)),seq_along(z$params)))
 for(j in seq_len(length(z$params)/22L)) for(k in seq_along(columns))
  stopifnot(identical(z$params[[(j-1L)*22L+k]],a[[columns[k]]][(i-1L)*500L+j]))
}
calls <- list(); e$inserisci_segnalazioni(NULL,a[0,]); stopifnot(!length(calls))
e$inserisci_segnalazioni(NULL,select(a[1,],-alert_id,-created_at))
stopifnot(is.na(calls[[1]]$params[[21]]),inherits(calls[[1]]$params[[22]],"POSIXct"))
# Ricerca strettamente successiva con timestamp duplicati e gateway diversi.
code <- readLines("01_Code/03_Data_Frequency.R")
block <- code[grep("^count_series <-",code):(grep("^reset_results <-",code)-1L)]
set.seed(10); t0 <- as.POSIXct("2026-01-01",tz="UTC")
count_events <- tibble(gateway_id=rep(c("G1","G2"),each=400),
 sensor_id=rep(rep(c("S1","S2"),each=200),2),timestamp=t0+sample(0:400,800,replace=TRUE)) |>
 arrange(gateway_id,sensor_id,timestamp)
eval(parse(text=block))
for(g in names(count_series)) for(sid in names(count_series[[g]])) for(t in c(-1,0:400,500)) {
 z <- count_series[[g]][[sid]]; idx <- findInterval(as.numeric(t0)+t,z$times)+1L
 actual <- if(idx>length(z$rows)) integer() else z$rows[idx]
 expected <- which(count_events$gateway_id==g & count_events$sensor_id==sid & count_events$timestamp>t0+t)
 stopifnot(identical(actual,head(expected,1)))
}
# Cache: filtri visivi invarianti; nuovi snapshot/macchina/granularita' invalidano.
counter <- 0L
server <- function(input,output,session) {
 macchina_attiva <- reactiveVal("A"); granularita_utilizzo_corrente <- reactiveVal("Settimana")
 alerts_correnti <- reactiveVal(tibble(coupon=character(),alert_type=character(),
  period_start=as.POSIXct(character()),period_end=as.POSIXct(character())))
 dati <- NULL
 prepara_valori_giornalieri_nok_data <- function(...) {counter <<- counter+1L; tibble(day=as.Date("2026-01-05")+0:20)}
 valuta_utilizzo_periodo <- function(...) tibble(utilization=1,lower_limit=.5,upper_limit=2,reference_weeks=2L)
 assignments("01_Code/dashboard/20_utilizzo.R",c("giorno_utilizzo","storico_utilizzo_macchina"),environment())
}
testServer(server, {
 first <- storico_utilizzo_macchina(); stopifnot(counter==1L,nrow(first)==3L)
 session$setInputs(dates=c("2026-01-01","2026-01-31"),sensors="S2")
 stopifnot(identical(first,storico_utilizzo_macchina()),counter==1L)
 macchina_attiva("B"); storico_utilizzo_macchina(); stopifnot(counter==2L)
 granularita_utilizzo_corrente("Mese"); storico_utilizzo_macchina(); stopifnot(counter==3L)
 alerts_correnti(tibble(coupon="B",alert_type="utilization_monthly",
  period_start=as.POSIXct("2026-01-01",tz="Europe/Rome"),period_end=as.POSIXct("2026-01-31",tz="Europe/Rome"),
  utilization_value=3,utilization_lower=.5,utilization_upper=2,utilization_references=2L))
 stopifnot(storico_utilizzo_macchina()$U_macchina==3,counter==4L)
})
cat("OK: vita, reset, SQL a blocchi e cache.\n")
# Runner: oggetti isolati per run, fallback RDS e nessuna contaminazione globale.
runner <- new.env()
runner$scripts <- c("producer", "consumer")
runner$here <- function(...) file.path(...)
runner$readRDS <- function(path) "disk"
runs <- list()
runner$source <- function(path,local) {
 if (basename(path)=="producer") {
   stopifnot(!exists("raw_data",envir=local,inherits=FALSE))
   local$raw_data <- "current run"
 } else {
   stopifnot(identical(local$.etl_read_input("raw_data","unused"),"current run"),
     identical(local$.etl_read_input("missing","unused"),"disk"))
   runs[[length(runs)+1L]] <<- local
 }
}
runner_expr <- Filter(function(x) is.call(x) && identical(x[[1]],as.name("local")),
 parse("01_Code/00_ETL_Lightsail.R"))[[1]]
eval(runner_expr,runner); eval(runner_expr,runner)
stopifnot(length(runs)==2,!identical(runs[[1]],runs[[2]]))
cat("OK: isolamento del runner e riuso esplicito dei dati.\n")
