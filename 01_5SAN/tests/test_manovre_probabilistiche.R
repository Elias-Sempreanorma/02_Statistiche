# Eseguire dalla cartella 01_5SAN: Rscript tests/test_manovre_probabilistiche.R
library(dplyr)
library(lubridate)
source("01_Code/01_Functions.R")
for (f in list.files("01_Code", pattern = "\\.R$", full.names = TRUE)) parse(f)

# Piccola differenza: puo' essere isolata per LOF, ma resta entro i limiti.
d <- tibble(day = as.Date("2026-01-01") + 0:32,
  daily_count = c(rep(c(3, 4), 16), 2), daily_uptime = c(rep(14, 32), 14.22),
  daily_value = daily_count / daily_uptime, rate = .24, size = Inf)
c <- classifica_anomalie_giornaliere(d)
stopifnot(!c$outlier_lof[33], c$limite_manovre_inf[33] == 0,
          c$limite_manovre_sup[33] == 7)
d$daily_count[33] <- 400; d$daily_value <- d$daily_count / d$daily_uptime
c <- classifica_anomalie_giornaliere(d)
stopifnot(c$outlier_lof[33])
# Solo probabilita' non basta: un gruppo frequente mantiene LOF basso.
d <- tibble(daily_count = rep(c(1, 2, 40), each = 15), daily_uptime = 10,
  daily_value = daily_count / daily_uptime, rate = .2, size = Inf)
c <- classifica_anomalie_giornaliere(d)
stopifnot(!any(c$outlier_lof), any(d$daily_count > c$limite_manovre_sup))
# Zero non e' piu' una regola automatica; serve sempre la doppia conferma.
d$rate <- 0.01; d$daily_count[1] <- 0; d$daily_value <- d$daily_count / 10
stopifnot(!classifica_anomalie_giornaliere(d)$outlier_lof[1])
d$daily_count[1] <- -1
stopifnot(!classifica_anomalie_giornaliere(d)$outlier_lof[1])
d$rate <- NA_real_
stopifnot(!any(classifica_anomalie_giornaliere(d)$outlier_lof))

# Selezione robusta con esposizioni diverse e contaminazione nota.
set.seed(42)
h <- runif(100, 5, 20)
make <- function(y) tibble(coupon = "C", gateway_id = "G", sensor_id = "S",
  day = as.Date("2026-01-01") + 0:99, daily_count = y, daily_uptime = h)
y <- rpois(100, .24*h)
m_p <- stima_modello_manovre(make(y))
y[10] <- 400
m_cont <- stima_modello_manovre(make(y))
stopifnot(m_p$family == "poisson", abs(m_cont$rate - .24) < .1)
set.seed(2026)
m_nb <- stima_modello_manovre(make(rnbinom(100, mu = 2*h, size = 2)))
stopifnot(!is.null(m_nb), m_nb$family == "negative_binomial", is.finite(m_nb$size))
stopifnot(is.null(stima_modello_manovre(make(rep(0,100)))),
          is.null(stima_modello_manovre(make(y)[1:20, ])))

# Cache: nessun refit a ogni ETL, refit dopo 7 giorni o correzione storico.
real_fit <- stima_modello_manovre
n_fit <- 0L
stima_modello_manovre <- function(df) {
  n_fit <<- n_fit + 1L
  tibble(family = "poisson", rate = .24, size = Inf, coverage = .95,
    fitted_at = Sys.time(), train_end = max(df$day), n_days = nrow(df))
}
path <- tempfile(fileext = ".rds")
x <- make(y)
invisible(aggiorna_modelli_manovre(x, path)); stopifnot(n_fit == 1L)
invisible(aggiorna_modelli_manovre(x, path)); stopifnot(n_fit == 1L)
new <- x[100, ]; new$day <- new$day + 1
invisible(aggiorna_modelli_manovre(bind_rows(x,new), path)); stopifnot(n_fit == 1L)
z <- readRDS(path); z$models$fitted_at <- z$models$fitted_at - days(8); saveRDS(z,path)
invisible(aggiorna_modelli_manovre(x, path)); stopifnot(n_fit == 2L)
x$daily_count[1] <- x$daily_count[1] + 1
invisible(aggiorna_modelli_manovre(x, path)); stopifnot(n_fit == 3L)
invisible(aggiorna_modelli_manovre(bind_rows(x, mutate(x, sensor_id = "NEW")), path))
stopifnot(n_fit == 4L)
stima_modello_manovre <- real_fit

# Classificazione ETL inclusa nel file incrementi e conservata da dplyr.
daily <- tibble(coupon="C", gateway_id="G", sensor_id="S", cds_name="FCM4",
  day=as.Date("2026-01-01") + 0:32,
  daily_count=c(rep(c(3,4),16),400), daily_uptime=14.22,
  daily_value=daily_count/daily_uptime, rate=.24, size=Inf)
daily <- classifica_anomalie_giornaliere(daily)
increments <- tibble(coupon="C", cds_name="FCM4", increment=1)
attr(increments,"giornalieri_nok") <- daily
saveRDS(increments,path)
loaded <- readRDS(path) |> mutate(field="F") |> filter(coupon=="C")
stima_modello_manovre <- function(...) stop("La dashboard non deve stimare modelli")
classifica_anomalie_giornaliere <- function(...) stop("La dashboard non deve calcolare LOF")
same <- prepara_valori_giornalieri_nok_data(loaded,"C","FCM4")
stopifnot(identical(same, daily), nrow(filter(same,!outlier_lof))==32L,
  nrow(same)==33L, nrow(prepara_valori_giornalieri_nok_data(loaded,"OTHER"))==0L)
cat("OK: sintassi, limiti FCM4, LOF AND probabilita', zero/invalidi, stima robusta Poisson/NB, cache e lettura dashboard senza calcoli.\n")

library(dplyr)
library(lubridate)
source("01_Code/01_Functions.R")
set.seed(24)
h <- runif(60,8,16)
sensor_count_increment <- tibble(
  company="A",field="F",project="P",coupon="C",machine_name="M",
  gateway_id="G",gateway_name="G",sensor_id="S",cds_name="FCM4",
  sensor_description="Component", day=as.Date("2026-01-01")+0:59,
  count=cumsum(rpois(60,.24*h)),increment=c(2,rpois(59,.24*h)),daily_uptime=h,
  gateway_disconnect_affected=FALSE)
test_root <- tempfile(); dir.create(file.path(test_root,"02_Output"),recursive=TRUE)
here <- function(...) file.path(test_root,...)
code <- readLines("01_Code/04_Counter_Analysis.R")
a <- grep("^# Unica classificazione giornaliera",code)
b <- grep('^saveRDS\\(sensor_count_increment',code)
eval(parse(text=code[a:b]))
saved <- readRDS(here("02_Output","sensor_count_increment.rds"))
daily <- prepara_valori_giornalieri_nok_data(saved)
stopifnot(nrow(daily)==60, all(c("lof_score","outlier_lof","limite_manovre_inf","family") %in% names(daily)))
stamp <- readRDS(here("02_Output","modelli_manovre.rds"))$models$fitted_at
eval(parse(text=code[a:b]))
stopifnot(identical(stamp,readRDS(here("02_Output","modelli_manovre.rds"))$models$fitted_at))
cat("OK: aggregazione ETL, join modelli, salvataggio attributo giornaliero, lettura e riuso cache.\n")
# Sensore senza un modello valido: nessuna esclusione automatica.
short <- sensor_count_increment[1:10,]
models <- aggiorna_modelli_manovre(aggrega_valori_giornalieri_nok_data(short),tempfile())
stopifnot(nrow(models)==1, is.na(models$rate))
d <- aggrega_valori_giornalieri_nok_data(short) |>
  left_join(models,by=c("coupon","gateway_id","sensor_id")) |>
  group_by(coupon,gateway_id,sensor_id,cds_name,sensor_description) |>
  group_modify(~classifica_anomalie_giornaliere(.x)) |>
  ungroup()
stopifnot(!any(d$outlier_lof))
# Dataset vuoto: lo schema deve essere conservato.
models <- aggiorna_modelli_manovre(aggrega_valori_giornalieri_nok_data(short[FALSE,]),tempfile())
stopifnot(nrow(models)==0, all(c("family","rate","size") %in% names(models)))
sensor_count_increment <- short[FALSE,]
eval(parse(text=code[a:b]))
stopifnot(nrow(prepara_valori_giornalieri_nok_data(sensor_count_increment))==0)

# Il testo degli allarmi usa gli stessi limiti e la direzione del modello.
daily <- tibble(coupon="C",gateway_id="G",sensor_id="S",cds_name="FCM4",
  sensor_description="Component",day=as.Date("2026-01-01")+0:32,
  daily_count=c(rep(c(3,4),16),400),daily_uptime=14.22,
  daily_value=daily_count/daily_uptime,rate=.24,size=Inf,family="poisson",
  gateway_disconnect_affected=FALSE)
daily <- classifica_anomalie_giornaliere(daily)
sensor_count_increment <- tibble(coupon="C",gateway_id="G",sensor_id="S",cds_name="FCM4",sensor_description="Component")
attr(sensor_count_increment,"giornalieri_nok") <- daily
sensori_allarmabili <- sensor_count_increment
inizio_giorno <- function(x) as.POSIXct(as.Date(x),tz="Europe/Rome")
fine_giorno <- function(x) inizio_giorno(x)+days(1)-seconds(1)
json_alert <- function(x) "{}"
alerts_code <- readLines("01_Code/07_Alerts.R")
a <- grep("^base_giornaliera <-",alerts_code)
b <- grep("^# 2\\) Anomalie orarie",alerts_code)-2L
eval(parse(text=alerts_code[a:b]))
stopifnot(nrow(alert_attivazioni_giornaliere)==1,
  grepl("limiti giornalieri 95%: 0 - 7",alert_attivazioni_giornaliere$signal_data_text),
  grepl("superiori al limite probabilistico",alert_attivazioni_giornaliere$signal_description))
cat("OK: allarme giornaliero dal flag condiviso, testo con atteso/limiti/modello.\n")
