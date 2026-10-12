# Diagnostica manuale delle anomalie: NON richiamata da ETL o dashboard.
# Legge solo un RDS gia' elaborato; nessuna connessione DB, mail o modifica
# ai dati operativi. Output in una cartella separata per ogni esecuzione.
#
# Rscript 01_Code/08_Check_Anomalies.R --self-test
# Rscript 01_Code/08_Check_Anomalies.R
# Opzioni: --input PATH --output DIR --alpha 0.01 --seed 20261012
#
# I limiti sono predittivi plug-in: non incorporano l'incertezza dei
# parametri stimati. Senza etichette manuali, il tasso di segnalazione
# NON e' un tasso di falsi allarmi. Nessuna promozione automatica in produzione.
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

args <- if (interactive()) character() else commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop("Manca il valore per ", flag)
  args[[i + 1L]]
}
# Source() espone il percorso in ofile; Run/console usa la cartella del progetto.
source_files <- vapply(sys.frames(), function(frame) {
  if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[[1]]
}, character(1))
source_files <- source_files[!is.na(source_files)]
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(source_files) > 0L) {
  script_path <- normalizePath(tail(source_files, 1L))
} else if (!interactive() && length(script_arg) == 1L) {
  script_path <- normalizePath(sub("^--file=", "", script_arg))
} else {
  candidates <- c(
    file.path(getwd(), "01_Code", "08_Check_Anomalies.R"),
    file.path(getwd(), "01_5SAN", "01_Code", "08_Check_Anomalies.R"),
    file.path(getwd(), "08_Check_Anomalies.R")
  )
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) {
    stop("Aprire il progetto 01_5SAN o eseguire source() sul percorso del file.")
  }
  script_path <- normalizePath(candidates[[1]])
}
project_dir <- dirname(dirname(script_path))
source(file.path(project_dir, "01_Code", "01_Functions.R"))

alpha <- as.numeric(arg_value("--alpha", "0.01"))
seed <- as.integer(arg_value("--seed", "20261012"))
stopifnot(is.finite(alpha), alpha > 0, alpha < 1, !is.na(seed))
set.seed(seed)

probabilita_code <- function(y, mu, theta = NULL) {
  if (is.null(theta)) {
    tibble(
      p_basso = ppois(y, lambda = mu),
      p_alto = ppois(y - 1, lambda = mu, lower.tail = FALSE),
      limite_inf = qpois(alpha / 2, lambda = mu),
      limite_sup = qpois(1 - alpha / 2, lambda = mu)
    )
  } else {
    tibble(
      p_basso = pnbinom(y, size = theta, mu = mu),
      p_alto = pnbinom(y - 1, size = theta, mu = mu, lower.tail = FALSE),
      limite_inf = qnbinom(alpha / 2, size = theta, mu = mu),
      limite_sup = qnbinom(1 - alpha / 2, size = theta, mu = mu)
    )
  }
}

self_test <- function() {
  dates <- as.POSIXct("2026-01-01 12:00:00", tz = "Europe/Rome") +
    days(0:29)
  specimen <- tibble(
    coupon = "TEST", gateway_id = "GW", sensor_id = "S1",
    cds_name = "C1", sensor_description = "Test",
    timestamp = dates, count = seq_along(dates)
  )
  stopifnot(
    nrow(calcola_sensori_allarmabili(
      specimen |> mutate(timestamp = dates[[1]])
    )) == 0L,
    nrow(calcola_sensori_allarmabili(specimen[1:29, ])) == 0L,
    calcola_sensori_allarmabili(specimen)$n_giorni_conteggio == 30L
  )
  make_daily <- function(counts, hours = rep(1, length(counts))) {
    tibble(
      daily_count = counts, daily_uptime = hours,
      daily_value = ifelse(hours > 0, counts / hours, NA_real_)
    )
  }
  rare <- classifica_anomalie_giornaliere(make_daily(c(0, 0, 0, 0, 1, 2)))
  stopifnot(!any(rare$outlier_lof[1:4]), !all(rare$outlier_lof))
  usual <- classifica_anomalie_giornaliere(make_daily(c(0, 1, 1, 1, 1, 1)))
  stopifnot(usual$outlier_lof[[1]])
  inactive <- classifica_anomalie_giornaliere(
    make_daily(c(0, 1, 1, 1, 1, 1), c(0, 1, 1, 1, 1, 1))
  )
  stopifnot(!inactive$outlier_lof[[1]])
  constant_rate <- classifica_anomalie_giornaliere(
    make_daily(c(1, 2, 4, 8, 12, 24), c(1, 2, 4, 8, 12, 24))
  )
  stopifnot(!any(constant_rate$outlier_lof))
  tails <- probabilita_code(c(0, 0), c(1, 10))
  stopifnot(tails$p_basso[[1]] > 0.3, tails$p_basso[[2]] < 0.001)
  cat("Self-test OK: 30 giorni, mediana zero, zero operativo, ore attive e code.\n")
}
if ("--self-test" %in% args) {
  self_test()
  quit(status = 0L)
}

input_path <- arg_value(
  "--input", file.path(project_dir, "02_Output", "sensor_count_increment.rds")
)
output_dir <- arg_value(
  "--output",
  file.path(project_dir, "02_Output", "anomaly_checks",
            format(Sys.time(), "%Y%m%d_%H%M%S"))
)
dati <- readRDS(input_path)
compatibility_notes <- character()
gateway_identity_verified <- "gateway_id" %in% names(dati)
if (!gateway_identity_verified) {
  if (!all(c("coupon", "gateway_name") %in% names(dati)) ||
      any(is.na(dati$gateway_name) | !nzchar(trimws(dati$gateway_name)))) {
    stop("Manca gateway_id e non e' disponibile un gateway_name valido.")
  }
  # Chiave SOLO diagnostica, distinta da un vero ID del gateway.
  dati$gateway_id <- paste0("diagnostic-name:", dati$gateway_name)
  compatibility_notes <- c(compatibility_notes,
    "gateway_id assente: raggruppamento diagnostico per coupon + gateway_name; identita' fisica non verificata.")
}
if (!"gateway_disconnect_affected" %in% names(dati)) {
  dati$gateway_disconnect_affected <- NA
  compatibility_notes <- c(compatibility_notes,
    "Flag disconnessione assente: impossibile escludere gli accumuli da disconnessione; risultati provvisori.")
}
if (length(compatibility_notes)) {
  warning(paste(compatibility_notes, collapse = "\n"), call. = FALSE)
}
required <- c(
  "company", "field", "project", "coupon", "machine_name",
  "gateway_id", "gateway_name", "sensor_id", "cds_name",
  "sensor_description", "day", "increment", "daily_uptime",
  "gateway_disconnect_affected", "count", "timestamp"
)
missing <- setdiff(required, names(dati))
if (length(missing)) stop("Colonne mancanti: ", paste(missing, collapse = ", "))
base <- prepara_valori_giornalieri_nok_data(dati)
keys <- c("coupon", "gateway_id", "sensor_id", "cds_name", "sensor_description")
maturity <- calcola_sensori_allarmabili(dati)
disconnect_quality <- dati |>
  group_by(across(all_of(c(keys, "day")))) |>
  summarise(
    disconnessioni_verificabili = all(!is.na(gateway_disconnect_affected)),
    .groups = "drop"
  )
base <- base |>
  left_join(maturity |> select(all_of(keys), n_giorni_conteggio), by = keys) |>
  left_join(disconnect_quality, by = c(keys, "day")) |>
  mutate(
    allarmabile = !is.na(n_giorni_conteggio),
    identita_gateway_verificata = gateway_identity_verified
  )
valid <- base |>
  filter(
    is.finite(daily_count), daily_count >= 0,
    abs(daily_count - round(daily_count)) < 1e-8,
    is.finite(daily_uptime), daily_uptime > 0,
    !is.na(day),
    !gateway_disconnect_affected
  ) |>
  arrange(coupon, gateway_id, sensor_id, day)
# I dati affetti da disconnessione restano nel file qualita_giornate.csv,
# ma non sono usati per stimare ne' valutare i modelli probabilistici.

lof_flags <- function(values, k) {
  if (length(values) < 6L || n_distinct(values) < 3L) {
    return(rep(FALSE, length(values)))
  }
  s <- dbscan::lof(matrix(values, ncol = 1),
                   minPts = min(k, length(values) - 1L))
  !is.na(s) & s > 2
}

prob_rows <- list()
model_rows <- list()
stability_rows <- list()
stability_summary <- list()
groups <- group_split(group_by(valid, across(all_of(keys))), .keep = TRUE)

for (g in groups) {
  meta <- g[1, keys, drop = FALSE]
  n <- nrow(g)
  n_train <- max(30L, floor(n * 0.7))
  n_test <- n - n_train
  if (n_train >= n || n_test < 10L) {
    model_rows[[length(model_rows) + 1L]] <- bind_cols(
      meta, tibble(
        modello = "non_stimato", n_giorni = n,
        n_train = min(n_train, n), n_test = max(0L, n_test),
        stato = "Servono almeno 30 giorni di training e 10 successivi di verifica"
      )
    )
  } else {
    train <- g[seq_len(n_train), ]
    test <- g[(n_train + 1L):n, ]
    fit_p <- tryCatch(
      glm(daily_count ~ 1 + offset(log(daily_uptime)),
          family = poisson(), data = train),
      error = function(e) e
    )
    fit_nb <- tryCatch(
      suppressWarnings(MASS::glm.nb(
        daily_count ~ 1 + offset(log(daily_uptime)), data = train
      )),
      error = function(e) e
    )
    fits <- list(Poisson = fit_p, Binomiale_negativa = fit_nb)
    for (model in names(fits)) {
      fit <- fits[[model]]
      ok <- !inherits(fit, "error") && isTRUE(fit$converged) &&
        all(is.finite(coef(fit)))
      if (model == "Binomiale_negativa" && ok) {
        ok <- is.finite(fit$theta) && fit$theta > 0 &&
          is.null(fit$th.warn)
      }
      if (!ok) {
        model_rows[[length(model_rows) + 1L]] <- bind_cols(
          meta, tibble(
            modello = model, n_giorni = n, n_train = n_train, n_test = n_test,
            stato = if (inherits(fit, "error")) conditionMessage(fit)
              else "Modello non convergente / dispersione non identificata"
          )
        )
        next
      }
      mu <- as.numeric(predict(fit, newdata = test, type = "response"))
      theta <- if (model == "Poisson") NULL else fit$theta
      tails <- probabilita_code(test$daily_count, mu, theta)
      detail <- bind_cols(
        test |> select(all_of(keys), day, daily_count, daily_uptime, allarmabile,
                         disconnessioni_verificabili, identita_gateway_verificata),
        tibble(modello = model, manovre_attese = mu, alpha = alpha), tails
      ) |>
        mutate(
          segnalazione_bassa = p_basso <= alpha / 2,
          segnalazione_alta = p_alto <= alpha / 2,
          segnalazione = segnalazione_bassa | segnalazione_alta
        )
      prob_rows[[length(prob_rows) + 1L]] <- detail
      model_rows[[length(model_rows) + 1L]] <- bind_cols(
        meta, tibble(
          modello = model, n_giorni = n, n_train = n_train, n_test = n_test,
          stato = "OK",
          ultimo_giorno_training = max(train$day),
          primo_giorno_verifica = min(test$day),
          manovre_ora_training = exp(coef(fit)[[1]]),
          dispersione_pearson = sum(residuals(fit, type = "pearson")^2) /
            fit$df.residual,
          theta = if (is.null(theta)) NA_real_ else theta,
          tasso_segnalazione_verifica = mean(detail$segnalazione),
          tasso_segnalazione_bassa = mean(detail$segnalazione_bassa),
          tasso_segnalazione_alta = mean(detail$segnalazione_alta)
        )
      )
    }
  }

  # Stabilita' separata dei soli LOF e delle regole operative complete.
  ks <- c(3L, 4L, 5L)
  lof_by_k <- sapply(ks, function(k) lof_flags(g$daily_value, k))
  all_by_k <- sapply(ks, function(k) {
    classifica_anomalie_giornaliere(g, k = k)$outlier_lof
  })
  baseline <- lof_flags(g$daily_value, 4L)
  judged <- integer(n)
  changed <- integer(n)
  candidates <- sample(seq_len(n), min(n, 30L))
  for (i in candidates) {
    survivors <- seq_len(n)[-i]
    reduced <- lof_flags(g$daily_value[survivors], 4L)
    judged[survivors] <- judged[survivors] + 1L
    changed[survivors] <- changed[survivors] +
      as.integer(reduced != baseline[survivors])
  }
  per_day <- g |>
    select(all_of(keys), day, daily_count, daily_uptime, daily_value, allarmabile,
           disconnessioni_verificabili, identita_gateway_verificata) |>
    mutate(
      lof_k3 = lof_by_k[, 1], lof_k4 = lof_by_k[, 2], lof_k5 = lof_by_k[, 3],
      regole_k3 = all_by_k[, 1], regole_k4 = all_by_k[, 2],
      regole_k5 = all_by_k[, 3],
      disaccordo_lof_k = rowSums(lof_by_k) > 0 & rowSums(lof_by_k) < 3,
      disaccordo_regole_k = rowSums(all_by_k) > 0 & rowSums(all_by_k) < 3,
      confronti_rimozione_giorno = judged,
      cambi_rimuovendo_giorno = changed,
      quota_cambi_rimozione = ifelse(judged > 0, changed / judged, NA_real_)
    )
  stability_rows[[length(stability_rows) + 1L]] <- per_day
  stability_summary[[length(stability_summary) + 1L]] <- bind_cols(
    meta, tibble(
      n_giorni = n, allarmabile = any(g$allarmabile),
      quota_disaccordo_lof_k = mean(per_day$disaccordo_lof_k),
      quota_disaccordo_regole_k = mean(per_day$disaccordo_regole_k),
      rimozioni_provate = length(candidates),
      quota_cambi_rimozione = if (sum(judged) > 0)
        sum(changed) / sum(judged) else NA_real_,
      stato_stabilita = if (n < 6L || n_distinct(g$daily_value) < 3L)
        "LOF non valutabile: pochi giorni o meno di 3 frequenze distinte"
        else "Valutato; nessuna soglia automatica di accettazione"
    )
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_table <- function(rows, filename) {
  x <- if (is.list(rows) && !is.data.frame(rows)) bind_rows(rows) else rows
  write.csv(x, file.path(output_dir, filename), row.names = FALSE, na = "")
}
write_table(base, "qualita_giornate.csv")
write_table(model_rows, "modelli_probabilistici.csv")
write_table(prob_rows, "limiti_probabilistici.csv")
write_table(stability_summary, "stabilita_riepilogo.csv")
write_table(stability_rows, "stabilita_giornate.csv")
writeLines(c(
  paste("Input:", normalizePath(input_path)),
  paste("Seed:", seed, "| alpha complessivo per punto:", alpha),
  compatibility_notes,
  "Consultare disconnessioni_verificabili e identita_gateway_verificata nei CSV.",
  "Training cronologico: almeno 30 giorni; verifica successiva: almeno 10.",
  "I modelli usano manovre giornaliere con ore attive come esposizione.",
  "Giornate con disconnessione, ore non valide o conteggi non interi escluse dai modelli.",
  "Limiti plug-in: non incorporano l'incertezza dei parametri stimati.",
  "Un tasso di segnalazione non e' un tasso di falsi allarmi senza etichette.",
  "LOF: confronto k=3,4,5; rimozione di fino a 30 giornate, con seed fisso.",
  "Nessuna modifica ai modelli di produzione o ai dati letti."
), file.path(output_dir, "LEGGIMI.txt"))
cat("Diagnostica completata:", normalizePath(output_dir), "\n")
