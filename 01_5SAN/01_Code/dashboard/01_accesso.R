library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)
library(shinyWidgets)
library(RColorBrewer)
library(here)
library(stringr)
library(lubridate)
library(scales)
library(DT)
library(dbscan)

# Guida sintetica condivisa dalle versioni interna e web.
source(here("01_Code", "09_Guida_Online.R"), local = TRUE)

# Modalita' applicativa:
# - internal: dashboard completa con selezione Azienda/Stabilimento/Macchina
# - web: macchina fissata dal parametro ?coupon=... della querystring
APP_MODE <- tolower(trimws(Sys.getenv("APP_MODE", unset = "internal")))
if (!APP_MODE %in% c("internal", "web")) {
  warning("APP_MODE non riconosciuto: uso 'internal'.")
  APP_MODE <- "internal"
}
IS_WEB_MODE <- identical(APP_MODE, "web")

# Configurazione OTP per la sola modalita' web.
# La secret non viene versionata: arriva dall'ambiente del container.
OTP_SECRET <- trimws(Sys.getenv("OTP_SECRET", unset = ""))
OTP_DIGITS <- suppressWarnings(as.integer(Sys.getenv("OTP_DIGITS", unset = "6")))
OTP_PERIOD <- suppressWarnings(as.numeric(Sys.getenv("OTP_PERIOD", unset = "30")))
OTP_ALGORITHM <- tolower(trimws(Sys.getenv("OTP_ALGORITHM", unset = "sha1")))
OTP_BEHIND <- suppressWarnings(as.integer(Sys.getenv("OTP_BEHIND", unset = "1")))

if (is.na(OTP_DIGITS) || OTP_DIGITS < 6L) OTP_DIGITS <- 6L
if (is.na(OTP_PERIOD) || OTP_PERIOD <= 0) OTP_PERIOD <- 30
if (!OTP_ALGORITHM %in% c("sha1", "sha256", "sha512")) OTP_ALGORITHM <- "sha1"
if (is.na(OTP_BEHIND) || OTP_BEHIND < 0L) OTP_BEHIND <- 1L

OTP_CONFIGURED <- nzchar(OTP_SECRET)
otp_verifier <- if (IS_WEB_MODE && OTP_CONFIGURED) {
  otp::TOTP$new(
    secret = OTP_SECRET,
    digits = OTP_DIGITS,
    period = OTP_PERIOD,
    algorithm = OTP_ALGORITHM
  )
} else {
  NULL
}

# Valida coupon + OTP. Viene usata prima di restituire l'HTML e anche
# lato server, cosi' il coupon non viene mai considerato affidabile senza
# una verifica valida.
valida_accesso_web <- function(query) {
  if (!IS_WEB_MODE) {
    return(list(ok = TRUE, coupon = NULL, reason = NULL, status = 200L))
  }

  if (!OTP_CONFIGURED || is.null(otp_verifier)) {
    return(list(
      ok = FALSE,
      coupon = NULL,
      reason = "Servizio temporaneamente non disponibile.",
      status = 503L
    ))
  }

  coupon <- query[["coupon"]]
  otp_code <- query[["otp"]]

  if (is.null(coupon) || length(coupon) < 1 ||
      is.null(otp_code) || length(otp_code) < 1) {
    return(list(
      ok = FALSE,
      coupon = NULL,
      reason = "Richiesta non valida.",
      status = 403L
    ))
  }

  coupon <- trimws(as.character(coupon[[1]]))
  otp_code <- trimws(as.character(otp_code[[1]]))

  if (!nzchar(coupon) || !grepl(paste0("^\\d{", OTP_DIGITS, "}$"), otp_code)) {
    return(list(
      ok = FALSE,
      coupon = NULL,
      reason = "Richiesta non valida.",
      status = 403L
    ))
  }

  otp_ok <- tryCatch(
    !is.null(otp_verifier$verify(otp_code, behind = OTP_BEHIND)),
    error = function(e) FALSE
  )

  if (!isTRUE(otp_ok)) {
    return(list(
      ok = FALSE,
      coupon = NULL,
      reason = "Codice di accesso non valido o scaduto.",
      status = 403L
    ))
  }

  if (!coupon %in% macchine_lookup$coupon) {
    return(list(
      ok = FALSE,
      coupon = NULL,
      reason = "Macchina non disponibile.",
      status = 403L
    ))
  }

  list(ok = TRUE, coupon = coupon, reason = NULL, status = 200L)
}

risposta_errore_web <- function(status, reason) {
  titolo <- if (identical(as.integer(status), 503L)) {
    "Servizio non disponibile"
  } else {
    "Accesso non consentito"
  }

  html <- paste0(
    "<!doctype html><html lang='it'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", titolo, "</title></head>",
    "<body style='margin:0;font-family:Arial,Helvetica,sans-serif;background:#F5F6F7;'>",
    "<div style='min-height:100vh;display:flex;align-items:center;justify-content:center;padding:24px;'>",
    "<div style='max-width:560px;width:100%;background:#fff;border:1px solid #DDE4EA;",
    "border-radius:10px;padding:34px;box-sizing:border-box;box-shadow:0 4px 18px rgba(0,0,0,.06);'>",
    "<h2 style='margin-top:0;color:#234A66;'>", titolo, "</h2>",
    "<p style='color:#5F6B76;font-size:16px;margin-bottom:0;'>", reason, "</p>",
    "</div></div></body></html>"
  )

  shiny::httpResponse(
    status = as.integer(status),
    content_type = "text/html; charset=UTF-8",
    content = html,
    headers = list("Cache-Control" = "no-store")
  )
}

# Le immagini non sono nella cartella www: le espongo a Shiny con un
# resource path dedicato. Se la cartella non esiste, l'app continua comunque
# a funzionare e nella Home non viene mostrato alcuno schema.
# Asset grafici header conservati nella radice 01_5SAN, senza cartella www.
addResourcePath("san-header", here())
cds_images_dir <- here("00_Data", "02_CdS")
if (dir.exists(cds_images_dir)) {
  addResourcePath("cds-images", cds_images_dir)
}

# DOCX statici scaricabili dalla dashboard. Restano fuori da www:
# il download passa quindi sempre da Shiny.
documenti_dir <- here("00_Data", "03_Documenti")
documenti_files <- if (dir.exists(documenti_dir)) {
  sort(
    list.files(
      documenti_dir,
      pattern = "\\.docx$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  )
} else {
  character(0)
}

