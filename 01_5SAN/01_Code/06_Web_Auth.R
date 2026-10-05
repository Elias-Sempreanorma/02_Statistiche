library(httpuv)
library(shiny)
library(otp)

OTP_SECRET <- trimws(Sys.getenv("OTP_SECRET", unset = ""))
OTP_DIGITS <- suppressWarnings(as.integer(Sys.getenv("OTP_DIGITS", unset = "6")))
OTP_PERIOD <- suppressWarnings(as.numeric(Sys.getenv("OTP_PERIOD", unset = "30")))
OTP_ALGORITHM <- tolower(trimws(Sys.getenv("OTP_ALGORITHM", unset = "sha1")))
OTP_BEHIND <- suppressWarnings(as.integer(Sys.getenv("OTP_BEHIND", unset = "1")))

if (is.na(OTP_DIGITS) || OTP_DIGITS < 6L) OTP_DIGITS <- 6L
if (is.na(OTP_PERIOD) || OTP_PERIOD <= 0) OTP_PERIOD <- 30
if (!OTP_ALGORITHM %in% c("sha1", "sha256", "sha512")) OTP_ALGORITHM <- "sha1"
if (is.na(OTP_BEHIND) || OTP_BEHIND < 0L) OTP_BEHIND <- 1L

otp_verifier <- if (nzchar(OTP_SECRET)) {
  otp::TOTP$new(
    secret = OTP_SECRET,
    digits = OTP_DIGITS,
    period = OTP_PERIOD,
    algorithm = OTP_ALGORITHM
  )
} else {
  NULL
}

valid_coupons <- tryCatch({
  dati <- readRDS("02_Output/sensor_count_increment.rds")
  unique(trimws(as.character(dati$coupon[!is.na(dati$coupon)])))
}, error = function(e) character(0))

reply <- function(status, body) {
  list(
    status = as.integer(status),
    headers = list(
      "Content-Type" = "text/plain; charset=UTF-8",
      "Cache-Control" = "no-store"
    ),
    body = body
  )
}

app <- list(
  call = function(req) {
    if (is.null(otp_verifier) || length(valid_coupons) == 0L) {
      return(reply(503L, "Authentication service unavailable"))
    }

    query_string <- req$HTTP_X_ORIGINAL_ARGS
    if (is.null(query_string) || !nzchar(query_string)) {
      query_string <- req$QUERY_STRING
    }
    if (is.null(query_string)) query_string <- ""

    query <- shiny::parseQueryString(query_string)
    coupon <- query[["coupon"]]
    otp_code <- query[["otp"]]

    if (is.null(coupon) || length(coupon) < 1L ||
        is.null(otp_code) || length(otp_code) < 1L) {
      return(reply(403L, "Forbidden"))
    }

    coupon <- trimws(as.character(coupon[[1]]))
    otp_code <- trimws(as.character(otp_code[[1]]))

    if (!nzchar(coupon) ||
        !grepl(paste0("^\\d{", OTP_DIGITS, "}$"), otp_code)) {
      return(reply(403L, "Forbidden"))
    }

    otp_ok <- tryCatch(
      !is.null(otp_verifier$verify(otp_code, behind = OTP_BEHIND)),
      error = function(e) FALSE
    )

    if (!isTRUE(otp_ok) || !coupon %in% valid_coupons) {
      return(reply(403L, "Forbidden"))
    }

    reply(200L, "OK")
  }
)

httpuv::runServer("0.0.0.0", 8080, app)
