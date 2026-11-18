# Autenticazione locale iniziale. Configurazione lato server, mai in Git.
# ACCESSI_NEAR_MISS_JSON: array di {account, hash_password, coupon:[...]}.
# Gli hash si generano con sodium::password_store("password-forte").
# Nessun account di default: fail-closed se la configurazione manca.
carica_accessi <- function() {
  config <- Sys.getenv("ACCESSI_NEAR_MISS_JSON")
  if (!nzchar(config)) stop("ACCESSI_NEAR_MISS_JSON non configurato: accesso disabilitato.")
  righe <- jsonlite::fromJSON(config, simplifyVector=FALSE)
  if (!is.list(righe) || !length(righe)) stop("Configurazione accessi vuota.")
  for (a in righe) {
    if (!is.character(a$account) || !nzchar(a$account) ||
        !is.character(a$hash_password) || !nzchar(a$hash_password) ||
        !length(a$coupon) || !all(nzchar(unlist(a$coupon))))
      stop("Account o autorizzazioni non validi.")
  }
  ids <- vapply(righe, function(a) a$account, character(1))
  if (anyDuplicated(ids)) stop("Account duplicati.")
  righe
}

verifica_credenziali <- function(account, password, accessi) {
  if (!nzchar(testo(account)) || !nzchar(testo(password))) return(NULL)
  profilo <- Filter(function(a) identical(a$account, testo(account)), accessi)
  if (length(profilo) != 1L) return(NULL)
  if (!isTRUE(tryCatch(sodium::password_verify(profilo[[1]]$hash_password,
                                           password),error=function(e) FALSE))) return(NULL)
  list(account=profilo[[1]]$account, coupon=unique(unlist(profilo[[1]]$coupon,
                                                      use.names=FALSE)))
}
