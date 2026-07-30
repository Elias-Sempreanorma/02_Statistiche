# funzione per connettersi a DB postgres

connetti_postgres <- function(database) {
  readRenviron(db)
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PG_HOST"),
    port = as.integer(Sys.getenv("PG_PORT")),
    dbname = database,
    user = Sys.getenv("PG_USER"),
    password = Sys.getenv("PG_PASSWORD")
  )
}


# funzione per connettersi a Maria DB

connetti_maria <- function() {
  readRenviron(db)
  
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
}


# funzioni controllo valori nulli

# Funzione somma solo se esiste almeno un valore valido.
# Evita di trasformare ore senza dati validi in finti zeri.
somma_o_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

# Funzione per calcolare statistiche evitando NaN o Inf
stat_o_na <- function(x, fun) {
  if (all(is.na(x))) NA_real_ else fun(x, na.rm = TRUE)
}

