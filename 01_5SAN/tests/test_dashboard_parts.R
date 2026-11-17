# Eseguire dalla cartella 01_5SAN. Nessun DB, file operativo o avvio HTTP.
library(shiny)

entry <- as.list(parse("01_Code/05_Plots.R", keep.source = FALSE))
assignment <- function(name) {
  matches <- Filter(function(x) is.call(x) &&
    identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name(name)), entry)
  stopifnot(length(matches) == 1L)
  matches[[1]]
}

# Verifica tutti i file, incluse le parti server non caricate con source().
for (path in list.files("01_Code/dashboard", pattern = "\\.R$", full.names = TRUE)) {
  parse(path)
}
env <- new.env(parent = globalenv())
env$here <- function(...) file.path(...)
eval(assignment("dashboard_server_parts"), env)
stopifnot(length(env$dashboard_server_parts) == 13L,
          all(vapply(env$dashboard_server_parts, is.expression, logical(1))))

# Usa l'effettivo wrapper server con due parti sintetiche. Le callback della
# prima devono risolvere le funzioni della seconda nello stesso ambiente;
# due sessioni devono invece mantenere valori indipendenti.
env$dashboard_server_parts <- list(
  expression(
    value <- reactiveVal(0),
    observeEvent(input$set, value(input$set)),
    output$value <- renderText(format_value(value()))
  ),
  expression(format_value <- function(x) paste0("valore=", x))
)
eval(assignment("server"), env)
testServer(env$server, {
  session$setInputs(set = 7)
  stopifnot(identical(output$value, "valore=7"))
})
testServer(env$server, {
  stopifnot(identical(output$value, "valore=0"))
  session$setInputs(set = 3)
  stopifnot(identical(output$value, "valore=3"))
})
cat("OK: parsing delle parti, dipendenze tra parti e isolamento delle sessioni.\n")
