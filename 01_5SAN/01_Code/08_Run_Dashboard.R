library(shiny)

shiny::runApp(
  appDir = "01_Code/06_Plots.R",
  host = "0.0.0.0",
  port = 3838,
  launch.browser = FALSE
)