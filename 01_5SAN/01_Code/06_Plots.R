library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(stringr)
library(here)
library(slider)

# Grafico orario di una macchina.
# Ogni pannello è un sensore.
# Usiamo sensore_orario perché vogliamo vedere i sensori separati.


# indicare coupon, periodo ("orario"/ "giornaliero" / "settimanale"), sensori e x_breaks
plot_macchina_attivazioni("LIN-SEMP-3625-7560-4998-4816-219", periodo = "orario", x_breaks = "1 hour")

plot_macchina_attivazioni("CAD-RITE-8841-6712-8594-2083-027", periodo = "giornaliero", sensori = c("Output1", "Sensor 3"), x_breaks = "1 day")
