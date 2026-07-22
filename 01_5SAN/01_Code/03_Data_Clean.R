library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(stringr)
library(here)

# carico i dati 
raw_data <- readRDS(here("02_Output", "raw_data.rds"))

# controllo duplicati
duplicati <- raw_data |>
  count(
    coupon,
    gateway_name,
    sensor_id,
    timestamp,
    name = "n") |>
  filter(n > 1)

if (nrow(duplicati) > 0) {
  warning("Sono presenti ", nrow(duplicati), " chiavi duplicate")
}