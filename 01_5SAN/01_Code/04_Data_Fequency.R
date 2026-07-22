library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(stringr)
library(here)

# carico i dati 
raw_data <- readRDS(here("02_Output", "raw_data.rds"))

# Intervalli tra gli invii effettivi di ogni gateway
gateway_invii <- raw_data |>
  # distinct(coupon, machine_name, gateway_name, timestamp) |>
  group_by(coupon, machine_name, gateway_name) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    timestamp_precedente = lag(timestamp),
    intervallo_secondi = as.numeric(
      difftime(timestamp, timestamp_precedente, units = "secs")
    )
  ) |>
  ungroup()

# Identifica i due intervalli più frequenti per gateway.
# Arrotondati al secondo per evitare differenze decimali irrilevanti.
modalita_gateway <- gateway_invii |>
  filter(!is.na(intervallo_secondi)) |>
  mutate(intervallo_secondi = round(intervallo_secondi)) |>
  count(coupon, machine_name, gateway_name,
        intervallo_secondi, name = "frequenza") |>
  group_by(coupon, machine_name, gateway_name) |>
  slice_max(frequenza, n = 2, with_ties = FALSE) |>
  arrange(intervallo_secondi, .by_group = TRUE) |>
  mutate(numero_moda = paste0("moda_", row_number())) |>
  ungroup() |>
  select(-frequenza) |>
  pivot_wider(
    names_from = numero_moda,
    values_from = intervallo_secondi
  )

# Un intervallo è normale se dista al massimo il 20%
# da almeno una delle due modalità principali.
tolleranza <- 0.20

gateway_intervalli <- gateway_invii |>
  left_join(
    modalita_gateway,
    by = c("coupon", "machine_name", "gateway_name")
  ) |>
  mutate(
    distanza_moda_1 = abs(intervallo_secondi - moda_1) / moda_1,
    distanza_moda_2 = abs(intervallo_secondi - moda_2) / moda_2,
    distanza_minima = pmin(distanza_moda_1, distanza_moda_2, na.rm = TRUE),
    
    tipo_intervallo = case_when(
      is.na(intervallo_secondi) ~ "primo_invio",
      distanza_minima <= tolleranza ~ "normale",
      intervallo_secondi < pmin(moda_1, moda_2, na.rm = TRUE) ~ "piccolissimo",
      intervallo_secondi > pmax(moda_1, moda_2, na.rm = TRUE) ~ "grande",
      TRUE ~ "anomalo_intermedio"
    )
  )

# Tabella con i periodi di mancata trasmissione
# o con invii insolitamente ravvicinati.
intervalli_anomali_gateway <- gateway_intervalli |>
  filter(tipo_intervallo != "normale",
         tipo_intervallo != "primo_invio") |>
  mutate(durata_minuti = intervallo_secondi / 60) |>
  select(
    coupon, machine_name, gateway_name,
    timestamp_precedente, timestamp,
    intervallo_secondi, durata_minuti,
    moda_1, moda_2, tipo_intervallo
  )

# Statistiche della frequenza ordinaria degli invii.
# Gli intervalli piccoli, grandi o intermedi vengono esclusi.
frequenza_gateway <- gateway_intervalli |>
  filter(tipo_intervallo == "normale") |>
  group_by(coupon, machine_name, gateway_name) |>
  summarise(
    numero_intervalli_normali = n(),
    intervallo_medio_sec = mean(intervallo_secondi, na.rm = TRUE),
    intervallo_mediano_sec = median(intervallo_secondi, na.rm = TRUE),
    deviazione_standard_sec = sd(intervallo_secondi, na.rm = TRUE),
    intervallo_minimo_sec = min(intervallo_secondi, na.rm = TRUE),
    intervallo_massimo_sec = max(intervallo_secondi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    intervallo_medio_min = intervallo_medio_sec / 60,
    intervallo_mediano_min = intervallo_mediano_sec / 60
  )

# Numero e durata complessiva delle anomalie per gateway.
riepilogo_intervalli_anomali <- gateway_intervalli |>
  filter(tipo_intervallo %in% c(
    "piccolissimo", "grande", "anomalo_intermedio"
  )) |>
  group_by(coupon, machine_name, gateway_name, tipo_intervallo) |>
  summarise(
    numero_intervalli = n(),
    durata_totale_sec = sum(intervallo_secondi, na.rm = TRUE),
    durata_massima_sec = max(intervallo_secondi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    durata_totale_ore = durata_totale_sec / 3600,
    durata_massima_ore = durata_massima_sec / 3600
  )
