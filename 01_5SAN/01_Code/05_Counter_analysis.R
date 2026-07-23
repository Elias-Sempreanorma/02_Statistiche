library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(stringr)
library(here)

# carico i dati 
raw_data <- readRDS(here("02_Output", "raw_data.rds"))

# Per ogni sensore confrontiamo il contatore
# con la rilevazione precedente.
controllo_count <- raw_data |>
  group_by(coupon, gateway_name, cds_name) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    timestamp_precedente = lag(timestamp),
    count_precedente = coalesce(lag(count), count),
    
    # Differenza grezza del contatore cumulativo
    differenza_count = count - count_precedente,
    
    # Tempo tra le due rilevazioni
    intervallo_secondi = as.numeric(
      difftime(timestamp, timestamp_precedente, units = "secs")
    )
  ) |>
  ungroup()

# Casi in cui il contatore diminuisce
count_diminuito <- controllo_count |>
  filter(differenza_count < 0)

# Casi in cui il contatore aumenta
count_aumentato <- controllo_count |>
  filter(differenza_count > 0)

# Casi in cui il contatore resta uguale
count_invariato <- controllo_count |>
  filter(differenza_count == 0)


# Classificazione semplice del comportamento del contatore.
# Non stiamo ancora eliminando o modificando valori.
controllo_count <- raw_data |>
  group_by(coupon, gateway_name, cds_name) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    timestamp_precedente = lag(timestamp),
    
    # Sulla prima riga uso il count stesso.
    # Quindi la prima differenza è sempre 0.
    count_precedente = if_else(
      is.na(timestamp_precedente),
      count,
      lag(count)
    ),
    
    differenza_count = count - count_precedente,
    
    intervallo_secondi = as.numeric(
      difftime(timestamp, timestamp_precedente, units = "secs")
    ),
    
    tipo_movimento_count = case_when(
      is.na(timestamp_precedente) ~ "prima_rilevazione",
      differenza_count > 0 ~ "aumento",
      differenza_count == 0 ~ "invariato",
      differenza_count < 0 ~ "diminuzione"
    ),
    
    attivazioni = case_when(
      tipo_movimento_count == "prima_rilevazione" ~ 0,
      tipo_movimento_count == "aumento" ~ differenza_count,
      tipo_movimento_count == "invariato" ~ 0,
      tipo_movimento_count == "diminuzione" ~ NA_real_
    )
  ) |>
  ungroup()

# Aggiungiamo il tipo di intervallo del gateway:
# normale, grande, piccolissimo, anomalo_intermedio, ecc.
controllo_count <- controllo_count |>
  left_join(
    gateway_intervalli |>
      select(coupon, gateway_name, timestamp, tipo_intervallo),
    by = c("coupon", "gateway_name", "timestamp")
  )

# Le attivazioni sono la differenza positiva del contatore.
# Le diminuzioni restano NA perché vanno controllate a parte.
controllo_count <- controllo_count |>
  mutate(
    attivazioni = case_when(
      tipo_movimento_count == "aumento" ~ differenza_count,
      tipo_movimento_count == "invariato" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Casi problematici o delicati da ispezionare.
controllo_count_sensore <- controllo_count |>
  filter(
    tipo_movimento_count == "diminuzione" |
      tipo_intervallo != "normale"
  ) |>
  select(
    coupon, machine_name, gateway_name,
    cds_name, timestamp_precedente, timestamp,
    intervallo_secondi, tipo_intervallo,
    count_precedente, count,
    differenza_count, tipo_movimento_count
  )


# Prepariamo le attivazioni usabili per le statistiche orarie.
# Usiamo solo:
# - contatore aumentato o invariato
# - intervallo gateway normale
count_movimenti_orari <- controllo_count |>
  mutate(
    ora = floor_date(timestamp, "hour"),
    
    # attivazioni_valide = case_when(
    #   tipo_intervallo == "normale" &
    #     tipo_movimento_count %in% c("aumento", "invariato") ~ attivazioni,
    #   TRUE ~ NA_real_
    # )
  )

# Tabella oraria per sensore.
# Questa sarà la base per giorno e settimana.
sensore_orario <- count_movimenti_orari |>
  group_by(coupon, machine_name, gateway_name, cds_name, ora) |>
  summarise(
    attivazioni_orarie = somma_o_na(attivazioni),
    
    numero_rilevazioni = n(),
    numero_rilevazioni_valide = sum(!is.na(attivazioni)),
    
    presenza_intervalli_anomali = any(tipo_intervallo != "normale", na.rm = TRUE),
    presenza_diminuzioni_count = any(tipo_movimento_count == "diminuzione", na.rm = TRUE),
    
    .groups = "drop"
  )

# Statistiche giornaliere per sensore.
# Ogni riga rappresenta un sensore in una giornata.
# Statistiche giornaliere robuste per sensore
sensore_giornaliero <- sensore_orario |>
  mutate(giorno = as.Date(ora)) |>
  group_by(coupon, machine_name, gateway_name, cds_name, giorno) |>
  summarise(
    attivazioni_totali = somma_o_na(attivazioni_orarie),
    media_oraria = stat_o_na(attivazioni_orarie, mean),
    mediana_oraria = stat_o_na(attivazioni_orarie, median),
    deviazione_standard_oraria = stat_o_na(attivazioni_orarie, sd),
    minimo_orario = stat_o_na(attivazioni_orarie, min),
    massimo_orario = stat_o_na(attivazioni_orarie, max),
    ore_presenti = n(),
    ore_valide = sum(!is.na(attivazioni_orarie)),
    copertura_ore = ore_valide / 24,
    ore_con_anomalie = sum(presenza_intervalli_anomali, na.rm = TRUE),
    ore_con_diminuzioni_count = sum(presenza_diminuzioni_count, na.rm = TRUE),
    .groups = "drop"
  )

# Statistiche settimanali per sensore.
# La settimana parte da lunedì.
sensore_settimanale <- sensore_orario |>
  mutate(settimana = floor_date(ora, "week", week_start = 1)) |>
  group_by(coupon, machine_name, gateway_name, cds_name, settimana) |>
  summarise(
    attivazioni_totali = somma_o_na(attivazioni_orarie),
    media_oraria = stat_o_na(attivazioni_orarie, mean),
    mediana_oraria = stat_o_na(attivazioni_orarie, median),
    deviazione_standard_oraria = stat_o_na(attivazioni_orarie, sd),
    minimo_orario = stat_o_na(attivazioni_orarie, min),
    massimo_orario = stat_o_na(attivazioni_orarie, max),
    ore_presenti = n(),
    ore_valide = sum(!is.na(attivazioni_orarie)),
    copertura_ore = ore_valide / 168,
    ore_con_anomalie = sum(presenza_intervalli_anomali, na.rm = TRUE),
    ore_con_diminuzioni_count = sum(presenza_diminuzioni_count, na.rm = TRUE),
    .groups = "drop"
  )


# getway

# Aggregazione oraria per gateway.
# Sommiamo le attivazioni dei sensori validi nella stessa ora.
gateway_orario <- sensore_orario |>
  group_by(coupon, machine_name, gateway_name, ora) |>
  summarise(
    attivazioni_orarie = somma_o_na(attivazioni_orarie),
    sensori_presenti = n(),
    sensori_validi = sum(!is.na(attivazioni_orarie)),
    sensori_con_anomalie = sum(presenza_intervalli_anomali, na.rm = TRUE),
    sensori_con_diminuzioni_count = sum(presenza_diminuzioni_count, na.rm = TRUE),
    presenza_intervalli_anomali = any(presenza_intervalli_anomali, na.rm = TRUE),
    presenza_diminuzioni_count = any(presenza_diminuzioni_count, na.rm = TRUE),
    .groups = "drop"
  )

# Statistiche giornaliere per gateway.
# Ogni riga rappresenta un gateway in una giornata.
gateway_giornaliero <- gateway_orario |>
  mutate(giorno = as.Date(ora)) |>
  group_by(coupon, machine_name, gateway_name, giorno) |>
  summarise(
    attivazioni_totali = somma_o_na(attivazioni_orarie),
    media_oraria = stat_o_na(attivazioni_orarie, mean),
    mediana_oraria = stat_o_na(attivazioni_orarie, median),
    deviazione_standard_oraria = stat_o_na(attivazioni_orarie, sd),
    minimo_orario = stat_o_na(attivazioni_orarie, min),
    massimo_orario = stat_o_na(attivazioni_orarie, max),
    ore_presenti = n(),
    ore_valide = sum(!is.na(attivazioni_orarie)),
    copertura_ore = ore_valide / 24,
    ore_con_anomalie = sum(presenza_intervalli_anomali, na.rm = TRUE),
    ore_con_diminuzioni_count = sum(presenza_diminuzioni_count, na.rm = TRUE),
    .groups = "drop"
  )

# Statistiche settimanali per gateway.
# Settimana con inizio lunedì.
gateway_settimanale <- gateway_orario |>
  mutate(settimana = floor_date(ora, "week", week_start = 1)) |>
  group_by(coupon, machine_name, gateway_name, settimana) |>
  summarise(
    attivazioni_totali = somma_o_na(attivazioni_orarie),
    media_oraria = stat_o_na(attivazioni_orarie, mean),
    mediana_oraria = stat_o_na(attivazioni_orarie, median),
    deviazione_standard_oraria = stat_o_na(attivazioni_orarie, sd),
    minimo_orario = stat_o_na(attivazioni_orarie, min),
    massimo_orario = stat_o_na(attivazioni_orarie, max),
    ore_presenti = n(),
    ore_valide = sum(!is.na(attivazioni_orarie)),
    copertura_ore = ore_valide / 168,
    ore_con_anomalie = sum(presenza_intervalli_anomali, na.rm = TRUE),
    ore_con_diminuzioni_count = sum(presenza_diminuzioni_count, na.rm = TRUE),
    .groups = "drop"
  )
