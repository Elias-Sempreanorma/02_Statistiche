library(dplyr)
library(lubridate)
library(here)

# ---------------------------------------------------------------------------
# Uptime giornaliero per gateway basato su SystemInfo.uptime.
#
# Regole:
# - SystemInfo e' emesso circa ogni ora.
# - uptime e' cumulativo in secondi e si azzera al reset del gateway.
# - se uptime_B < uptime_A, il reset e' avvenuto.
# - restart_ts = timestamp_B - uptime_B.
# - per ogni sensore del gateway considero il primo count con timestamp
#   successivo a restart_ts e ne calcolo l'incremento rispetto al count
#   precedente dello stesso sensore.
# - se almeno un sensore ha primo incremento = 0 oppure > 1:
#     probabile disconnessione gateway -> recupero il buco;
# - se tutti i sensori osservabili hanno primo incremento = 1:
#     probabile riavvio macchina -> NON recupero il buco.
# - i sensori senza un count successivo a restart_ts non partecipano
#   alla classificazione.
# - gli intervalli vengono uniti prima della somma per evitare doppi conteggi.
# - aggiungo 1 ora finale di buffer giornaliero, per coprire la possibile
#   coda dopo l'ultimo SystemInfo della giornata; massimo 24 ore.
# ---------------------------------------------------------------------------

raw_data <- readRDS(here("02_Output", "raw_data.rds"))
system_info <- readRDS(here("02_Output", "system_info.rds"))

# Serie count con incremento per sensore, usata solo per classificare i reset.
count_events <- raw_data |>
  filter(
    !is.na(gateway_id),
    !is.na(sensor_id),
    !is.na(timestamp),
    is.finite(count)
  ) |>
  transmute(
    coupon,
    gateway_id,
    gateway_name,
    sensor_id,
    timestamp = with_tz(timestamp, "Europe/Rome"),
    count
  ) |>
  group_by(gateway_id, sensor_id) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    previous_count = lag(count),
    increment = count - previous_count
  ) |>
  ungroup()

# Helper: unisce intervalli sovrapposti/adiacenti per gateway.
merge_intervals <- function(df) {
  if (nrow(df) == 0) return(df)

  df <- df |>
    arrange(start, end)

  out <- vector("list", nrow(df))
  k <- 1L
  cur_start <- df$start[1]
  cur_end <- df$end[1]

  if (nrow(df) > 1) {
    for (i in 2:nrow(df)) {
      if (df$start[i] <= cur_end) {
        cur_end <- max(cur_end, df$end[i])
      } else {
        out[[k]] <- tibble(start = cur_start, end = cur_end)
        k <- k + 1L
        cur_start <- df$start[i]
        cur_end <- df$end[i]
      }
    }
  }

  out[[k]] <- tibble(start = cur_start, end = cur_end)
  bind_rows(out[seq_len(k)])
}

# Helper: spezza un intervallo sulle giornate locali Europe/Rome.
split_interval_by_day <- function(start, end) {
  if (is.na(start) || is.na(end) || end <= start) {
    return(tibble(day = as.Date(character()), start = as.POSIXct(character()), end = as.POSIXct(character())))
  }

  tz_use <- "Europe/Rome"
  day_start <- as.Date(start, tz = tz_use)
  day_end <- as.Date(end - seconds(1), tz = tz_use)
  days <- seq(day_start, day_end, by = "day")

  bind_rows(lapply(days, function(d) {
    d_start <- as.POSIXct(d, tz = tz_use)
    d_end <- d_start + days(1)
    tibble(
      day = d,
      start = max(start, d_start),
      end = min(end, d_end)
    )
  }))
}

# ---------------------------------------------------------------------------
# Intervalli certi derivati direttamente dall'uptime.
#
# Ogni SystemInfo B certifica che il gateway e' rimasto acceso almeno
# nell'intervallo [timestamp_B - uptime_B, timestamp_B].
# ---------------------------------------------------------------------------
system_info_clean <- system_info |>
  filter(
    !is.na(coupon),
    !is.na(gateway_id),
    !is.na(timestamp),
    is.finite(uptime),
    uptime >= 0
  ) |>
  mutate(timestamp = with_tz(timestamp, "Europe/Rome")) |>
  arrange(gateway_id, timestamp)

base_intervals <- system_info_clean |>
  transmute(
    coupon,
    gateway_id,
    gateway_name,
    start = timestamp - seconds(uptime),
    end = timestamp,
    source = "systeminfo"
  ) |>
  filter(end > start)

# ---------------------------------------------------------------------------
# Classificazione dei reset e recupero dei buchi da disconnessione gateway.
# ---------------------------------------------------------------------------
reset_candidates <- system_info_clean |>
  group_by(gateway_id) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    previous_timestamp = lag(timestamp),
    previous_uptime = lag(uptime),
    reset = !is.na(previous_uptime) & uptime < previous_uptime,
    restart_ts = timestamp - seconds(uptime)
  ) |>
  filter(reset) |>
  ungroup()

recovered_intervals <- lapply(seq_len(nrow(reset_candidates)), function(i) {
  reset_row <- reset_candidates[i, ]

  gateway_counts <- count_events |>
    filter(gateway_id == reset_row$gateway_id)

  sensor_ids <- gateway_counts |>
    distinct(sensor_id) |>
    pull(sensor_id)

  if (length(sensor_ids) == 0) return(NULL)

  first_after_by_sensor <- lapply(sensor_ids, function(sid) {
    sensor_data <- gateway_counts |>
      filter(sensor_id == sid) |>
      arrange(timestamp)

    after <- sensor_data |>
      filter(timestamp > reset_row$restart_ts) |>
      slice_head(n = 1)

    if (nrow(after) == 0) return(NULL)

    tibble(
      sensor_id = sid,
      first_timestamp = after$timestamp,
      first_increment = after$increment
    )
  }) |>
    bind_rows()

  # Sensori senza misura successiva non partecipano.
  observable <- first_after_by_sensor |>
    filter(is.finite(first_increment))

  if (nrow(observable) == 0) return(NULL)

  # Disconnessione gateway se almeno un sensore mostra incremento 0 o >1.
  gateway_disconnect <- any(
    observable$first_increment == 0 |
      observable$first_increment > 1
  )

  # Riavvio macchina solo se tutti i sensori osservabili mostrano +1.
  machine_restart <- all(observable$first_increment == 1)

  if (!gateway_disconnect || machine_restart) return(NULL)

  # Costruisco un intervallo recuperato per ogni sensore che ha fornito
  # un'osservazione post-reset. Per ciascuno parto dal massimo tra:
  # - timestamp SystemInfoA
  # - ultimo count precedente a restart_ts dello stesso sensore
  recovered <- lapply(seq_len(nrow(observable)), function(j) {
    sid <- observable$sensor_id[j]
    end_ts <- observable$first_timestamp[j]

    last_pre_count <- gateway_counts |>
      filter(
        sensor_id == sid,
        timestamp < reset_row$restart_ts
      ) |>
      arrange(desc(timestamp)) |>
      slice_head(n = 1) |>
      pull(timestamp)

    if (length(last_pre_count) == 0) {
      start_ts <- reset_row$previous_timestamp
    } else {
      start_ts <- max(reset_row$previous_timestamp, last_pre_count)
    }

    if (is.na(start_ts) || is.na(end_ts) || end_ts <= start_ts) return(NULL)

    tibble(
      coupon = reset_row$coupon,
      gateway_id = reset_row$gateway_id,
      gateway_name = reset_row$gateway_name,
      start = start_ts,
      end = end_ts,
      source = "gateway_disconnect"
    )
  }) |>
    bind_rows()

  recovered
}) |>
  bind_rows()

all_intervals <- bind_rows(base_intervals, recovered_intervals)

# ---------------------------------------------------------------------------
# Unione intervalli per gateway, poi spezzatura per giornata.
# ---------------------------------------------------------------------------
merged_intervals <- all_intervals |>
  group_by(coupon, gateway_id) |>
  group_modify(~ {
    merged <- merge_intervals(.x |> select(start, end))
    merged
  }) |>
  ungroup()

daily_intervals <- lapply(seq_len(nrow(merged_intervals)), function(i) {
  row <- merged_intervals[i, ]
  split_interval_by_day(row$start, row$end) |>
    mutate(
      coupon = row$coupon,
      gateway_id = row$gateway_id
    ) |>
    select(coupon, gateway_id, day, start, end)
}) |>
  bind_rows()

uptime <- daily_intervals |>
  mutate(
    interval_hours = as.numeric(difftime(end, start, units = "hours"))
  ) |>
  group_by(coupon, gateway_id, day) |>
  summarise(
    observed_uptime = sum(interval_hours, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # Buffer finale: SystemInfo e' orario, quindi dopo l'ultima osservazione
    # possono esserci fino a circa 60 minuti di accensione non ancora inviati.
    daily_uptime = pmin(observed_uptime + 1, 24)
  ) |>
  select(coupon, gateway_id, day, daily_uptime)

saveRDS(uptime, here("02_Output", "uptime.rds"))
