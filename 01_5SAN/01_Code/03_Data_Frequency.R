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
# - per ogni sensore del gateway considero il primo count successivo
#   al timestamp dell'ultimo SystemInfo prima del reset + 1 ora.
#   Ne calcolo l'incremento rispetto al count precedente dello stesso sensore.
#   restart_ts resta usato per la stima del riavvio e dell'uptime.
# - una disconnessione rilevante viene registrata solo quando almeno un
#   sensore mostra un primo incremento > 1.
# - incrementi 0 o 1 non generano da soli un evento di disconnessione.
# - il buco tra l'ultimo SystemInfo precedente e il restart NON viene
#   recuperato nell'uptime: non ricostruiamo tempo non osservato.
# - i segmenti di uptime realmente osservati prima/dopo eventuali reset
#   vengono invece uniti e sommati, anche quando il reset avviene nello
#   stesso giorno.
# - il buffer finale resta modellato come intervallo di 1 ora dopo l'ultimo
#   SystemInfo giornaliero e viene unito agli intervalli osservati.
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

# Helper: unisce intervalli sovrapposti/adiacenti.
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
    return(
      tibble(
        day = as.Date(character()),
        start = as.POSIXct(character()),
        end = as.POSIXct(character())
      )
    )
  }

  tz_use <- "Europe/Rome"
  first_day <- as.Date(start, tz = tz_use)
  last_day <- as.Date(end - seconds(1), tz = tz_use)
  day_seq <- seq(first_day, last_day, by = "day")

  bind_rows(lapply(day_seq, function(d) {
    d_start <- as.POSIXct(d, tz = tz_use)
    d_end <- as.POSIXct(d + 1, tz = tz_use)

    tibble(
      day = d,
      start = max(start, d_start),
      end = min(end, d_end)
    )
  }))
}

# ---------------------------------------------------------------------------
# Intervalli certi derivati direttamente dall'uptime.
# Ogni SystemInfo certifica l'intervallo:
# [timestamp - uptime, timestamp].
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
    start = timestamp - seconds(uptime),
    end = timestamp,
    source = "systeminfo"
  ) |>
  filter(end > start)

# ---------------------------------------------------------------------------
# Reset SystemInfo e classificazione delle disconnessioni rilevanti.
#
# Non modifichiamo i conteggi e non ricostruiamo il tempo mancante.
# L'evento serve solo a:
# - storicizzare nel DB la disconnessione;
# - marcare i primi dati post-reset con incremento > 1 come potenzialmente
#   alterati temporalmente dalla disconnessione.
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

empty_reset_result <- function() {
  list(event = NULL, sensor_events = NULL)
}

reset_results <- lapply(seq_len(nrow(reset_candidates)), function(i) {
  reset_row <- reset_candidates[i, ]

  gateway_counts <- count_events |>
    filter(gateway_id == reset_row$gateway_id)

  sensor_ids <- gateway_counts |>
    distinct(sensor_id) |>
    pull(sensor_id)

  if (length(sensor_ids) == 0) return(empty_reset_result())

  # I conteggi possono arrivare prima del riavvio stimato dall'uptime.
  # Cerco la ripresa oltre l'ora coperta dall'ultimo SystemInfo precedente.
  count_search_start <- reset_row$previous_timestamp + hours(1)

  first_after_by_sensor <- lapply(sensor_ids, function(sid) {
    sensor_data <- gateway_counts |>
      filter(sensor_id == sid) |>
      arrange(timestamp)

    after <- sensor_data |>
      filter(timestamp > count_search_start) |>
      slice_head(n = 1)

    if (nrow(after) == 0) return(NULL)

    tibble(
      sensor_id = sid,
      first_timestamp = after$timestamp,
      first_increment = after$increment
    )
  }) |>
    bind_rows()

  # Sensori senza una coppia di count valida non partecipano.
  observable <- first_after_by_sensor |>
    filter(is.finite(first_increment))

  if (nrow(observable) == 0) return(empty_reset_result())

  # Regola concordata: solo incremento > 1 rende la disconnessione rilevante.
  evidence <- observable |>
    filter(first_increment > 1)

  if (nrow(evidence) == 0) return(empty_reset_result())

  gap_start_ts <- reset_row$previous_timestamp
  gap_end_ts <- reset_row$restart_ts
  gap_minutes <- if (
    !is.na(gap_start_ts) &&
    !is.na(gap_end_ts) &&
    gap_end_ts > gap_start_ts
  ) {
    as.numeric(difftime(gap_end_ts, gap_start_ts, units = "mins"))
  } else {
    NA_real_
  }

  first_affected_ts <- min(evidence$first_timestamp, na.rm = TRUE)
  last_affected_ts <- max(evidence$first_timestamp, na.rm = TRUE)

  event <- tibble(
    coupon = reset_row$coupon,
    gateway_id = reset_row$gateway_id,
    gateway_name = reset_row$gateway_name,
    systeminfo_a_ts = reset_row$previous_timestamp,
    systeminfo_b_ts = reset_row$timestamp,
    systeminfo_a_uptime_seconds = reset_row$previous_uptime,
    systeminfo_b_uptime_seconds = reset_row$uptime,
    restart_ts = reset_row$restart_ts,
    gap_start_ts = gap_start_ts,
    gap_end_ts = gap_end_ts,
    gap_minutes = gap_minutes,
    first_affected_ts = first_affected_ts,
    last_affected_ts = last_affected_ts,
    observed_sensors = nrow(observable),
    evidence_sensors = nrow(evidence),
    max_first_increment = max(evidence$first_increment, na.rm = TRUE),
    sensor_evidence = paste0(
      evidence$sensor_id,
      "=",
      evidence$first_increment,
      " @ ",
      format(
        evidence$first_timestamp,
        "%Y-%m-%d %H:%M:%S",
        tz = "Europe/Rome"
      ),
      collapse = "; "
    )
  )

  sensor_events <- evidence |>
    transmute(
      coupon = reset_row$coupon,
      gateway_id = reset_row$gateway_id,
      gateway_name = reset_row$gateway_name,
      systeminfo_a_ts = reset_row$previous_timestamp,
      systeminfo_b_ts = reset_row$timestamp,
      restart_ts = reset_row$restart_ts,
      gap_start_ts = gap_start_ts,
      gap_end_ts = gap_end_ts,
      gap_minutes = gap_minutes,
      sensor_id,
      affected_ts = first_timestamp,
      accumulated_increment = first_increment
    )

  list(event = event, sensor_events = sensor_events)
})

gateway_disconnect_events <- bind_rows(
  lapply(reset_results, function(x) x$event)
)

gateway_disconnect_sensor_events <- bind_rows(
  lapply(reset_results, function(x) x$sensor_events)
)

saveRDS(
  gateway_disconnect_events,
  here("02_Output", "gateway_disconnect_events.rds")
)

saveRDS(
  gateway_disconnect_sensor_events,
  here("02_Output", "gateway_disconnect_sensor_events.rds")
)

# ---------------------------------------------------------------------------
# Buffer giornaliero: un'ora dopo l'ultimo SystemInfo del giorno.
# Lo tratto come intervallo per evitare doppio conteggio.
# ---------------------------------------------------------------------------
buffer_intervals <- system_info_clean |>
  mutate(day = as.Date(timestamp, tz = "Europe/Rome")) |>
  group_by(coupon, gateway_id, day) |>
  slice_max(timestamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    day_end = as.POSIXct(day + 1, tz = "Europe/Rome"),
    buffer_end = if_else(
      timestamp + hours(1) < day_end,
      timestamp + hours(1),
      day_end
    )
  ) |>
  transmute(
    coupon,
    gateway_id,
    start = timestamp,
    end = buffer_end,
    source = "daily_buffer"
  ) |>
  filter(end > start)

# Il gap di disconnessione NON viene aggiunto all'uptime.
# Restano solo gli intervalli osservati da SystemInfo e il buffer finale.
all_intervals <- bind_rows(
  base_intervals,
  buffer_intervals
)

# ---------------------------------------------------------------------------
# Unione intervalli per gateway, poi spezzatura per giornata.
# ---------------------------------------------------------------------------
merged_intervals <- all_intervals |>
  group_by(coupon, gateway_id) |>
  group_modify(~ merge_intervals(.x |> select(start, end))) |>
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
    daily_uptime = sum(interval_hours, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    daily_uptime = pmin(pmax(daily_uptime, 0), 24)
  )

saveRDS(uptime, here("02_Output", "uptime.rds"))
