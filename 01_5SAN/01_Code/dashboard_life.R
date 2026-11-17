# Aggregazione condivisa tra ETL e fallback per output precedenti.
prepara_vita_dashboard <- function(raw_life_data) {
  raw_life_data <- raw_life_data |>
    mutate(timestamp = lubridate::with_tz(timestamp, "Europe/Rome"))
  if (!"cds_elapsed_years" %in% names(raw_life_data)) {
    raw_life_data$cds_elapsed_years <- NA_real_
  }

  raw_life_data |>
    filter(
      !is.na(coupon),
      !is.na(cds_name),
      !is.na(timestamp)
    ) |>
    arrange(timestamp) |>
    group_by(coupon, cds_name) |>
    summarise(
      cds_vds = last(cds_vds[!is.na(cds_vds)], default = NA_real_),
      cds_t10d = last(cds_t10d[!is.na(cds_t10d)], default = NA_real_),
      count = if (all(is.na(count))) NA_real_ else max(count, na.rm = TRUE),
      offset = if (all(is.na(offset))) NA_real_ else max(offset, na.rm = TRUE),
      lifetime = last(
        cds_elapsed_years[is.finite(cds_elapsed_years) & cds_elapsed_years >= 0],
        default = NA_real_
      ),
      .groups = "drop"
    ) |>
    mutate(count = count + offset) |>
    select(coupon, cds_name, cds_vds, cds_t10d, count, lifetime)
}
