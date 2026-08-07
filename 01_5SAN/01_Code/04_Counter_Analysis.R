library(dplyr)
library(lubridate)
library(here)

# carica i dati 
raw_data <- readRDS(here("02_Output", "raw_data.rds")) |>
  mutate(field = if_else(is.na(field) | trimws(field) == "", "(Non specificato)", field))
uptime <- readRDS(here("02_Output", "uptime.rds"))

# calcola gli incrementi dei conteggi per ogni sensore e li classifico
sensor_count_increment <- raw_data |>
  group_by(company, field, project, coupon, machine_name, gateway_name, cds_name, cds_description, 
           cds_brand, cds_use, cds_vds, sensor_description) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(count_type = case_when(row_number() == 1 ~ "first_count",
                                row_number() == 2 ~ "second_count",
                                TRUE ~ "not_first"),
         previous_count = if_else(row_number() == 1, count, lag(count)),
         increment = case_when(count_type == "first_count" ~ 0, 
                               count_type == "second_count" & previous_count == 0 ~ count - count, 
                               TRUE ~ count - previous_count),
         increment_type = case_when(increment > 0 ~ "increment",
                                    increment == 0 ~ "stable",
                                    increment < 0 ~ "decrease"),
         hour =  floor_date(timestamp, "hour"),
         day = as.Date(timestamp)) |>
  # unisce l'uptime gionaliero
  left_join(uptime, by = c("coupon", "day")) |>
  ungroup()

# cds_colors <- sensor_count_increment |>
#   distinct(cds_name) |>
#   arrange(cds_name) |>
#   mutate(cds_color = hcl.colors(n = n(), palette = "Dark 3"))
# 
# # calcolo la somma oraria per ogni sensore
# sensor_hour_average <- sensor_count_increment |>
#   filter(increment_type != "decrease") |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, cds_name, cds_description, 
#            cds_brand, cds_use, cds_vds, sensor_description, hour) |>
#   summarise(increment_avg = sum(increment),
#             .groups = "drop_last") |>
#   left_join(cds_colors, by = "cds_name")
#   
# # calcolo la somma oraria per ogni macchina
# machine_hour_average <- sensor_count_increment |>
#   filter(increment_type != "decrease") |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, hour) |>
#   summarise(increment_avg = sum(increment),
#             .groups = "drop_last")  
# 
# # calcolo la media giornaliera per ogni sensore
# sensor_day_average <- sensor_count_increment |>
#   mutate(day =  floor_date(hour, "day")) |>
#   filter(increment_type != "decrease") |>
#   left_join(uptime, by = c("coupon", "day")) |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, cds_name, cds_description, 
#            cds_brand, cds_use, cds_vds, sensor_description, day, daily_uptime) |>
#   summarise(
#     increment = sum(increment),
#     increment_avg = round(increment/unique(daily_uptime),3),
#     .groups = "drop_last")
# 
# # calcolo la media giornaliera per ogni macchina
# machine_day_average <- sensor_count_increment |>
#   mutate(day =  floor_date(hour, "day")) |>
#   filter(increment_type != "decrease") |>
#   left_join(uptime, by = c("coupon", "day")) |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, day, daily_uptime) |>
#   summarise(increment_avg = round(sum(increment)/unique(daily_uptime),3),
#             .groups = "drop_last") 
# 
# # calcolo la media settimanale per ogni sensore
# sensor_week_average <- sensor_count_increment |>
#   mutate(week =  floor_date(hour, "week")) |>
#   filter(increment_type != "decrease") |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, cds_name, cds_description, 
#            cds_brand, cds_use, cds_vds, sensor_description, week) |>
#   summarise(increment_avg = round(mean(increment),3),
#             .groups = "drop_last")
# 
# # calcolo la media settimanale per ogni macchina
# machine_week_average <- sensor_count_increment |>
#   mutate(week =  floor_date(hour, "week")) |>
#   filter(increment_type != "decrease") |>
#   group_by(company, field, project, coupon, machine_name, gateway_name, week) |>
#   summarise(increment_avg = round(mean(increment),3),
#             .groups = "drop_last") 
# 
# salvo i dati in formato R
saveRDS(sensor_count_increment, here("02_Output", "sensor_count_increment.rds"))
# saveRDS(sensor_hour_average, here("02_Output", "sensor_hour_average.rds"))
# # e in xlsx
# write.xlsx(sensor_hour_average, here("02_Output", "Attivazioni_Sensori.xlsx"))
# write.xlsx(sensor_day_average, here("02_Output", "Attivazioni_Sensori_giorno.xlsx"))
# write.xlsx(sensor_count_increment, here("02_Output", "Conteggio_Attivazioni_Sensori.xlsx"))
