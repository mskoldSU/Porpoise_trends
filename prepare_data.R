library(tidyverse)
# Path of file exported as "Detections and environment" by CPOD.exe
detenv_path <- "CPOD/Harbour Porpoise DetEnv 2020-09-07.txt"
# Path of file exported as "Click Details!" by CPOD.exe. Since
# file names are abbreviated to first 6 characters, these must
# be unique if used on batch of files. See rename_files.R
click_path <- "CPOD/Click Details 2020-09-07.txt"

file_contents <- read_lines(detenv_path)
# Lines where pod is OFF have inconsistent column format, remove them
file_contents <- file_contents[!str_detect(file_contents, "0m ON")] 
data_full <- read_delim(file_contents, 
                        delim = "\t", 
                        col_types = cols(ChunkEnd = col_datetime(format = "%d/%m/%Y %H:%M")), 
                        locale = locale(decimal_mark = ","), 
                        trim_ws = TRUE) %>% 
  janitor::clean_names() %>% 
  mutate(station = str_sub(file, 1, 4), 
         date = format(chunk_end, "%Y-%m-%d"),
         file_id = str_sub(file, 1, 6),
         pod = str_extract(file, "POD.{0,4}"))
rm(file_contents) 

# Dates/stations without/with detections are needed for padding
all_days <- data_full %>% 
  select(date, station, file_id) %>% 
  distinct()
detection_days <- data_full %>% 
  filter(dpm > 0) %>% 
  select(date, station, file_id) %>% 
  distinct()
zero_days <- setdiff(all_days, detection_days)

# Count DPH
daily_dph <- data_full %>% 
  filter(dpm == 1) %>% 
  group_by(station, date, file_id) %>% 
  summarise(dph = n_distinct(lubridate::hour(chunk_end))) %>% 
  ungroup() %>% 
  bind_rows(mutate(zero_days, dph = 0))

# Get daily DPS and trains from Click file
daily_dps <- read_delim(click_path, delim = "\t",
                        col_types = cols(Minute = col_datetime(format = "%d/%m/%Y %H:%M"))) %>% 
  janitor::clean_names() %>% 
  mutate(file_id = as.character(abbreviated_file_name),
         station = str_sub(file_id, 1, 4),
         date = format(minute, "%Y-%m-%d"),
         second = lubridate::hour(minute) * 60 * 60 + lubridate::minute(minute)* 60 + floor(microsec / 1000000)) %>% 
  group_by(station, date, file_id) %>% 
  summarise(dps = n_distinct(second), 
            n_trains = n_distinct(tr_n)) %>% 
  ungroup() %>% 
  bind_rows( # Pad with zero days
    mutate(zero_days, dps = 0, n_trains = 0)
  ) %>% 
  arrange(station, date)

# Count daily encounters by clustering
k <- 11 # Cutoff for clustering
daily_encounters <- filter(data_full, dpm == 1) %>% 
  mutate(minutes_since_start = as.numeric(julian(chunk_end)*60*24),
         minutes_since_start = minutes_since_start - min(minutes_since_start)) %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate(.x, visit = dist(.x$minutes_since_start) %>% 
                                    hclust(method = "single") %>% 
                                    cutree(h = k)))
  ) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  mutate(visit = paste(station, visit, sep = "_")) %>% 
  group_by(station, visit, file_id) %>% 
  summarise(date = min(date)) %>% 
  group_by(station, date, file_id) %>% 
  summarise(n_encounters = n()) %>% 
  ungroup() %>% 
  right_join(detection_days, by = c("date", "station", "file_id")) %>% 
  mutate(n_encounters = ifelse(is.na(n_encounters), 0, n_encounters)) %>% 
  bind_rows(mutate(zero_days, n_encounters = 0))


# Add everything up and save
daily_data <- data_full %>% 
  group_by(station, date, file_id, file, pod) %>% 
  summarise(minutes_on = n(),
            min_temp = min(temp), 
            max_temp = max(temp),
            minutes_lying = sum((angle>70) & (angle < 100)),
            n_clicks = sum(nfiltered_m)) %>% 
  ungroup() %>% 
  left_join(daily_dph, by = c("date", "station", "file_id")) %>% 
  left_join(daily_encounters, by = c("date", "station", "file_id")) %>% 
  left_join(daily_dps, by = c("date", "station", "file_id")) %>% 
  arrange(station, date) %>% 
  mutate(remove = (minutes_on < 1439) | (minutes_lying > 100) | (max_temp - min_temp > 10)) # Flag suspicious days 

write_csv(daily_data, "daily_data.csv")
