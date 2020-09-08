# When exporting "Click Details!" using CPOD.exe, file names are
# abbreviated to their first 6 characters. This replaces the first
# 4 characters XXXX of file names (assumed to be station code) with 
# XXXXYY, YY = 01, 02, 03, ... to ensure unique abbreviated names.


library(tidyverse)
CP3_path <- "CPOD/" # Directory of .CP3-files to be renamed

files <- tibble(old_name = list.files(CP3_path, full.names = TRUE)) %>%
  filter(str_detect(old_name, ".CP3")) %>%
  mutate(station = str_sub(old_name, 7, 10)) %>%
  group_by(station) %>%
  mutate(n = 1:n() %>% str_pad(2, "left", "0"),
         pattern = paste0("/", station),
         replacement = paste0("/", station, n),
         new_name = str_replace(old_name, pattern, replacement))
walk2(files$old_name, files$new_name, ~file.rename(.x, .y))