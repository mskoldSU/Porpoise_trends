library(tidyverse)
source("functions.R")

##
## Data
##

daily_data <- read_csv("daily_data.csv", col_types = cols(station = col_character())) %>% 
  mutate(study = ifelse(lubridate::year(date) < 2015, "SAMBAH", "SNMP")) %>% 
  filter(remove == FALSE)

# Stations, years and season for indices
index_stations <- c("1032", "1041", "1036")
index_years <- c(2011, 2012, 2017, 2018, 2019)
index_season <- 5:10 # May-Oct



##
## Summary table of data
##

data_table <- summary_table(daily_data) %>% 
  left_join(summary_table(filter(daily_data, lubridate::month(date) %in% index_season)), 
            by = "station", 
            suffix = c("", ".summer"))


##
## Trends tables
##

responses <- c("dph", "dps", "n_clicks", "n_encounters", "n_trains")
index_data <- map(responses, ~make_indices(index_stations, index_years, index_season, response = .x))
coeff_tables <- map(index_data, coeff_table)

##
## Trends summary
##

trend_summary <- map2_df(responses, coeff_tables, ~mutate(.y, response = .x)) %>% 
  select(station, trend, response) %>% 
  pivot_wider(names_from = "response", values_from = "trend")

##
## Save tables
##

xlsx::write.xlsx(data_table, "tables.xlsx", sheetName = "data_table", append = FALSE)
walk2(paste0("trend_table_", indices), coeff_tables, ~xlsx::write.xlsx(.y, "tables.xlsx", sheetName = .x, append = TRUE))
xlsx::write.xlsx(trend_summary, "tables.xlsx", sheetName = "trend_summary", append = TRUE)



