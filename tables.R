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
## Trends table
##

index_data <- make_indices(index_stations, index_years, index_season, response = "dph")
trend_table_dph <- coeff_table(index_data)


##
## Save tables
##

xlsx::write.xlsx(data_table, "tables.xlsx", sheetName = "data_table", append = FALSE)
xlsx::write.xlsx(trend_table, "tables.xlsx", sheetName = "trend_table_dph", append = TRUE)


