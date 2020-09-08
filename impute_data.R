# Uses mgcv::gam to impute missing values in order to get full calendar years
library(tidyverse)
library(mgcv)

add_missing_days <- function(data){
  # Helper function, augments a dataframe with missing dates/stations
  stations <- unique(data$station)
  years <- unique(lubridate::year(data$date))
  days_of_year <- function(year){
    seq(from = as.Date(paste0(year, "-01-01")), to = as.Date(paste0(year, "-12-31")), by = 1)
  }
  dates <- as.Date(numeric(0))
  for (y in years){
    dates <- c(dates, days_of_year(y))
  }
  all_dates <- expand.grid(station = stations, date = dates)
  missing_dates <- setdiff(all_dates, select(data, date, station))
  bind_rows(data, missing_dates) %>% 
    arrange(station, date)
}

impute_data <- function(data, response_type){
  # Helper function, predicts values for dates with missing data
  years_recorded <- filter(data, !is.na(y)) %>% 
    pull(year) %>% unique()
  data <- filter(data, year %in% years_recorded)
  if (response_type %in% c("n_encounters", "dph"))
    fit <- mgcv::gam(y ~ s(julian_day, bs = "cc") + year, data = data, family = "poisson")
  if (response_type %in% c("n_clicks", "dps", "n_trains")) 
    fit <- mgcv::gam(y ~ s(julian_day, bs = "cc") + year, data = data, family=Tweedie(1.25))
  mutate(data, predicted = predict(fit, newdata = data, type = "response"),
         imputed = ifelse(is.na(y), predicted, y))
}


stations <- c("1032", "1041", "1036") # Stations to impute
imputed_data <- read_csv("daily_data.csv", 
                         col_types = cols(station = col_factor(), 
                                          date = col_date(format = "%Y-%m-%d"))) %>% 
  filter(station %in% stations, remove == FALSE) %>% 
  select(station, date, dph, dps, n_clicks, n_encounters, n_trains) %>% 
  add_missing_days() %>% 
  pivot_longer(c("dph", "dps", "n_clicks", "n_encounters", "n_trains"), names_to = "response_type", values_to = "y") %>% 
  mutate(julian_day = lubridate::yday(date),
         year = as.factor(lubridate::year(date))) %>% 
  group_by(station, response_type) %>% 
  nest() %>% 
  mutate(data = map2(data, response_type, impute_data)) %>% 
  unnest(data) %>% 
  ungroup()

write_csv(imputed_data, "imputed_data.csv")