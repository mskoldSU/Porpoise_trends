library(tidyverse)
source("functions.R")

##
## Data
##

daily_data <- read_csv("daily_data.csv", col_types = cols(station = col_character())) %>% 
  mutate(study = factor(ifelse(lubridate::year(date) < 2015, "SAMBAH", "SNMP"))) %>% 
  filter(remove == FALSE)

# Stations, years and season for indices
index_stations <- c("1032", "1041", "1036")
index_years <- c(2011, 2012, 2017, 2018, 2019)
index_season <- 5:10 # May-Oct

# Station cooordinates for map
station_cords <- tibble(station = c("1014", "1019", "1020", "1021", "1024", 
                                "1025", "1026", "1029", "1031", "1032", "1036", "1041"), 
                    lat = c(55.814, 55.982384, 55.873339, 55.763451, 56.0542, 55.977303, 55.832534, 
                            55.866258, 56.123757, 56.012008, 56.078047, 56.256312), 
                    long = c(15.219833, 15.479153, 15.80197, 16.122975, 15.998025, 16.319291, 16.639634, 
                             16.898637, 16.517921, 16.83893, 17.36011, 17.564027))

##
## Map figs
##
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)

map_polygons <- ne_countries(scale = "large", returnclass = "sf") %>% 
  st_crop(c(xmin=7, xmax=26, ymin=53, ymax=61))

inset_map <- ggplot(ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf() +
  coord_sf(xlim = c(8, 25), ylim = c(54, 60), expand = FALSE) +
  annotate("rect", xmin = 15, xmax = 17.75, ymin = 55.5, ymax = 56.5, color = "red", fill = NA) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0, "mm"))

# Basic map
map_plot <- ggplot(map_polygons) + geom_sf() +    
  coord_sf(xlim = c(15, 17.75), ylim = c(55.5, 56.5), expand = FALSE) +
  geom_point(data = station_cords, aes(x = long, y = lat), color = "black") +
  theme_bw() + theme(panel.grid = element_blank()) +
  xlab("") + ylab("") +
  geom_text(data = station_cords, aes(x = long, y = lat+.05, label = station)) +
  annotation_north_arrow(location = "tl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotation_scale()

map_basic <- cowplot::ggdraw() +
  cowplot::draw_plot(map_plot) +
  cowplot::draw_plot(inset, x = 0.73, y = 0.17, width = 0.25, height = 0.25)

# DPH by study
map_data <- daily_data %>% 
  group_by(station, study) %>% 
  summarise(dph = mean(dph)) %>% 
  left_join(positions)

map_dph <- ggplot(map_polygons) + geom_sf() +    
  coord_sf(xlim = c(15, 17.75), ylim = c(55.5, 56.5), expand = FALSE) +
  geom_point(data = map_data, aes(x = long, y = lat, size = dph), color = "black") +
  theme_bw() + theme(panel.grid = element_blank()) +
  xlab("") + ylab("") +
  geom_text(data = station_cords, aes(x = long, y = lat+.05, label = station)) +
  annotation_north_arrow(location = "tl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotation_scale() +
  facet_wrap(~study, ncol = 1) +
  theme(legend.position = "none")


##
## Seasonality figs
##

fit_common <- daily_data %>% 
  filter(station %in% index_stations) %>% 
  mutate(julian_day = lubridate::yday(date),
         year = as.factor(lubridate::year(date))) %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(fit = map(data, ~mgcv::gam(dph ~ s(julian_day, bs = "cc") + year, data = .x, family =poisson())),
         season = map(fit, ~tibble(predicted = predict(.x, newdata = tibble(julian_day = 1:365, year = 2013, study = "SAMBAH"), 
                                                       type = "response"), 
                                   julian_day = 1:365)
         )
  ) %>% 
  select(station, season) %>% 
  unnest(season) %>% 
  group_by(station) %>% 
  mutate(predicted = predicted / sum(predicted), study = "Both") %>% 
  ungroup()
fit_study <- daily_data %>% 
  filter(station %in% index_stations) %>% 
  mutate(julian_day = lubridate::yday(date),
         year = as.factor(lubridate::year(date))) %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(fit = map(data, ~mgcv::gam(dph ~ s(julian_day, bs = "cc", by = study) + year, data = .x, family =poisson())),
         season = map(fit, ~tibble(predicted = predict(.x, newdata = 
                                                         bind_rows(
                                                           tibble(julian_day = 1:365, year = 2013, study = "SAMBAH"),
                                                           tibble(julian_day = 1:365, year = 2017, study = "SNMP")), 
                                                       type = "response"),
                                   julian_day = rep(1:365, 2),
                                   study = c(rep("SAMBAH", 365), rep("SNMP", 365)))
         )
  ) %>% 
  select(station, season) %>% 
  unnest(season) %>% 
  group_by(station, study) %>% 
  mutate(predicted = predicted / sum(predicted)) %>% 
  ungroup()

month_numbers <- str_pad(1:12, width = 2, side = "left", pad = "0")
xaxis_breaks <-  c(lubridate::yday(as.Date(paste0("2000-", month_numbers, "-15"))))
month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

season_fig <- ggplot() + geom_line(data = fit_common, aes(x = julian_day, y = predicted), size = 1, color = "steelblue") + 
  geom_line(data = fit_study, aes(x = julian_day, y = predicted, linetype = study), color = "steelblue") + 
  facet_wrap(~station) + theme_bw() + scale_linetype_manual(values = c(2, 3)) + 
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.ticks = element_blank()) + 
  scale_y_continuous(breaks = 0, expand = c(0, 0), limits = c(0, NA)) + xlab("") + ylab("") +
  scale_x_continuous(breaks = xaxis_breaks, labels = month_labels)

##
## Index trend figs
##

index_data <- make_indices(index_stations, index_years, index_season, response = "dph")

trend_fig <- index_data %>% ggplot(aes(x = year, y = index)) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = "log")), color = "steelblue") + 
  geom_point() + facet_wrap(~station, scales = "free_y")+ theme_bw() + 
  scale_x_continuous(breaks = seq(2012, 2019, by = 2)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  xlab("") + ylab("Mean daily DPH")


##
## Data fig
##


data_fig <- daily_data %>% ggplot(aes(x = date, y = dph)) + 
  geom_point(size = .7, stroke = 0, alpha = .5)  + theme_bw() +
  scale_x_date(breaks = as.Date(paste0(2010:2020, "-07-01")), 
               minor_breaks = as.Date(paste0(2010:2020, "-01-01")), date_labels = "%Y") + 
  theme_bw() +
  theme(panel.spacing = unit(0.1, "lines"), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank()) + 
  xlab("") + ylab("Daily DPH") + 
  facet_grid(station~study, scales = "free_x", space = "free_x")


##
## Save figs
##

ggsave(map_basic, filename = "figs/map_basic.pdf", width = 7, height = 6)
ggsave(map_dph, filename = "figs/map_dph.pdf", width = 7, height = 10)
ggsave(season_fig, filename = "figs/season_fig.pdf", width = 7, height = 3)
ggsave(trend_fig, filename = "figs/trend_fig.pdf", width = 7, height = 6)
ggsave(data_fig, filename = "figs/data_fig.pdf", width = 7, height = 6)
