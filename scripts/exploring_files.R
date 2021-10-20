library(tidyverse)

library(ggplot2)

library(sf)

sites <- read_csv('data/raw/2021-06-22_Site.csv') %>% 
  filter(
    Name != 'Example Site',
    Latitude < 50)

us_map <- st_read('data/geographic/states.shp')

sites_sf <- 
  sites %>% 
  st_as_sf(
    coords = c('Longitude', 'Latitude'),
    crs = st_crs(us_map))

ggplot(us_map) +
  geom_sf() +
  geom_sf(data = sites_sf) +
  theme_void()
