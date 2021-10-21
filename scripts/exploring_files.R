# next steps: 
# fix the fkn raster
# make sure the CC buffers I pulled are actually good to use

library(tidyverse)

library(ggplot2)

library(sf)

library(rgdal)

sites <- read_csv('data/raw/2021-06-22_Site.csv') %>% 
  filter(
    Name != 'Example Site',
    Latitude < 50)

us_map <- st_read('data/geographic/states.shp')

nc <- us_map %>% 
  filter(NAME == 'North Carolina')

sites_sf <- 
  sites %>% 
  st_as_sf(
    coords = c('Longitude', 'Latitude'),
    crs = st_crs(us_map))

ggplot(us_map) +
  geom_sf() +
  geom_sf(data = sites_sf) +
  theme_void()

impervious <- raster::raster('data/geographic/nlcd_2019_impervious.img')

raster::writeRaster(impervious, 'ncld_impervious_2019.tif')

raster::extract(
  impervious,
  sites_sf %>% st_transform(crs = raster::crs(impervious)))

cc_buffs <- st_read('data/geographic/catcount_site_buffers.shp') %>% 
  st_transform(crs = st_crs(us_map))

ggplot(nc) +
  geom_sf() +
  geom_sf(data = st_crop(cc_buffs, nc))

mean_impervious <- 
  raster::extract(
  impervious,
  cc_buffs %>% st_transform(crs = raster::crs(impervious)),
  fun = mean,
  na.rm = T,
  sp = T) %>% 
  as_tibble()
