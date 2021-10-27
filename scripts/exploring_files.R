# next steps: 
# cat analysis public (in the middle) cat sites land cover script for buffer files will provide info on buffers to use for all cc sites
# work with NLCD on cluster
# make a dataset of impervious cover for sites and save - make a clean script for all of this

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

impervious <- raster::raster('data/geographic/nlcd_2019_impervious_l48_20210604/nlcd_2019_impervious_l48_20210604.img')

sites_sf <- 
  sites %>% 
  st_as_sf(
    coords = c('Longitude', 'Latitude'),
    crs = st_crs(us_map))

cc_buffs <- st_read('data/geographic/catcount_site_buffers.shp') %>% 
  st_transform(crs = st_crs(us_map))

ggplot(us_map) +
  geom_sf() +
  geom_sf(data = sites_sf) +
  geom_sf(data = cc_buffs) +
  theme_void()

ggplot(us_map) +
  geom_sf() +
  geom_sf(data = st_cast(cc_buffs, to = 'POINT')) +
  theme_void()

cc_buff_points <- st_cast(cc_buffs, to = 'POINT')

cc_buff_states <- st_intersection(us_map, cc_buffs) %>% pull(NAME) %>% unique()

cc_states_sf <- us_map %>% 
  filter(NAME %in% cc_buff_states) %>% 
  st_transform(crs = raster::crs(impervious))


# nc impervious file is cropped not masked

raster::writeRaster(impervious, 'nlcd_impervious_2019.tif')

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
