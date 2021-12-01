
# setup -------------------------------------------------------------------

library(tidyverse)

library(sf)

library(ggplot2)

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

lsm_3000m <- read_csv('data/processed/lsm_metrics_3000m.csv')

pc_3000m <- read_csv('data/processed/percent_cover_3000m.csv')


# lsm metrics distributions ------------------------------------------------------

# distribution of mean forest patch area in square meters

ggplot(lsm_3000m) +
  geom_histogram(aes(x = area_mn))

summary(lsm_3000m$area_mn)

# distribution of mean forest patch shape index (increases unbounded from 1 with complexity)

ggplot(lsm_3000m) +
  geom_histogram(aes(x = shape_mn))

summary(lsm_3000m$shape_mn)

# distribution of mean forest patch contiguity index (increases from 0 to 1 with greater connectivity)

ggplot(lsm_3000m) +
  geom_histogram(aes(x = contig_mn))

summary(lsm_3000m$contig_mn)

# distribution of Euclidean nearest neighbor distance (shortest straight-line distance to a neighbor in meters - NA if only one patch)

ggplot(lsm_3000m) +
  geom_histogram(aes(x = enn_mn))

summary(lsm_3000m$enn_mn)


# percent cover distributions ---------------------------------------------

# generate tibble with sums

pc_3000m_sums <- pc_3000m %>% 
  rowwise() %>% 
  mutate(
    devo_total = devo_open + devo_low + devo_med + devo_high,
    forest_total = forest_decid + forest_everg + forest_mix)

# distribution of total developed cover

pc_3000m_sums %>%
  ggplot() +
  geom_histogram(aes(x = devo_total))

summary(pc_3000m_sums$devo_total)

# distribution of total forest cover

pc_3000m_sums %>% 
  ggplot() +
  geom_histogram(aes(x = forest_total))

summary(pc_3000m_sums$forest_total)


# maps --------------------------------------------------------------------

sf_stats <- sites_sf %>% 
  left_join(lsm_3000m, by = c('ID' = 'siteID', 'Name')) %>%
  left_join(pc_3000m_sums, by = c('ID' = 'siteID', 'Name'))

ggplot(us_map) +
  geom_sf() +
  geom_sf(
    data = sf_stats,
    mapping = aes(
      color = devo_total))
