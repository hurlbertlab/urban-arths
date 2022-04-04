
# setup -------------------------------------------------------------------

library(tidyverse)

lsm_500a <- read_csv('data/processed/lsm_metrics_500m.csv') %>% 
  select(!Name)

lsm_2000a <- read_csv('data/processed/lsm_metrics_2000m.csv') %>% 
  select(!Name)

lsm_1000a <- read_csv('data/processed/lsm_metrics_1000m.csv') %>% 
  select(!Name)

lsm_5000a <- read_csv('data/processed/lsm_metrics_5000m.csv') %>% 
  select(!Name)

lsm_3000a <- read_csv('data/processed/lsm_metrics_3000m.csv') %>% 
  select(!Name)

lsm_1000b <- read_csv('data/processed/lsm_metrics_1000m_2.csv') %>% 
  select(!Name)

lsm_20000a <- read_csv('data/processed/lsm_metrics_20000m.csv') %>% 
  select(!Name)

lsm_500b <- read_csv('data/processed/lsm_metrics_500m_2.csv') %>% 
  select(!Name)

lsm_2000b <- read_csv('data/processed/lsm_metrics_2000m_2.csv') %>% 
  select(!Name)

lsm_5000b <- read_csv('data/processed/lsm_metrics_5000m_2.csv') %>% 
  select(!Name)

lsm_3000b <- read_csv('data/processed/lsm_metrics_3000m_2.csv') %>% 
  select(!Name)

lsm_20000b <- read_csv('data/processed/lsm_metrics_20000m_2.csv') %>% 
  select(!Name)


# joining -----------------------------------------------------------------

lsm_500a %>% 
  full_join(lsm_500b, by = 'siteID') %>% 
  write_csv('data/processed/lsm_500m_full.csv')

lsm_1000a %>% 
  full_join(lsm_1000b, by = 'siteID') %>% 
  write_csv('data/processed/lsm_1km_full.csv')

lsm_2000a %>% 
  full_join(lsm_2000b, by = 'siteID') %>% 
  write_csv('data/processed/lsm_2km_full.csv')

lsm_2000a %>% 
  full_join(lsm_2000b, by = 'siteID') %>% 
  write_csv('data/processed/lsm_2km_full.csv')

lsm_3000a %>% 
  full_join(lsm_3000b, by = 'siteID') %>% 
  write_csv('data/processed/lsm_3km_full.csv')

lsm_5000a %>% 
  full_join(lsm_5000b, by = 'siteID') %>% 
  write_csv('data/processed/lsm_5km_full.csv')

lsm_10000a %>% 
  full_join(lsm_10000b, by = 'siteID') %>% 
  write_csv('data/processed/lsm_10km_full.csv')
