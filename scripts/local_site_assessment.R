
# setup -------------------------------------------------------------------

library(tidyverse)

cc_sites <- read_csv('data/raw/2021-11-18_Site.csv')

lsm_3000 <- read_csv('data/processed/lsm_metrics_3000m.csv')

pc_3000 <- read_csv('data/processed/percent_cover_3000m.csv')


# local site table -----------------------------------------------------------

local_sites <- cc_sites %>% 
  select(ID, Name, Latitude, Longitude, URL, OpenToPublic) %>% 
  filter(
    Longitude <= -78.5,
    Longitude >= -79.3,
    Latitude <= 36.2,
    Latitude >= 35.6) %>% 
  left_join(
    lsm_3000 %>% 
      select(-Name),
    by = c('ID' = 'siteID')) %>% 
  left_join(
    pc_3000 %>% 
      mutate(
        devo_total = devo_open + devo_low + devo_med + devo_high,
        forest_total = forest_mix + forest_everg + forest_decid) %>% 
      select(siteID, devo_total, forest_total),
    by = c('ID' = 'siteID'))

#write_csv(local_sites, 'data/processed/local_site_landscapes_3000m.csv')


# compare summary statistics ----------------------------------------------

all_sites <- cc_sites %>% 
  select(ID, Name, Latitude, Longitude, URL, OpenToPublic) %>% 
  left_join(
    lsm_3000 %>% 
      select(-Name),
    by = c('ID' = 'siteID')) %>% 
  left_join(
    pc_3000 %>% 
      mutate(
        devo_total = devo_open + devo_low + devo_med + devo_high,
        forest_total = forest_mix + forest_everg + forest_decid) %>% 
      select(siteID, devo_total, forest_total),
    by = c('ID' = 'siteID'))

mean_comparisons <- all_sites %>% 
  summarize(across(.cols = 7:12, .fns = ~ mean(.x, na.rm = T))) %>% 
  mutate(sites = 'all', .before = area_mn) %>% 
  bind_rows(
    local_sites %>% 
      summarize(across(.cols = 7:12, .fns = ~ mean(.x, na.rm = T))) %>% 
      mutate(sites = 'local', .before = area_mn)) %>% 
  bind_rows(
    local_sites %>% 
      filter(ID %in% c(60,77,78)) %>% 
      summarize(across(.cols = 7:12, .fns = ~ mean(.x, na.rm = T))) %>% 
      mutate(sites = 'ours', .before = area_mn))

min_comparisons <- all_sites %>% 
  summarize(across(.cols = 7:12, .fns = ~ min(.x, na.rm = T))) %>% 
  mutate(sites = 'all', .before = area_mn) %>% 
  bind_rows(
    local_sites %>% 
      summarize(across(.cols = 7:12, .fns = ~ min(.x, na.rm = T))) %>% 
      mutate(sites = 'local', .before = area_mn)) %>% 
  bind_rows(
    local_sites %>% 
      filter(ID %in% c(60,77,78)) %>% 
      summarize(across(.cols = 7:12, .fns = ~ min(.x, na.rm = T))) %>% 
      mutate(sites = 'ours', .before = area_mn))

max_comparisons <- all_sites %>% 
  summarize(across(.cols = 7:12, .fns = ~ max(.x, na.rm = T))) %>% 
  mutate(sites = 'all', .before = area_mn) %>% 
  bind_rows(
    local_sites %>% 
      summarize(across(.cols = 7:12, .fns = ~ max(.x, na.rm = T))) %>% 
      mutate(sites = 'local', .before = area_mn)) %>% 
  bind_rows(
    local_sites %>% 
      filter(ID %in% c(60,77,78)) %>% 
      summarize(across(.cols = 7:12, .fns = ~ max(.x, na.rm = T))) %>% 
      mutate(sites = 'ours', .before = area_mn))
