
# setup -------------------------------------------------------------------

library(tidyverse)

cc_full <- read_csv('data/raw/fullCCDataset_2021-12-01.csv')

lsm_500 <- read_csv('data/processed/lsm_metrics_500m.csv')

pc_500 <- read_csv('data/processed/percent_cover_500m.csv')

lsm_2000 <- read_csv('data/processed/lsm_metrics_2000m.csv')

pc_2000 <- read_csv('data/processed/percent_cover_2000m.csv')

# make a character mode function

charMode <- function(x){
  uniques <- unique(x)
  uniques[which.max(tabulate(match(x,uniques)))]
}

# creating dataframes for analysis ----------------------------------------

abundance_frame <- cc_full %>% 
  filter(
    Year %in% 2018:2021,
    ObservationMethod == 'Beat sheet',
    Length < 150) %>% 
  mutate(
    solstice_jday = if_else(
      Year %in% c(2018,2019),
      true = 172,
      false = 171)) %>% 
  filter(abs(solstice_jday - julianday) <= 27) %>% 
  mutate(arthID = if_else(is.na(arthID), 0, arthID)) %>%
  group_by(SiteFK, ID) %>% 
  summarize(
    total_arths = sum(Quantity),
    total_biomass = sum(Biomass_mg)) %>% 
  group_by(SiteFK) %>% 
  summarize(
    mean_arths = mean(total_arths),
    mean_biomass = mean(total_biomass)) %>% 
  left_join(
    cc_full %>% 
      filter(
        Year %in% 2018:2021,
        ObservationMethod == 'Beat sheet') %>%
      group_by(SiteFK) %>% 
      summarize(
        dominant_tree = charMode(Species)),
    by = 'SiteFK') %>% 
  left_join(
    cc_full %>% 
      select(SiteFK, Latitude, Longitude) %>% 
      distinct(),
    by = 'SiteFK') %>% 
  left_join(
    lsm_500 %>% 
      rename_with(
        ~ str_c(., '_500m'),
        .cols = 3:6) %>% 
      left_join(
        lsm_2000 %>% 
          rename_with(
            ~ str_c(., '_2000m'),
            .cols = 3:6),
        by = c('siteID', 'Name')) %>% 
      left_join(
        pc_500 %>% 
          rowwise() %>% 
          mutate(
            devo_lo = devo_open + devo_low,
            devo_hi = devo_med + devo_high,
            forest_total = forest_decid + forest_everg + forest_mix) %>% 
          select(siteID, devo_lo, devo_hi, forest_total) %>% 
          rename_with(
            ~ str_c(., '_500m'),
            .cols = 2:4),
        by = 'siteID') %>% 
      left_join(
        pc_2000 %>% 
          rowwise() %>% 
          mutate(
            devo_lo = devo_open + devo_low,
            devo_hi = devo_med + devo_high,
            forest_total = forest_decid + forest_everg + forest_mix) %>% 
          select(siteID, devo_lo, devo_hi, forest_total) %>% 
          rename_with(
            ~ str_c(., '_2000m'),
            .cols = 2:4),
        by = 'siteID'),
    by = c('SiteFK' = 'siteID'))

abundance_frame %>% 
  ggplot() +
  geom_point(aes(
    x = log(area_mn_500m),
    y = mean_biomass))

abundance_frame %>% 
  ggplot() +
  geom_point(aes(
    x = log(area_mn_500m),
    y = mean_arths))

abundance_frame %>% 
  ggplot() +
  geom_point(aes(
    x = log(area_mn_2000m),
    y = mean_biomass))

abundance_frame %>% 
  ggplot() +
  geom_point(aes(
    x = log(area_mn_2000m),
    y = mean_arths))
