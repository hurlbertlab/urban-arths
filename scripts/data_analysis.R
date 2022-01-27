
# setup -------------------------------------------------------------------

library(tidyverse)

cc_full <- read_csv('data/raw/fullCCDataset_2021-12-01.csv')

lsm_500 <- read_csv('data/processed/lsm_metrics_500m.csv')

pc_500 <- read_csv('data/processed/percent_cover_500m.csv')

lsm_2000 <- read_csv('data/processed/lsm_metrics_2000m.csv')

pc_2000 <- read_csv('data/processed/percent_cover_2000m.csv')

lsm_5000 <- read_csv('data/processed/lsm_metrics_5000m.csv')

pc_5000 <- read_csv('data/processed/percent_cover_5000m.csv')

lsm_3000 <- read_csv('data/processed/lsm_metrics_3000m.csv')

pc_3000 <- read_csv('data/processed/percent_cover_3000m.csv')

cc_plants <- read_csv('data/raw/2021-11-18_Plant.csv')


# data preparation --------------------------------------------------------

# make a character mode function

charMode <- function(x){
  uniques <- unique(x)
  uniques[which.max(tabulate(match(x,uniques)))]
}

# make a dominant tree species dataframe

dominant_species <- cc_plants %>% 
  group_by(SiteFK) %>% 
  summarize(dom_spp = charMode(Species))

# function to relabel lsm dataframes

lsm_rename <- function(frame, buffer){
  frame %>% 
    rename_with(
      ~ str_c(., buffer),
      .cols = 3:6)
}

# combine all lsm measurements and scales into single dataframe

lsm <- map2(
  list(lsm_500,lsm_2000,lsm_3000,lsm_5000),
  list('_500m','_2000m','_3000m','_5000m'),
  ~ lsm_rename(frame = .x, buffer = .y) %>%
    select(-Name) %>% 
    right_join(lsm_5000 %>% select(siteID))) %>% 
  bind_cols() %>% 
  select(-(starts_with('siteID') & !ends_with('...1'))) %>%
  rename(siteID = siteID...1)

# function to process and rename pc dataframes

pc_process <- function(frame, buffer){
  frame %>% 
    rowwise() %>% 
    mutate(forest_total = forest_decid + forest_everg + forest_mix) %>% 
    select(siteID, forest_total) %>% 
    rename_with(
      ~ str_c(., buffer),
      .cols = 2)
}

pc <- map2(
  list(pc_500,pc_2000,pc_3000,pc_5000),
  list('_500m','_2000m','_3000m','_5000m'),
  ~ pc_process(frame = .x, buffer = .y) %>% 
    select(-siteID)) %>% 
  bind_cols() %>% 
  cbind(siteID = pc_500$siteID)

# creating dataframes for analysis

cc_full %>% 
  filter(Year %in% 2018:2021) %>% nrow()

cc_full %>% 
  filter(
    Year %in% 2018:2021,
    ObservationMethod %in% c('Beat sheet', 'Visual')) %>% nrow()

cc_full %>% 
  filter(
    Year %in% 2018:2021,
    ObservationMethod %in% 'Beat sheet') %>% nrow()

cc_full %>% 
  filter(
    Year %in% 2018:2021,
    ObservationMethod %in% 'Visual') %>% nrow()

abundance_frames <- map(
  list('Beat sheet', 'Visual', c('Beat sheet', 'Visual')),
  function(x){
    cc_full %>% 
      filter(
        Year %in% 2018:2021,
        ObservationMethod %in% x) %>% 
      mutate(
        solstice_jday = if_else(
          Year %in% c(2018,2019),
          true = 172,
          false = 171)) %>% 
      filter(abs(solstice_jday - julianday) <= 14) %>% 
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
        dominant_species,
        by = 'SiteFK') %>% 
      left_join(
        cc_full %>% 
          select(SiteFK, Latitude, Longitude) %>% 
          distinct(),
        by = 'SiteFK') %>% 
      left_join(
        pc,
        by = c('SiteFK' = 'siteID')) %>% 
      left_join(
        lsm,
        by = c('SiteFK' = 'siteID'))
  }) %>% 
  set_names(c('visuals_frame', 'beats_frame', 'full_frame'))
