
# setup -------------------------------------------------------------------

library(tidyverse)

library(lme4)

cc_full <- read_csv('data/processed/cleaned_cc_2022-04-06.csv', ) %>% 
  filter(
    Name != 'Example Site',
    !Region %in% c('ON','AB','AK'))

sites <- read_csv('data/raw/2021-11-18_Site.csv') %>% 
  filter(
    Name != 'Example Site',
    !Region %in% c('ON','AB','AK'))

cc_plants <- read_csv('data/raw/2021-11-18_Plant.csv') %>% 
  filter(!SiteFK %in% c(2,100,106,107,161,205,225,258,273,277,278))

lsm_500m <- read_csv('data/processed/lsm_500m.csv')

lsm_1km <- read_csv('data/processed/lsm_1km.csv')

lsm_2km <- read_csv('data/processed/lsm_2km.csv')

lsm_3km <- read_csv('data/processed/lsm_3km.csv')

lsm_5km <- read_csv('data/processed/lsm_5km.csv')

lsm_10km <- read_csv('data/processed/lsm_10km.csv')

pc_500m <- read_csv('data/processed/percent_cover_500m.csv')

pc_1km <- read_csv('data/processed/percent_cover_1km.csv')

pc_2km <- read_csv('data/processed/percent_cover_2km.csv')

pc_3km <- read_csv('data/processed/percent_cover_3km.csv')

pc_5km <- read_csv('data/processed/percent_cover_5km.csv')

pc_10km <- read_csv('data/processed/percent_cover_10km.csv')

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
      .cols = 2:9)
}

# combine all lsm measurements and scales into single dataframe

lsm <- map2(
  list(lsm_500m,lsm_1km,lsm_2km,lsm_3km,lsm_5km,lsm_10km),
  list('_500m','_1km','_2km','_3km','_5km','_10km'),
  ~ lsm_rename(frame = .x, buffer = .y) %>%
    right_join(lsm_10km %>% select(siteID))) %>% 
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
  list(pc_500m,pc_1km,pc_2km,pc_3km,pc_5km,pc_10km),
  list('_500m','_1km','_2km','_3km','_5km','_10km'),
  ~ pc_process(frame = .x, buffer = .y) %>% 
    select(-siteID)) %>% 
  bind_cols() %>% 
  cbind(siteID = pc_500m$siteID)

# creating dataframes for analysis

abundance_frames <- map(
  list('Beat sheet', 'Visual', c('Beat sheet', 'Visual')),
  function(x){
    cc_full %>% 
      filter(
        Year %in% 2018:2021,
        ObservationMethod %in% x,
        !Region %in% c('ON','AB','AK'),
        SiteFK != 274) %>% 
      mutate(
        solstice_jday = if_else(
          Year %in% c(2018,2019),
          true = 172,
          false = 171)) %>% 
      filter(abs(solstice_jday - julianday) <= 14) %>%
      group_by(SiteFK, ID) %>% 
      summarize(
        total_arths = sum(Quantity),
        total_biomass = sum(Biomass_mg),
        spiders_biomass = sum(Biomass_mg[Group == 'spider']),
        spiders_present = Group == 'spider',
        beetles_biomass = sum(Biomass_mg[Group == 'beetle']),
        beetles_present = Group == 'beetle',
        cats_biomass = sum(Biomass_mg[Group == 'caterpillar']),
        cats_present = Group == 'caterpillar',
        hoppers_biomass = sum(Biomass_mg[Group == 'leafhopper']),
        hoppers_present = Group == 'leafhopper',
        truebugs_biomass = sum(Biomass_mg[Group == 'truebugs']),
        truebugs_present = Group == 'truebugs') %>% 
      mutate(
        spiders_biomass = if_else(
          is.na(spiders_biomass),
          0,
          spiders_biomass),
        beetles_biomass = if_else(
          is.na(beetles_biomass),
          0,
          beetles_biomass),
        cats_biomass = if_else(
          is.na(cats_biomass),
          0,
          cats_biomass),
        hoppers_biomass = if_else(
          is.na(hoppers_biomass),
          0,
          hoppers_biomass),
        truebugs_biomass = if_else(
          is.na(truebugs_biomass),
          0,
          truebugs_biomass)) %>% 
      group_by(SiteFK) %>% 
      summarize(
        mean_arths = mean(total_arths),
        mean_biomass = mean(total_biomass),
        # mean biomass of spiders per survey
        mean_spiders = mean(spiders_biomass),
        # percent of surveys where spiders were present
        percent_spiders = 100*sum(spiders_present, na.rm = T)/length(spiders_present),
        mean_beetles = mean(beetles_biomass),
        percent_beetles = 100*sum(beetles_present, na.rm = T)/length(beetles_present),
        mean_cats = mean(cats_biomass),
        percent_cats = 100*sum(cats_present, na.rm = T)/length(cats_present),
        mean_hoppers = mean(hoppers_biomass),
        percent_hoppers = 100*sum(hoppers_present, na.rm = T)/length(hoppers_present),
        mean_truebugs = mean(truebugs_biomass),
        percent_truebugs = 100*sum(truebugs_present, na.rm = T)/length(truebugs_present)) %>% 
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
  set_names(c('visuals_frame', 'beats_frame', 'full_frame')) %>% 
  list2env(.GlobalEnv)

# scale assessment

lm(
  percent_truebugs ~ forest_total_10km,
  data = full_frame) %>% 
  summary()

# next steps
## model strength of responses to each landscape scale to select for final models
## set up initial models with each response variable (mean_arths:percent_truebugs) across sampling types (visual, beat sheet, both)
## pull environmental data to use in models

# assessing common catch groups for local beat sheets

cc_full %>% 
  filter(
    Name %in% c(
      'NC State University',
      'UNC Chapel Hill Campus',
      'Prairie Ridge Ecostation',
      'NC Botanical Garden'),
    Year %in% 2018:2021,
    ObservationMethod == 'Beat sheet') %>% 
  group_by(Group) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))
