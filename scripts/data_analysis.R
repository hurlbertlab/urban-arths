
# setup -------------------------------------------------------------------

library(tidyverse)

cc_full <- read_csv('data/raw/fullCCDataset_2021-12-01.csv')


# creating dataframes for analysis ----------------------------------------

cc_full %>% 
  filter(
    Year %in% 2018:2021,
    ObservationMethod == 'Beat sheet') %>% 
  mutate(arthID = if_else(is.na(arthID), 0, arthID)) %>%
  group_by(ID) %>% 
  summarize(
    total_arths = sum(Quantity),
    total_biomass = sum(Biomass_mg))
  
