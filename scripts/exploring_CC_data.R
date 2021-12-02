
# setup -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)

cc_sites <- read_csv('data/raw/2021-06-22_Site.csv') %>% 
  filter(
    Name != 'Example Site',
    Latitude < 50)

cc_plants <- read_csv('data/raw/2021-06-22_Plant.csv')

cc_surveys <- read_csv('data/raw/2021-06-22_Survey.csv')

cc_arths <- read_csv('data/raw/2021-06-22_ArthropodSighting.csv')


# assess data quantity ----------------------------------------------------

survey_counts <- cc_sites %>% 
  select(SiteID = ID) %>% 
  right_join(
    cc_plants %>% select(PlantID = ID, SiteFK),
    by = c('SiteID' = 'SiteFK')) %>% 
  right_join(
    cc_surveys %>% select(surveyID = ID, PlantFK, LocalDate),
    by = c('PlantID' = 'PlantFK')) %>% 
  mutate(Year = year(LocalDate)) %>% 
  group_by(SiteID, Year) %>% 
  summarize(n_surveys = length(surveyID)) %>% 
  ungroup() %>% 
  left_join(
    cc_sites %>% 
      select(SiteID = ID, Name, Latitude, Longitude, Region, OpenToPublic, Active),
    by = 'SiteID') %>% 
  relocate(SiteID, Name, Year, n_surveys, Region, Latitude, Longitude, OpenToPublic, Active)
