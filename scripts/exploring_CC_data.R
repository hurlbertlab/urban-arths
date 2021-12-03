# do we want to select sites by number of survey dates or total number of surveys?


# setup -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)
library(sf)

cc_sites <- read_csv('data/raw/2021-11-18_Site.csv') %>% 
  filter(
    Name != 'Example Site',
    Latitude < 50)

cc_plants <- read_csv('data/raw/2021-11-18_Plant.csv')

cc_surveys <- read_csv('data/raw/2021-11-18_Survey.csv')

cc_arths <- read_csv('data/raw/2021-11-18_ArthropodSighting.csv')

us_map <- st_read('data/geographic/states.shp')

nc <- us_map %>% 
  filter(NAME == 'North Carolina')

sites_sf <- 
  cc_sites %>% 
  st_as_sf(
    coords = c('Longitude', 'Latitude'),
    crs = st_crs(us_map))


# assess data quantity ----------------------------------------------------

# make a table of number of survey dates each year at each site - what are sites 2 and 258 without names?

survey_counts <- cc_sites %>% 
  select(SiteID = ID) %>% 
  right_join(
    cc_plants %>% select(PlantID = ID, SiteFK),
    by = c('SiteID' = 'SiteFK')) %>% 
  right_join(
    cc_surveys %>% 
      select(surveyID = ID, PlantFK, LocalDate),
    by = c('PlantID' = 'PlantFK')) %>% 
  select(SiteID, LocalDate) %>% 
  distinct() %>% 
  mutate(Year = year(LocalDate)) %>% 
  group_by(SiteID, Year) %>% 
  summarize(survey_dates = n()) %>% 
  ungroup() %>% 
  left_join(
    cc_sites %>% 
      select(SiteID = ID, Name, Latitude, Longitude, Region),
    by = 'SiteID') %>% 
  relocate(SiteID, Name, Year, survey_dates, Region, Latitude, Longitude)

# assess number of sites with more than 10 surveys each year

survey_counts %>% 
  filter(survey_dates >= 10) %>% 
  group_by(Year) %>% 
  summarize(n_sites = n())

# assess sites with more than 10 surveys every year 2018-2020

survey_counts %>% 
  select(SiteID, Year, survey_dates) %>% 
  filter(survey_dates >= 10, Year == 2018) %>%
  inner_join(
    survey_counts %>% 
      select(SiteID, Year, survey_dates) %>% 
      filter(survey_dates >= 10, Year == 2019),
    by = 'SiteID') %>% 
  select(SiteID) %>% 
  inner_join(
    survey_counts %>% 
      select(SiteID, Year, survey_dates) %>% 
      filter(survey_dates >= 10, Year == 2020),
    by = 'SiteID') %>% 
  select(SiteID) %>% 
  left_join(
    cc_sites %>% 
      select(ID, Name, Region, Latitude, Longitude),
    by = c('SiteID' = 'ID'))
  

# maps --------------------------------------------------------------------

ggplot(us_map) +
  geom_sf() +
  geom_sf(
    data = survey_counts %>% 
      filter(
        !is.na(Name),
        Year %in% 2018:2021,
        survey_dates >= 10) %>% 
      st_as_sf(
        coords = c('Longitude', 'Latitude'),
        crs = st_crs(us_map)),
    mapping = aes(color = survey_dates)) +
  facet_wrap(~Year) +
  theme_void()

