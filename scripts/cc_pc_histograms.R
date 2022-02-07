
# setup -------------------------------------------------------------------

library(tidyverse)

pc_500 <- read_csv('data/processed/percent_cover_500m.csv')

pc_1000 <- read_csv('data/processed/percent_cover_1000m.csv')

pc_2000 <- read_csv('data/processed/percent_cover_2000m.csv')

pc_3000 <- read_csv('data/processed/percent_cover_3000m.csv')

pc_5000 <- read_csv('data/processed/percent_cover_5000m.csv')

pc_10000 <- read_csv('data/processed/percent_cover_10000m.csv')


# data prep ---------------------------------------------------------------

pc_process <- function(frame){
  frame %>% 
    rowwise() %>% 
    mutate(
      forest_total = forest_decid + forest_everg + forest_mix,
      devo_total = devo_open + devo_low + devo_med + devo_high) %>% 
    select(siteID, forest_total, devo_total)
}

map(
  list(pc_500,pc_1000,pc_2000,pc_3000,pc_5000,pc_10000),
  ~ pc_process(.)
) %>% 
  set_names('pc_500p','pc_1000p','pc_2000p','pc_3000p','pc_5000p','pc_10000p') %>% 
  list2env(.GlobalEnv)


# plotting histograms -----------------------------------------------------

ggplot(pc_500p, aes(x = 100*forest_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'forestgreen', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Forested Cover within 500m of Caterpillars Count! Sites',
    x = 'Percent Forested Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_1000p, aes(x = 100*forest_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'forestgreen', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Forested Cover within 1km of Caterpillars Count! Sites',
    x = 'Percent Forested Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_2000p, aes(x = 100*forest_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'forestgreen', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Forested Cover within 2km of Caterpillars Count! Sites',
    x = 'Percent Forested Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_3000p, aes(x = 100*forest_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'forestgreen', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Forested Cover within 3km of Caterpillars Count! Sites',
    x = 'Percent Forested Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_5000p, aes(x = 100*forest_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'forestgreen', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Forested Cover within 5km of Caterpillars Count! Sites',
    x = 'Total Forested Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_10000p, aes(x = 100*forest_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'forestgreen', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Forested Cover within 10km of Caterpillars Count! Sites',
    x = 'Total Forested Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_500p, aes(x = 100*devo_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'darkgoldenrod2', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Developed Cover within 500m of Caterpillars Count! Sites',
    x = 'Total Developed Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_1000p, aes(x = 100*devo_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'darkgoldenrod2', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Developed Cover within 1km of Caterpillars Count! Sites',
    x = 'Total Developed Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_2000p, aes(x = 100*devo_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'darkgoldenrod2', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Developed Cover within 2km of Caterpillars Count! Sites',
    x = 'Total Developed Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_3000p, aes(x = 100*devo_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'darkgoldenrod2', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Developed Cover within 3km of Caterpillars Count! Sites',
    x = 'Total Developed Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_5000p, aes(x = 100*devo_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'darkgoldenrod2', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Developed Cover within 5km of Caterpillars Count! Sites',
    x = 'Total Developed Cover',
    y = 'Count') +
  theme_bw()

ggplot(pc_10000p, aes(x = 100*devo_total)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = 'darkgoldenrod2', color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = 'Percent Developed Cover within 10km of Caterpillars Count! Sites',
    x = 'Total Developed Cover',
    y = 'Count') +
  theme_bw()
