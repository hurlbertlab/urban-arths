
# setup -------------------------------------------------------------------

library(tidyverse)

mass_regs <-  read.csv('data/processed/arthropod_length_weight_regressions.csv', header = TRUE, stringsAsFactors = FALSE)

cleaned_cc <- read_csv('data/processed/flagged_dataset_2022-01-27.csv')


# recalculating biomass ---------------------------------------------------

mass_update <- cleaned_cc %>%
  left_join(mass_regs, by = 'Group') %>%
  mutate(Biomass_mg = Quantity*a_constant*Length^b_exponent) %>% 
  select(ID:actionTaken)

write.csv(mass_update, str_c('data/processed/cleaned_cc_', as.character(lubridate::today()), '.csv'), row.names = F)
