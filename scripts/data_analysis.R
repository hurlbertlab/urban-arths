
# setup -------------------------------------------------------------------

library(tidyverse)

library(lme4)

library(fields)

cc_full <- read_csv('data/processed/cleaned_cc_2022-04-06.csv', ) %>% 
  filter(
    Name != 'Example Site',
    # remove sites outside continental US
    !Region %in% c('ON','AB','AK'),
    # remove rows flagged to remove from CC QA/QC
    status != 'remove') %>% 
  # functionally convert rows where arths are 1mm to rows where no arths were observed
  mutate(
    arthID = ifelse(Length < 5, NA, arthID),
    Group = ifelse(Length <5 , NA, Group),
    Length = ifelse(Length <5 , NA, Length),
    Quantity = ifelse(Length < 5, 0, Quantity),
    Biomass_mg = ifelse(Length < 5, NA, Biomass_mg))

sites <- read_csv('data/raw/2021-11-18_Site.csv') %>% 
  filter(
    Name != 'Example Site',
    !Region %in% c('ON','AB','AK'))

cc_plants <- read_csv('data/raw/2021-11-18_Plant.csv') %>% 
  # filter to sites present in other frames by site ID because Region and Name are absent
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
# buffer argument should be formatted as '_*m' or '_*km'

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
# same buffer format as lsm_rename

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
        !Region %in% c('ON','AB','AK')) %>% 
      mutate(
        solstice_jday = if_else(
          Year %in% c(2018,2019),
          true = 172,
          false = 171)) %>% 
      filter(abs(solstice_jday - julianday) <= 14) %>%
      group_by(SiteFK, ID) %>% 
      summarize(
        total_arths = sum(Quantity, na.rm = T),
        total_biomass = sum(Biomass_mg, na.rm = T),
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
      group_by(SiteFK) %>% 
      summarize(
        mean_arths = mean(total_arths, na.rm = T),
        mean_biomass = mean(total_biomass, na.rm = T),
        # mean biomass of spiders per survey
        mean_spiders = sum(spiders_biomass, na.rm = T)/length(spiders_biomass),
        # prop of surveys where spiders were present
        prop_spiders = sum(spiders_present, na.rm = T)/length(spiders_present),
        mean_beetles = sum(beetles_biomass, na.rm = T)/length(beetles_biomass),
        prop_beetles = sum(beetles_present, na.rm = T)/length(beetles_present),
        mean_cats = sum(cats_biomass, na.rm = T)/length(cats_biomass),
        prop_cats = sum(cats_present, na.rm = T)/length(cats_present),
        mean_hoppers = sum(hoppers_biomass, na.rm = T)/length(hoppers_biomass),
        prop_hoppers = sum(hoppers_present, na.rm = T)/length(hoppers_present),
        mean_truebugs = sum(truebugs_biomass, na.rm = T)/length(truebugs_biomass),
        prop_truebugs = sum(truebugs_present, na.rm = T)/length(truebugs_present)) %>% 
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

# correlations

# test spearman rank correlation calculations

cor.test(
  x = full_frame$mean_arths,
  y = full_frame$forest_total_3km,
  method = 'spearman')

rank_test <- tibble(
  mean_arths = rank(full_frame$mean_arths, ties.method = 'average'),
  forest_total_3km = rank(full_frame$forest_total_3km, ties.method = 'average'))

test_rho <- cov(rank_test) / (sd(rank_test$mean_arths) * sd(rank_test$forest_total_3km))

n <- length(rank_test$mean_arths)
r <- cor(x = rank_test$mean_arths, y = rank_test$forest_total_3km, method = 'pearson')
s <- (n^3 - n) * (1 - r) / 6
s

t <- r * sqrt((n - 2) / (1 - r^2))
p <- 2 * (1-pt(q = t, df = n - 2))
p

# make a dataframe of ranks for all variables
ranks <- sapply(
  X = full_frame %>% select(!c(SiteFK, dom_spp, Latitude, Longitude)),
  FUN = function(x) rank(x, ties.method = 'average')) %>% 
  as_tibble() %>% 
  cbind(SiteFK = full_frame$SiteFK) %>% 
  relocate(SiteFK)

# map spearman coefficients to a list
spearman_coefficients <- map(
  ranks[,2:13],
  function(arths) 
    map(
      ranks[,14:67],
      function(landscape) (cov(cbind(arths, landscape)) / (sd(arths) * sd(landscape)))[[2]]
    )
)

spearman_df <- map(
  1:12,
  ~ spearman_coefficients[.x] %>% 
    unlist() %>% 
    bind_rows() %>% 
    rename_with(
      .fn = function(n) str_remove(n, '.*\\.')
    )
) %>% 
  bind_rows() %>% 
  cbind(arth_trait = names(spearman_coefficients)) %>% 
  relocate(arth_trait)

map(
  c('500m','1km','2km','3km','5km','10km'),
  ~ spearman_df %>% 
    select(
      arth_trait,
      ends_with(.x)) %>% 
    column_to_rownames(var = 'arth_trait')
) %>% 
  set_names(nm = c(
    'spearman_500m',
    'spearman_1km',
    'spearman_2km',
    'spearman_3km',
    'spearman_5km',
    'spearman_10km')
  ) %>% 
  list2env(envir = .GlobalEnv)

spearman_p <- map(
  ranks[,2:13],
  function(arths){
    map(
      ranks[,14:67],
      function(landscape){
    n <- nrow(ranks)
    r <- cor(x = arths, y = landscape, method = 'pearson')
    t <- r * sqrt((n - 2) / (1 - r^2))
    p <- 2 * (1-pt(q = t, df = n - 2))
    p
      })
  })

spearman_p_df <- map(
  1:12,
  ~ spearman_p[.x] %>% 
    unlist() %>% 
    bind_rows() %>% 
    rename_with(
      .fn = function(n) str_remove(n, '.*\\.')
    )
) %>% 
  bind_rows() %>% 
  cbind(arth_trait = names(spearman_p)) %>% 
  relocate(arth_trait)

map(
  c('500m','1km','2km','3km','5km','10km'),
  ~ spearman_p_df %>% 
    select(
      arth_trait,
      ends_with(.x)) %>% 
    column_to_rownames(var = 'arth_trait')
) %>% 
  set_names(nm = c(
    'spearman_p_500m',
    'spearman_p_1km',
    'spearman_p_2km',
    'spearman_p_3km',
    'spearman_p_5km',
    'spearman_p_10km')
  ) %>% 
  list2env(envir = .GlobalEnv)

image.real <- function(
    mat, 
    xCol = c('green4', 'green2', 'green', 'white', 'white', 'red', 'red2', 'red4'), 
    range = c(-1,1), 
    x.labels = rownames(mat), 
    y.labels = colnames(mat)) { 
  mat <- t(mat)[,nrow(mat):1]
  fields::image.plot(
    mat, 
    axes = FALSE, 
    zlim = range, 
    col = colorRampPalette(xCol)(30))
  axis(1, at = seq(0, 1, length = nrow(mat)), labels = x.labels, cex.axis = 0.5, las = 2)
  axis(2, at = seq(0, 1, length = ncol(mat)), labels = y.labels, las = 2, cex.axis = 0.5, las = 1)
  box() 
}

image.real(mat = spearman_3km)


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
