
# setup -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(corrplot)

lsm_500m <- read_csv('data/processed/lsm_500m.csv')

lsm_1000m <- read_csv('data/processed/lsm_1km.csv')

lsm_3000m <- read_csv('data/processed/lsm_3km.csv')

lsm_5000m <- read_csv('data/processed/lsm_5km.csv')

lsm_10000m <- read_csv('data/processed/lsm_10km.csv')

pc_500m <- read_csv('data/processed/percent_cover_500m.csv')

pc_1000m <- read_csv('data/processed/percent_cover_1km.csv')

pc_3000m <- read_csv('data/processed/percent_cover_3km.csv')

pc_5000m <- read_csv('data/processed/percent_cover_5km.csv')

pc_10000m <- read_csv('data/processed/percent_cover_10km.csv')


# plot correlation matrix ----------------------------------------

# calculate total forest cover at each site
map(
  list(pc_500m,pc_1000m,pc_3000m,pc_5000m,pc_10000m),
  function(x){
   
    x %>% 
      rowwise() %>% 
      mutate(forest_total = forest_decid + forest_everg + forest_mix) %>% 
      select(siteID, forest_total)
      
  }) %>% 
  set_names(
    c('pc_500m_2', 'pc_1000m_2', 'pc_3000m_2', 'pc_5000m_2', 'pc_10000m_2')) %>% 
  list2env(envir = .GlobalEnv)

# put all data into a list of dataframes with columns renamed to include radii
mapped_list <- map2(
  list(
    pc_500m_2,pc_1000m_2,pc_3000m_2,pc_5000m_2,pc_10000m_2,
    lsm_500m,lsm_1000m,lsm_3000m,lsm_5000m,lsm_10000m),
  c('500m','1000m','3000m','5000m','10000m',
    '500m','1000m','3000m','5000m','10000m'),
  function(x,y){
    
    x %>% 
      rename_with(~ str_c(., y, sep = '_'), .cols = 2:length(x))
    
  })

# join all data into single dataframe
combined_data <- mapped_list[[1]] %>% 
  full_join(mapped_list[[2]]) %>% 
  full_join(mapped_list[[3]]) %>% 
  full_join(mapped_list[[4]]) %>% 
  full_join(mapped_list[[5]]) %>% 
  full_join(mapped_list[[6]]) %>% 
  full_join(mapped_list[[7]]) %>% 
  full_join(mapped_list[[8]]) %>% 
  full_join(mapped_list[[9]]) %>% 
  full_join(mapped_list[[10]])

# function to calculate matrix of p values for correlations between columns in a dataframe
pMatrix <- function(mat, ...){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p_vals <- pMatrix(combined_data)

# generate a visualizqtion of correlations between variables
combined_data %>% 
  cor(use = 'complete.obs') %>%
  corrplot(
    method = 'circle',
    type = 'upper',
    tl.col = 'black',
    tl.cex = 0.6,
    p.mat = p_vals,
    sig.level = 0.01,
    insig = 'blank')
