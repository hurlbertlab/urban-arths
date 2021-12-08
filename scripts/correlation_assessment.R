
# setup -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(corrplot)

cc_sites <- read_csv('data/raw/2021-11-18_Site.csv') %>% 
  filter(
    Name != 'Example Site',
    Latitude < 50)

cc_plants <- read_csv('data/raw/2021-11-18_Plant.csv')

cc_surveys <- read_csv('data/raw/2021-11-18_Survey.csv')

lsm_500m <- read_csv('data/processed/lsm_metrics_500m.csv')

lsm_1000m <- read_csv('data/processed/lsm_metrics_1000m.csv')

lsm_3000m <- read_csv('data/processed/lsm_metrics_3000m.csv')

lsm_5000m <- read_csv('data/processed/lsm_metrics_5000m.csv')

lsm_10000m <- read_csv('data/processed/lsm_metrics_10000m.csv')

pc_500m <- read_csv('data/processed/percent_cover_500m.csv')

pc_1000m <- read_csv('data/processed/percent_cover_1000m.csv')

pc_3000m <- read_csv('data/processed/percent_cover_3000m.csv')

pc_5000m <- read_csv('data/processed/percent_cover_5000m.csv')

pc_10000m <- read_csv('data/processed/percent_cover_10000m.csv')


# plot correlation matrix ----------------------------------------

mapped_list <- map2(
  list(
    pc_500m,pc_1000m,pc_3000m,pc_5000m,pc_10000m,
    lsm_500m,lsm_1000m,lsm_3000m,lsm_5000m,lsm_10000m),
  c('500m','1000m','3000m','5000m','10000m',
    '500m','1000m','3000m','5000m','10000m'),
  function(x,y){
    
    x %>% 
      rename_with(~ str_c(., y, sep = '_'), .cols = 3:length(x)) %>% 
      select(-Name)
    
  })

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

combined_data %>% 
  cor(use = 'complete.obs') %>%
  corrplot(
    method = 'color',
    type = 'upper',
    tl.col = 'black',
    tl.cex = 0.6,
    p.mat = p_vals,
    sig.level = 0.01,
    insig = 'blank')
