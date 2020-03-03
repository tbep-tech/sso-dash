library(tidyverse)
library(R.matlab)
library(here)
library(lubridate)

# model parameters --------------------------------------------------------

modprm <- readMat(here::here('data/data-raw/LR_final_20200109.mat'))

save(modprm, file = here::here('data/modprm.RData'), compress = 'xz')

# model data

indvar0 <- read.csv(here::here('data/data-raw/LR_final_indvar0.csv'), stringsAsFactors = F) %>% 
  mutate(stime0 = dmy(stime0))

save(indvar0, file = here::here('data/indvar0.RData'), compress = 'xz')
