library(tidyverse)
library(R.matlab)
library(here)
library(lubridate)


# recreate glm ------------------------------------------------------------

# import data
indvar0 <- read.csv(here::here('data/data-raw/LR_final_indvar0.csv'), stringsAsFactors = F)
depvar0 <- read.csv(here::here('data/data-raw/LR_final_depvar0.csv'), stringsAsFactors = F)

# combine independent, dependent, rename some things
dat <- bind_cols(depvar0, indvar0) %>% 
  select(
    overflow = DepVar, 
    precp = `indvar0...1.`,
    var2 = `indvar0...2.`,
    water = `indvar0...3.`, 
    var4 = `indvar0...4.`, 
    date = stime0
  ) %>% 
  mutate(date = dmy(date))

# remove all records where water < mean water level
dat <- dat %>% 
  filter(water >= mean(water))

# model
rskmod <- glm(overflow ~ precp + water + var2 + var4, family =binomial(link = "logit"), data = dat)

save(rskmod, file = here::here('data/rskmod.RData'), compress = 'xz')

# model data --------------------------------------------------------------

indvar0 <- read.csv(here::here('data/data-raw/LR_final_indvar0.csv'), stringsAsFactors = F) %>% 
  mutate(stime0 = dmy(stime0)) %>% 
  rename(
    precp = `indvar0...1.`,
    var2 = `indvar0...2.`,
    water = `indvar0...3.`, 
    var4 = `indvar0...4.`, 
    date = stime0
  )

save(indvar0, file = here::here('data/indvar0.RData'), compress = 'xz')
