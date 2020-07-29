library(tidyverse)
library(R.matlab)
library(here)
library(lubridate)
library(extRemes)

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

# declustering data -------------------------------------------------------

# import data
indvar0 <- read.csv(here::here('data/data-raw/LR_final_indvar0.csv'), stringsAsFactors = F)

# rename and format date
dat <- indvar0 %>% 
  select(
    date = stime0,
    precp = `indvar0...1.`,
    water = `indvar0...3.`
  ) %>% 
  mutate(date = dmy(date))

dcldat <- crossing(
  sdup = c(3, 4, 5), 
  vardcl = c('precp', 'precp_water')
  ) %>% 
  group_by(sdup, vardcl) %>% 
  nest() %>% 
  mutate(
    data = purrr::pmap(list(sdup, vardcl), function(sdup, vardcl){

      if(vardcl == 'precp')
        
        out <- dat %>% 
          mutate(
            fltval = mean(precp, na.rm = T) + sdup * sd(precp, na.rm = T),
            dclprecp = decluster(precp, unique(fltval))
          ) %>% 
          filter(precp > fltval) %>% 
          select(-precp, -fltval) %>% 
          rename(precp = dclprecp)
        
      if(vardcl == 'precp_water')
        
        out <- dat %>% 
          mutate(
            prcval = mean(precp, na.rm = T) + sdup * sd(precp, na.rm = T),
            dclprecp = decluster(precp, unique(prcval)),    
            wtrval = mean(water, na.rm = T) + sdup * sd(water, na.rm = T),
            dclwater = decluster(water, unique(wtrval))
          ) %>% 
          filter(precp > prcval) %>% 
          select(-precp, -water, -prcval, -wtrval) %>% 
          rename(
            precp = dclprecp,
            water = dclwater
          )
        
      out <- out %>% 
        select(date, precp, water)
      
      return(out)
      
    })
  ) %>% 
  unnest(data)

write.csv(dcldat, here::here('data/data-raw', 'dcldat.csv'), row.names = F)

# future risk output ------------------------------------------------------
# 
# futrsk <- read.csv('data/data-raw/SSO_SLR_LookupC0.csv') %>% 
#   rename(
#     water = SLR..m., 
#     precp = Pchng..cm., 
#     prb = pi
#   ) %>% 
#   mutate(
#     water = round(water, 2), 
#     precp = round(precp, 1),
#     prb = round(prb, 1),
#     los = N_d - 1.96 * SE,
#     his = N_d + 1.96 * SE
#   ) %>% 
#   select(-SE, -SD)
# 
# save(futrsk, file = 'data/futrsk.RData')

# future probabilities by total per day -----------------------------------

# futdlyraw <- readMat('~/Desktop/SSO_SLR_Lookup0.mat')
# 
# futdly <- futdlyraw %>% 
#   .[[1]]

futyly <- read.csv(here::here('data/data-raw/SSO_SLR_yearly1.csv'), skip = 1, header = F)
futyly <- futyly[-1, 1:203]
names(futyly)[1:203] <- c('water', 'precp', 'prb', 1:200)
futyly <- futyly %>% 
  gather('yr', 'ndays', -water, -precp, -prb) %>% 
  mutate(
    yr = as.numeric(yr)
  ) %>% 
  mutate_all(as.numeric)

futrsk <- futyly %>% 
  group_by(water, precp, prb) %>% 
  summarise(
    N_d = mean(ndays),
    los = t.test(ndays)$conf.int[1], 
    his = t.test(ndays)$conf.int[2]
  )

save(futyly, file = here::here('data/futyly.RData'), compress = 'xz')
save(futrsk, file = here::here('data/futrsk.RData'), compress = 'xz')