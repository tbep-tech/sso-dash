library(tidyverse)
library(here)
library(lubridate)
library(patchwork)

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
  mutate(date = dmy(date)) %>% 
  mutate(
    precp = precp * 2.54, 
    var2 = var2 * 2.54, 
    var4 = var4 * 2.54 
  )

p1 <- ggplot(dat, aes(x = date, y = precp)) + 
  geom_line(colour = 'grey') + 
  geom_vline(data = dat[dat$overflow == 1, ], aes(xintercept = date), linetype = 'dashed', color = 'red', size = 1) + 
  theme_bw() + 
  scale_x_date(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = NULL, y = '2-day precip (cm)') + 
  theme_minimal() + 
  theme(axis.text.x = element_blank()) + 
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank()
  )

p2 <- ggplot(dat, aes(x = date, y = water)) + 
  geom_line(colour = 'lightblue') + 
  geom_vline(data = dat[dat$overflow == 1, ], aes(xintercept = date), linetype = 'dashed', color = 'red', size = 1) + 
  theme_bw() + 
  scale_x_date(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = NULL, y = '2-day ax sea level (m)') + 
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank()
  )

p <- p1 + p2 + plot_layout(ncol = 1, heights = c(0.95, 1))

png('~/Desktop/sso-ex.png', height = 4, width = 9, units = 'in', res = 200, family = 'serif')
p
dev.off()
