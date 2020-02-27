library(tidyverse)
library(R.matlab)
library(here)


# model parameters --------------------------------------------------------

modprm <- readMat(here::here('data/data-raw/LR_final_20200109.mat'))

# model data

moddat <- readMat(here::here('data/data-raw/LR_final_indvar0.mat'))