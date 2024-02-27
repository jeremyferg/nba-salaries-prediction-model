########################################
########################################
#######                         ########
######      BASELINE METRICS     #######
#######                         ########
########################################
########################################


#####################
##### Libraries #####
#####################

library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

#####################
##### Data Sets #####
#####################

load(here('results/null_fit_folds.rda'))
load(here("results/baseline_fit_folds.rda"))
nba_seasons_test <- read_rds(here('data/splits_folds/nba_seasons_test.rds'))


################################################################################
################################################################################
################################################################################

#nba_seasons_metrics <- metric_set(rmse, mae, rsq)

null_fit_folds |> 
  collect_metrics()

baseline_fit_folds |> 
  collect_metrics()

##


