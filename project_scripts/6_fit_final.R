#################################################
#################################################
#######                                  ########
######      FIT BEST PERFORMING MODEL     #######
#######                                  ########
#################################################
#################################################


#####################
##### Libraries #####
#####################

library(tidyverse)
library(tidymodels)
library(here)
library(parallel)

tidymodels_prefer()

# parallel processing
doParallel::registerDoParallel(detectCores(logical = TRUE))

#####################
##### Data Sets #####
#####################

load(here("results/interact_recipe/rf_interact_tuned.rda"))  
nba_seasons_train <- read_rds(here('data/splits_folds/nba_seasons_train.rds'))

################################################################################
################################################################################
################################################################################

# finalize workflow ----
final_wflow <-
  rf_outta_tuned |> 
  extract_workflow(rf_outta_tuned) |> 
  finalize_workflow(select_best(rf_outta_tuned, metric = 'rmse'))

# train final model ----
final_fit <- fit(final_wflow, nba_seasons_train)

# save fit model ----
save(final_fit, file = here('results/final_fit.rda'))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

