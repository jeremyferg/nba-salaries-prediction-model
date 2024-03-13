#######################################################
#######################################################
#######                                          ######
######      TUNE RANDOM FOREST MODEL GENERAL    #######
#######                                          ######
#######################################################
#######################################################

## USING RANDOM SEED ##

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

load(here('recipes/nba_recipe_general.rda'))
load(here("data/splits_folds/nba_seasons_folds_base.rda"))
load(here('results/keep_wflow_rsample.rda'))

################################################################################
################################################################################
################################################################################

# model specifications ----
rf_spec <- 
  rand_forest(
    trees = tune(), 
    min_n = tune(),
    mtry = tune()
  ) |> 
  set_mode('regression') |> 
  set_engine("ranger")

# define workflows ----
rf_general_wflow <- 
  workflow() |> 
  add_model(rf_spec) |> 
  add_recipe(nba_recipe_general)

# hyperparameter tuning values ----
extract_parameter_set_dials(rf_spec)

# change hyperparamter ranges
rf_general_params <- extract_parameter_set_dials(rf_spec) |> 
  # N:= maximum number of random predictor columns we want to try 
  # should be less than the number of available columns
  update(mtry = mtry(c(1, 15)),
         min_n = min_n(range = c(2, 4)),
         trees = trees(range = c(100, 1000))) 

# build tuning grid
rf_grid <- grid_regular(rf_general_params, levels = 5)

# fit workflows/models ----
set.seed(572497)
rf_general_tuned <- 
  rf_general_wflow |> 
  tune_grid(
    nba_seasons_folds_base, 
    grid = rf_grid,
    control = keep_wflow_rsample
  )

# write out results (fitted/trained workflows) ----

save(rf_general_tuned, file = here('results/general_recipe/rf_general_tuned.rda'))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


