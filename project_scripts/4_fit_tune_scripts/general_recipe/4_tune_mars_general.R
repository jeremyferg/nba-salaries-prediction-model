##################################################################################
##################################################################################
#######                                                                     ######
######      TUNE MULTIVARIATE ADAPTIVE REGRESSION SPLINES MODEL GENERAL    #######
#######                                                                     ######
##################################################################################
##################################################################################

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

# elastic net
mars_spec <-
  mars(
    num_terms = tune(),
    prod_degree = tune(),
    prune_method = 'backward'
  ) |> 
  set_engine('earth') |> 
  set_mode('regression') 

# defining workflow
mars_general_wflow <-
  workflow() |> 
  add_model(mars_spec) |> 
  add_recipe(nba_recipe_general)

# hyperparameter tuning values ----
extract_parameter_set_dials(mars_spec)

# change hyperparamter ranges
mars_general_params <- extract_parameter_set_dials(mars_spec) |> 
  # N:= maximum number of random predictor columns we want to try 
  # should be less than the number of available columns
  update(num_terms = num_terms(range = c(1, 50)),
         prod_degree = prod_degree(range = c(1, 5)))

# build tuning grid
mars_grid <- grid_regular(mars_general_params, levels = 5)

# resampling
set.seed(359376)
mars_general_tuned <- 
  mars_general_wflow |> 
  tune_grid(
    nba_seasons_folds_base, 
    grid = mars_grid,
    control = keep_wflow_rsample
  )

# write out results (fitted/trained workflows) ----

save(mars_general_tuned, file = here('results/general_recipe/mars_general_tuned.rda'))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


