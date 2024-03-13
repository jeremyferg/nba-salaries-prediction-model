############################################################
############################################################
#######                                             ########
######      TUNE NEURAL NETWORKS MODEL INTERACT      #######
#######                                             ########
############################################################
############################################################

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

load(here('recipes/nba_recipe_interact.rda'))
load(here("data/splits_folds/nba_seasons_folds_base.rda"))
load(here('results/keep_wflow_rsample.rda'))

################################################################################
################################################################################
################################################################################

# elastic net
ann_spec <-
  mlp(
    penalty = tune(),
    hidden_units = tune(),
    epoch = tune()
  ) |> 
  set_engine('nnet') |> 
  set_mode('regression') 

# defining workflow ----
ann_interact_wflow <-
  workflow() |> 
  add_model(ann_spec) |> 
  add_recipe(nba_recipe_interact)

# hyperparameter tuning values ----
extract_parameter_set_dials(ann_spec)

# change hyperparamter ranges
ann_interact_params <- extract_parameter_set_dials(ann_spec) |> 
  # N:= maximum number of random predictor columns we want to try 
  # should be less than the number of available columns
  update(hidden_units = mixture(range = c(0, 5)),
         penalty = penalty(range = c(-10, 0),),
         epochs = epochs(range = c(100, 750)))

# build tuning grid
ann_grid <- grid_regular(ann_interact_params, levels = 5)

# resampling
set.seed(56709)
ann_interact_tuned <- 
  ann_interact_wflow |> 
  tune_grid(
    nba_seasons_folds_base, 
    grid = ann_grid,
    control = keep_wflow_rsample
  )

# write out results (fitted/trained workflows) ----

save(ann_interact_tuned, file = here('results/interact_recipe/ann_interact_tuned.rda'))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
