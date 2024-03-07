#####################################################
#####################################################
#######                                      ########
######      TUNE ELASTIC NET MODEL GENERAL    #######
#######                                      ########
#####################################################
#####################################################


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
enet_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine('glmnet') |> 
  set_mode('regression') 

# defining workflow
enet_outta_wflow <-
  workflow() |> 
  add_model(enet_spec) |> 
  add_recipe(nba_recipe_general)

# hyperparameter tuning values ----
extract_parameter_set_dials(enet_spec)

# change hyperparamter ranges
enet_outta_params <- extract_parameter_set_dials(enet_spec) |> 
  # N:= maximum number of random predictor columns we want to try 
  # should be less than the number of available columns
  update(mixture = mixture(range = c(0, 1)),
         penalty = penalty(range = c(-100000, 0)))

# build tuning grid
enet_grid <- grid_regular(enet_outta_params, levels = 5)

# resampling
set.seed(47338)
enet_outta_tuned <- 
  enet_outta_wflow |> 
  tune_grid(
    nba_seasons_folds_base, 
    grid = enet_grid,
    control = keep_wflow_rsample
  )

# write out results (fitted/trained workflows) ----

save(enet_outta_tuned, file = here('results/general_recipe/enet_general_tuned.rda'))
