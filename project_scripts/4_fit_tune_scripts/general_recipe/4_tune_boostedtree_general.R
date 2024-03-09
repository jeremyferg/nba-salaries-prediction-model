#########################################################
#########################################################
#######                                          ########
######      TUNE BOOSTED TREE MODEL NONLINEAR     #######
#######                                          ########
#########################################################
#########################################################

## NOTE: Seed used for randomization

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
bt_spec <- 
  boost_tree(
    trees = tune(),
    learn_rate = tune(), 
    min_n = tune(),
    mtry = tune(),
    
  ) |> 
  set_mode('regression') |> 
  set_engine("xgboost")

# define workflows ----
bt_general_wflow <- 
  workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(nba_recipe_general)

# hyperparameter tuning values ----

extract_parameter_set_dials(bt_spec)

bt_general_params <- extract_parameter_set_dials(bt_spec) |> 
  update(mtry = mtry(c(1, 15)),
         min_n = min_n(range = c(1, 10)),
         learn_rate = learn_rate(range = c(1/10, 19/20)),
         trees = trees(range = c(100, 1000)))

# build tuning grid
bt_grid <- grid_regular(bt_general_params, levels = 5)

# fit workflows/models ----
set.seed(57879)
bt_general_tuned <- 
  bt_general_wflow |> 
  tune_grid(
    nba_seasons_folds_base, 
    grid = bt_grid,
    control = keep_wflow_rsample
  )

# write out results (fitted/trained workflows) ----

save(bt_general_tuned, file = here('results/general_recipe/bt_general_tuned.rda'))

