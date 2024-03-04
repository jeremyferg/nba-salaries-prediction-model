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

load(here('recipes/nba_recipe_nonlinear.rda'))
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
bt_outta_wflow <- 
  workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(nba_recipe_nonlinear)

# hyperparameter tuning values ----

extract_parameter_set_dials(bt_spec)

bt_outta_params <- extract_parameter_set_dials(bt_spec) |> 
  update(mtry = mtry(c(1, 15)),
         min_n = min_n(range = c(1, 10)),
         learn_rate = learn_rate(range = c(1/10, 19/20)),
         trees = trees(range = c(100, 1000)))

# build tuning grid
bt_grid <- grid_regular(bt_outta_params, levels = 5)

# fit workflows/models ----
set.seed(57879)
bt_outta_tuned <- 
  bt_outta_wflow |> 
  tune_grid(
    nba_seasons_folds_base, 
    grid = bt_grid,
    control = keep_wflow_rsample
  )

# write out results (fitted/trained workflows) ----

save(bt_outta_tuned, file = here('results/nonlinear_recipe/bt_nonlinear_tuned.rda'))

