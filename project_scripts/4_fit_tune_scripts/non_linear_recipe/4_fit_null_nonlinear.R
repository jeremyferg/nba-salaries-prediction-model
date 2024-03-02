###############################################
###############################################
#######                                ########
######      FIT NULL MODEL NONLINEAR    #######
#######                                ########
###############################################
###############################################


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

## null model ##

# creating specification
null_spec <-
  null_model() |> 
  set_engine('parsnip') |> 
  set_mode('regression') 

# defining workflow
null_wflow <-
  workflow() |> 
  add_model(null_spec) |> 
  add_recipe(nba_recipe_nonlinear)

# resampling
null_fit_folds <- 
  fit_resamples(
    null_wflow,
    resamples = nba_seasons_folds_base,
    control = keep_wflow_rsample
  )

## save out the folds ##

save(null_fit_folds, file = here('results/null_fit_folds_nonlinear.rda'))



