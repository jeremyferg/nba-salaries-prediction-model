#####################################
#####################################
#######                      ########
######      FIT BASELINES     #######
#######                      ########
#####################################
#####################################


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

load(here('recipes/nba_recipe_baseline.rda'))
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
  add_recipe(nba_recipe_baseline)

# resampling
null_fit_folds <- 
  fit_resamples(
    null_wflow,
    resamples = nba_seasons_folds_base,
    control = keep_wflow_rsample
  )

## baseline lm model ##

# creating specification
baseline_spec <-
  linear_reg() |> 
  set_engine('lm') |> 
  set_mode('regression') 

# defining workflow
baseline_wflow <-
  workflow() |> 
  add_model(baseline_spec) |> 
  add_recipe(nba_recipe_baseline)

# resampling
baseline_fit_folds <- 
  fit_resamples(
    baseline_wflow,
    resamples = nba_seasons_folds_base,
    control = keep_wflow_rsample
  )

## save out the folds ##

save(null_fit_folds, file = here('results/null_fit_folds.rda'))
save(baseline_fit_folds, file = here('results/baseline_fit_folds.rda'))





