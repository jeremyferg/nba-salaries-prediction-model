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

tidymodels_prefer()

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
    resamples = nba_seasons_folds,
    control = keep_wflow_rsample
  )

save(null_fit_folds, file = here('results/null_fit_folds.rda'))





