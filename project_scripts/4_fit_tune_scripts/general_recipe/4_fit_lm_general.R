##################################################
##################################################
#######                                   ########
######      FIT LINEAR MODEL NONLINEAR     #######
#######                                   ########
##################################################
##################################################


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

## lm model ##

# creating specification
lm_spec <-
  linear_reg() |> 
  set_engine('lm') |> 
  set_mode('regression') 

# defining workflow
lm_wflow <-
  workflow() |> 
  add_model(lm_spec) |> 
  add_recipe(nba_recipe_general)

# resampling
lm_fit_folds_general <- 
  fit_resamples(
    lm_wflow,
    resamples = nba_seasons_folds_base,
    control = keep_wflow_rsample
  )

## save out the folds ##

save(lm_fit_folds_general, file = here('results/general_recipe/lm_fit_folds_general.rda'))


