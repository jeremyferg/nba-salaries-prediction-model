###########################################################################
###########################################################################
#######                                                            ########
######      TUNE MULTIVARIATE ADAPTIVE REGRESSION SPLINES MODEL     #######
#######                                                            ########
###########################################################################
###########################################################################


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



