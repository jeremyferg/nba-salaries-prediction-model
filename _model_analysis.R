######################################
######################################
#######                       ########
######      MODEL ANALYSIS     #######
#######                       ########
######################################
######################################


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

load(here('results/baseline_fit_folds.rda'))
load(here('results/null_fit_folds.rda'))

################################################################################
################################################################################
################################################################################

rbind(
null_fit_folds |> 
  collect_metrics() |> 
  mutate(model = 'Null', .before = .metric),

baseline_fit_folds |> 
  collect_metrics() |> 
  mutate(model = 'OLS', .before = .metric)
) |> 
  select(-c(.estimator, .config)) |> 
  filter(.metric == 'rmse')
  
