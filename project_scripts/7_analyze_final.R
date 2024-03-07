############################################
############################################
#######                             ########
######      FINAL MODEL ANALYSIS     #######
#######                             ########
############################################
############################################


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

load(here('results/final_fit.rda'))
nba_seasons_test <- read_rds(here('data/splits_folds/nba_seasons_train.rds'))

################################################################################
################################################################################
################################################################################

# predicting  
seasons_test_res <- predict(final_fit, nba_seasons_test)

# binding prediction with actual value
seasons_test_res <- 
  bind_cols(seasons_test_res, nba_seasons_test) |> 
  select(adj_salary, .pred)

seasons_test_res

# metric set tibble
rmse(seasons_test_res, truth = adj_salary, estimate = .pred)

