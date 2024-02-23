###############################
###############################
#######                ########
######      RECIPES     #######
#######                ########
###############################
###############################


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

nba_seasons_train <- read_rds(here('data/splits_folds/nba_seasons_train.rds'))

################################################################################
################################################################################
################################################################################

## basic recipe ##

# the baseline recipe has 49 predictors
nba_recipe_baseline <-
  recipe(adj_salary ~ ., data = nba_seasons_train) |> 
  step_rm(c(salary, player, team, season)) |> 
  step_mutate(x3p_percent = if_else(is.na(x3p_percent) & !is.na(ft_percent), 0, x3p_percent)) |> 
  step_impute_mean(c(x3p_percent, ft_percent)) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

# check recipe
nba_recipe_baseline |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()


## saving recipe ##

save(nba_recipe_baseline, file = here('recipes/nba_recipe_baseline.rda'))

