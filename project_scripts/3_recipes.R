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
nba_recipe_general <-
  recipe(adj_salary ~ mp + fg + ft + orb + drb + trb + ast + stl + blk + tov + pf + pts + pos + g + gs + all_star, 
         data = nba_seasons_train) |> 
  
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_interact( ~ starts_with('pos_'):starts_with('all_star_')) |> 
  step_interact( ~ ast:tov + fg:pts) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_predictors(), threshold = .7)

# check recipe
nba_recipe_general |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()


## saving recipe ##

save(nba_recipe_general, file = here('recipes/nba_recipe_general.rda'))


#step_mutate(x3p_percent = if_else(is.na(x3p_percent) & !is.na(ft_percent), 0, x3p_percent)) |>
#step_impute_mean(c(x3p_percent, ft_percent)) |> 