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

## interacts recipe ##

#recipe(adj_salary ~ )


## out of control recipe ##

nba_recipe_outta <-
  recipe(adj_salary ~ years_in_league + five_years + ten_years + market_size + 
           playoffs + conference + pos + all_star + g + gs + mp,
         data = nba_seasons_train) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_interact(~ starts_with('all_star_'):starts_with('market_size_')) |> 
  step_interact(~ gs:starts_with('five_years_')) |>
  step_interact(~ gs:starts_with('ten_years_')) |> 
  step_interact(~ g:starts_with('five_years_')) |>
  step_interact(~ g:starts_with('ten_years_')) |> 
  step_interact(~ starts_with('ten_years_'):starts_with('market_size_')) |> 
  step_interact(~ gs:starts_with('pos_')) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_predictors(), threshold = .7)

# check recipe
nba_recipe_outta |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()

## saving recipe ##

save(nba_recipe_general, file = here('recipes/nba_recipe_general.rda'))
save(nba_recipe_outta, file = here('recipes/nba_recipe_outta.rda'))


#step_mutate(x3p_percent = if_else(is.na(x3p_percent) & !is.na(ft_percent), 0, x3p_percent)) |>
#step_impute_mean(c(x3p_percent, ft_percent)) |> 