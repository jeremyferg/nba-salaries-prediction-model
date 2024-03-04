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

## general recipe ##

# the baseline recipe has 49 predictors
nba_recipe_general <-
  recipe(adj_salary ~ mp + fg + ft + orb + drb + trb + ast + stl + blk + tov + 
           pf + pts + pos + g + gs + all_star + ws, 
         data = nba_seasons_train) |> 
  
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_interact( ~ starts_with('pos_'):starts_with('all_star_')) |> 
  step_interact( ~ ast:tov + fg:pts + mp:pts) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_predictors(), threshold = .7)

# check recipe
nba_recipe_general |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()

## nonlinear recipe ##

# orb --> 8 DoF
# ast --> 4 DoF
# blk --> 5 DoF
# stl --> 4 DoF, step bs
#ft_percent --> 5 DoF, step bs
nba_recipe_nonlinear <-
  recipe(adj_salary ~ pts + drb + orb + ast + blk + stl + tov + pf +
           fg_percent + x2p_percent + x3p_percent + ft_percent + ts_percent + 
           e_fg_percent + ws + mp + g + gs + pos + all_star, 
         data = nba_seasons_train) |> 
  
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  
  # omit any 0% (expect 3pt)
  step_mutate(fg_percent = if_else(fg_percent < .05, NA, fg_percent)) |> 
  step_mutate(x2p_percent = if_else(x2p_percent < .05, NA, x2p_percent)) |> 
  step_mutate(ft_percent = if_else(ft_percent < .05, NA, ft_percent)) |> 
  step_mutate(ts_percent = if_else(ts_percent < .05, NA, ts_percent)) |> 
  step_mutate(e_fg_percent = if_else(e_fg_percent < .05, NA, e_fg_percent)) |>
  step_naomit(c(fg_percent, x2p_percent, ft_percent, ts_percent, e_fg_percent)) |> 
  
  # make any 100% just the means
  step_mutate(fg_percent = if_else(fg_percent > .95, NA, fg_percent)) |> 
  step_mutate(x2p_percent = if_else(x2p_percent > .95, NA, x2p_percent)) |>
  step_mutate(x3p_percent = if_else(x3p_percent > .95, NA, x3p_percent)) |>
  step_mutate(ft_percent = if_else(ft_percent > .95, NA, ft_percent)) |> 
  step_mutate(ts_percent = if_else(ts_percent > .95, NA, ts_percent)) |> 
  step_mutate(e_fg_percent = if_else(e_fg_percent > .95, NA, e_fg_percent)) |>
  step_impute_mean(c(fg_percent, x2p_percent, x3p_percent, ft_percent, ts_percent, e_fg_percent)) |> 
  
  #non linearity
  step_ns(orb, deg_free = 8) |> 
  step_ns(ast, deg_free = 4) |> 
  step_ns(blk, deg_free = 5) |> 
  step_bs(stl, deg_free = 4) |> 
  step_bs(ft_percent, deg_free = 5) |> 
  
  # step interacts used in general function
  step_interact( ~ starts_with('pos_'):starts_with('all_star_')) |> 
  step_interact( ~ starts_with('ast_'):tov + fg_percent:pts) |> 
  
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_predictors(), threshold = .7)

# check recipe
nba_recipe_nonlinear |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()


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

## interact recipe ##

nba_recipe_interact <-
  recipe(adj_salary ~ five_years + ten_years + market_size + pos + all_star + g 
         + gs + ast + tov + mp + pts + pf + ws + playoffs + fg + fg_percent +
           ft + ft_percent + x3p + x3p_percent,
         data = nba_seasons_train) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  
  # omit any 0% (expect 3pt)
  step_mutate(fg_percent = if_else(fg_percent < .05, NA, fg_percent)) |> 
  step_mutate(ft_percent = if_else(ft_percent < .05, NA, ft_percent)) |> 
  step_naomit(c(fg_percent, ft_percent)) |> 
  
  # make any 100% just the means
  step_mutate(fg_percent = if_else(fg_percent > .95, NA, fg_percent)) |> 
  step_mutate(x3p_percent = if_else(x3p_percent > .95, NA, x3p_percent)) |>
  step_mutate(ft_percent = if_else(ft_percent > .95, NA, ft_percent)) |> 
  step_impute_mean(c(fg_percent, x3p_percent, ft_percent)) |> 
  
  
  # interactions #
  step_interact(~ starts_with('all_star_'):starts_with('market_size_')) |> 
  step_interact(~ gs:starts_with('five_years_') + gs:starts_with('ten_years_')) |>
  step_interact(~ g:starts_with('five_years_') + g:starts_with('ten_years_')) |>
  step_interact(~ starts_with('ten_years_'):starts_with('market_size_')) |> 
  step_interact(~ gs:starts_with('pos_') + g:starts_with('pos_')) |> 
  step_interact(~ ws:starts_with('playoffs_')) |> 
  step_interact(~ ast:tov + mp:pts + mp:pf) |> 
  step_interact(~ ft:ft_percent + fg:fg_percent + x3p:x3p_percent) |> 
  
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_predictors(), threshold = .7)

# check recipe
nba_recipe_interact |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()


## saving recipe ##

save(nba_recipe_general, file = here('recipes/nba_recipe_general.rda'))
save(nba_recipe_outta, file = here('recipes/nba_recipe_outta.rda'))
save(nba_recipe_interact, file = here('recipes/nba_recipe_interact.rda'))
save(nba_recipe_nonlinear, file = here('recipes/nba_recipe_nonlinear.rda'))

