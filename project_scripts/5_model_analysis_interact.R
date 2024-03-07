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

load(here("results/interact_recipe/ann_interact_tuned.rda"))
load(here("results/interact_recipe/bt_interact_tuned.rda"))  
load(here("results/interact_recipe/enet_interact_tuned.rda"))    
load(here("results/interact_recipe/lm_fit_folds_interact.rda"))  
load(here("results/interact_recipe/mars_interact_tuned.rda"))
load(here("results/interact_recipe/null_fit_folds_interact.rda"))
load(here("results/interact_recipe/rf_interact_tuned.rda"))   

################################################################################
################################################################################
################################################################################


rbind(
  
  ann_outta_tuned |> 
    show_best(metric = 'rmse') |> 
    slice_head() |> 
    select(c(.metric, mean, n, std_err)) |>
    mutate(model = 'ANN', .before = .metric),
  
  bt_outta_tuned |> 
    show_best(metric = 'rmse') |> 
    slice_head() |> 
    select(c(.metric, mean, n, std_err)) |>
    mutate(model = 'Boosted Trees', .before = .metric),
  
  enet_outta_tuned |> 
    show_best(metric = 'rmse') |> 
    slice_head() |> 
    select(c(.metric, mean, n, std_err)) |>
    mutate(model = 'Elastic Net', .before = .metric),
  
  lm_fit_folds |> 
    show_best(metric = 'rmse') |> 
    slice_head() |> 
    select(c(.metric, mean, n, std_err)) |>
    mutate(model = 'OLS', .before = .metric),
  
  mars_outta_tuned |> 
    show_best(metric = 'rmse') |> 
    slice_head() |> 
    select(c(.metric, mean, n, std_err)) |> 
    mutate(model = 'MARS', .before = .metric),
  
  null_fit_folds |> 
    show_best(metric = 'rmse') |> 
    slice_head() |> 
    select(c(.metric, mean, n, std_err)) |> 
    mutate(model = 'Null', .before = .metric),
  
  rf_outta_tuned |> 
    show_best(metric = 'rmse') |> 
    slice_head() |> 
    select(c(.metric, mean, n, std_err)) |> 
    mutate(model = 'Random Forest', .before = .metric)
  
)
  
enet_outta_tuned |> collect_metrics(metric = 'rmse') |> 
  print(n = 50)
