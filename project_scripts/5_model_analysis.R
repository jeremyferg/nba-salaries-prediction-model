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

load(here("results/general_recipe/ann_general_tuned.rda"))
load(here("results/general_recipe/bt_general_tuned.rda"))  
load(here("results/general_recipe/enet_general_tuned.rda"))    
load(here("results/general_recipe/lm_fit_folds_general.rda"))  
load(here("results/general_recipe/mars_general_tuned.rda"))
load(here("results/general_recipe/null_fit_folds_general.rda"))
load(here("results/general_recipe/rf_general_tuned.rda"))

load(here("results/interact_recipe/ann_interact_tuned.rda"))
load(here("results/interact_recipe/bt_interact_tuned.rda"))  
load(here("results/interact_recipe/enet_interact_tuned.rda"))    
load(here("results/interact_recipe/lm_fit_folds_interact.rda"))  
load(here("results/interact_recipe/mars_interact_tuned.rda"))
load(here("results/interact_recipe/null_fit_folds_interact.rda"))
load(here("results/interact_recipe/rf_interact_tuned.rda"))   

load(here("results/nonlinear_recipe/ann_nonlinear_tuned.rda"))
load(here("results/nonlinear_recipe/bt_nonlinear_tuned.rda"))  
load(here("results/nonlinear_recipe/enet_nonlinear_tuned.rda"))    
load(here("results/nonlinear_recipe/lm_fit_folds_nonlinear.rda"))  
load(here("results/nonlinear_recipe/mars_nonlinear_tuned.rda"))
load(here("results/nonlinear_recipe/null_fit_folds_nonlinear.rda"))
load(here("results/nonlinear_recipe/rf_nonlinear_tuned.rda"))   

load(here("results/outta_recipe/ann_outta_tuned.rda"))
load(here("results/outta_recipe/bt_outta_tuned.rda"))  
load(here("results/outta_recipe/enet_outta_tuned.rda"))    
load(here("results/outta_recipe/lm_fit_folds_outta.rda"))  
load(here("results/outta_recipe/mars_outta_tuned.rda"))
load(here("results/outta_recipe/null_fit_folds_outta.rda"))
load(here("results/outta_recipe/rf_outta_tuned.rda"))   


################################################################################
################################################################################
################################################################################

rbind(
null_fit_folds |> 
  collect_metrics() |> 
  mutate(model = 'Null', .before = .metric),

lm_fit_folds |> 
  collect_metrics() |> 
  mutate(model = 'OLS', .before = .metric)
) |> 
  select(-c(.estimator, .config)) |> 
  filter(.metric == 'rmse')

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
  
enet_outta_tuned |> show_best(metric = 'rmse')
