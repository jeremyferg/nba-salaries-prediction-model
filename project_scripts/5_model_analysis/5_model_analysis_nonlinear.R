################################################
################################################
#######                                 ########
######      MODEL ANALYSIS NONLINEAR     #######
#######                                 ########
################################################
################################################


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

load(here("results/nonlinear_recipe/ann_nonlinear_tuned.rda"))
load(here("results/nonlinear_recipe/bt_nonlinear_tuned.rda"))  
load(here("results/nonlinear_recipe/enet_nonlinear_tuned.rda"))    
load(here("results/nonlinear_recipe/lm_fit_folds_nonlinear.rda"))  
load(here("results/nonlinear_recipe/mars_nonlinear_tuned.rda"))
load(here("results/nonlinear_recipe/null_fit_folds_nonlinear.rda"))
load(here("results/nonlinear_recipe/rf_nonlinear_tuned.rda"))   

################################################################################
################################################################################
################################################################################

# best performing results
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
  
  mars_nonlinear_tuned |> 
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

#bt_outta_tuned |> 
#  show_best(metric = 'rmse')

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
