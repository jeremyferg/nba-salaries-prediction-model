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

# metrics set
seasons_metrics <- metric_set(rmse, mae, rsq)

# predicting  
seasons_test_res <- predict(final_fit, nba_seasons_test)

# binding prediction with actual value
seasons_test_res <- 
  bind_cols(seasons_test_res, nba_seasons_test) |> 
  select(adj_salary, .pred)

# transforming back to original
seasons_test_res_og <- 
  seasons_test_res |> 
  mutate(adj_salary = adj_salary**7,
         .pred = .pred**7)

nba_seasons_test_og <- 
  nba_seasons_test |> 
  mutate(adj_salary = adj_salary**7)

# metric set tibble
seasons_metrics(seasons_test_res, truth = adj_salary, estimate = .pred)

seasons_metrics(seasons_test_res_og, truth = adj_salary, estimate = .pred)

# model plotting
## transformed
ggplot(bind_cols(seasons_test_res |> select(.pred), nba_seasons_test), aes(x = adj_salary, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.1) + 
  theme_bw() +
  labs(
    title = 'Observed Versus Predicted Values for Random Forest Model',
    subtitle = '7th Root Transformed',
    y = "Predicted Salaries", 
    x = "Salaries") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred() 

## original
ggplot(bind_cols(seasons_test_res_og |> select(.pred), nba_seasons_test_og), aes(x = adj_salary, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.1) + 
  theme_bw() +
  labs(
    title = 'Observed Versus Predicted Values for Random Forest Model',
    subtitle = 'Original Values',
    y = "Predicted Salaries", 
    x = "Salaries") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred() 


