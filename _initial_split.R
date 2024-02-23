#####################################
#####################################
#######                      ########
######      INITIAL SPLIT     #######
#######                      ########
#####################################
#####################################

### USING RANDOM SEED ###

#####################
##### Libraries #####
#####################

library(tidyverse)
library(tidymodels)
library(pracma)
library(here)

tidymodels_prefer()

#####################
##### Data Sets #####
#####################

nba_seasons <- read_rds(here('data/nba_seasons.rds'))

################################################################################
################################################################################
################################################################################

## log_transform adj_salary ##

nba_seasons_nth7 <-
  nba_seasons |> 
  mutate(adj_salary = nthroot(adj_salary, 7),
         # make predictors that should be a factor a factor
         five_years = factor(five_years),
         ten_years = factor(ten_years),
         pos = factor(pos),
         conference = factor(conference),
         market_size = factor(market_size, ordered = TRUE),
         playoffs = factor(playoffs),
         as = factor(as)) |> 
  rename(all_star = as) |> 
  filter(!is.na(x2p_percent))

## split data ##

# set seed
set.seed(0372)
nba_seasons_splits <-
  nba_seasons_nth7 |> 
  initial_split(prop = .75, strata = adj_salary)

nba_seasons_train <- nba_seasons_splits |> training()
nba_seasons_test <- nba_seasons_splits |> testing()

## folds ##

# this time around, let's keep it simple and use v-fold cross validation
# let's also say i'm more interested in a estimate with low bias (so high variance)
nba_seasons_folds <- vfold_cv(nba_seasons_train, v = 8, repeats = 5, strata = adj_salary)

# set up controls for fitting resamples
keep_wflow <- control_resamples(save_workflow = TRUE)

## save the split, train, and test ##

write_rds(nba_seasons_splits, file = here("data/splits_folds/nba_seasons_splits.rds"))
write_rds(nba_seasons_train, file = here("data/splits_folds/nba_seasons_train.rds"))
write_rds(nba_seasons_test, file = here("data/splits_folds/nba_seasons_test.rds"))
write_rds(nba_seasons_folds, file = here("data/splits_folds/nba_seasons_folds.rda"))
save(keep_wflow, file = here('results/keep_wflow.rda'))

