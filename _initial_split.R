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

nba_seasons_log <-
  nba_seasons |> 
  mutate(adj_salary = log10(adj_salary))

## split data ##

# set seed
set.seed(0372)
nba_seasons_splits <-
  nba_seasons_log |> 
  initial_split(prop = .75, strata = adj_salary)

nba_seasons_train <- nba_seasons_splits |> training()
nba_seasons_test <- nba_seasons_splits |> testing()

## save the split, train, and test ##

write_rds(nba_seasons_splits, file = here("data/splits_folds/nba_seasons_splits.rds"))
write_rds(nba_seasons_train, file = here("data/splits_folds/nba_seasons_train.rds"))
write_rds(nba_seasons_test, file = here("data/splits_folds/nba_seasons_test.rds"))

