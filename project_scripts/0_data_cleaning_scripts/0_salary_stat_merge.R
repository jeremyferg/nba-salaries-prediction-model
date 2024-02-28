##########################################
##########################################
#######                           ########
######      SALARY-STATS MERGE     #######
#######                           ########
##########################################
##########################################

#####################
##### Libraries #####
#####################

library(tidyverse)
library(here)

#####################
##### Data Sets #####
#####################

nba_stats <- read_rds(here('data/nba_stats.rds'))
nba_salaries <- read_rds(here('data/nba_salaries.rds'))
# consumer price index for all urban consumers from 1990 to 2023
cpi_data <- read_csv(here('data/cpi_data.csv'))

################################################################################
################################################################################
################################################################################

# to find the adjusted salaries, I want to use the CPIs recorded in October of a
##season. October was chosen because this is the month the NBA season starts
cpi_data <-
cpi_data |> 
  janitor::clean_names() |> 
  select(c(year, oct)) |> 
  rename(season = year,
         cpi = oct)

# merging NBA salaries and season stats data
nba_seasons <-
  nba_stats |> 
  inner_join(nba_salaries, join_by(player, season)) |> 
  relocate(c(pos, salary), .after = team) 

# making some variables factors 
nba_seasons <-
nba_seasons |> 
  inner_join(cpi_data, join_by(season)) |> 
  mutate(five_years = factor(five_years),
         ten_years = factor(ten_years),
         as = factor(as),
         conference = factor(conference),
         market_size = factor(market_size),
         pos = factor(pos),
         playoffs = factor(playoffs),
         
         player = if_else(str_detect(player, '_\\d$'), str_remove(player, '_\\d$'), player),
         # equation for finding the adjusted salary
         adj_salary = floor((salary)/(cpi/307.671))
         )|> 
  select(-c(rk, cpi)) |> 
  relocate(ws, .after = e_fg_percent) |> 
  relocate(adj_salary, .after = salary)
  
# save the dataset
write_rds(nba_seasons, here('data/nba_seasons.rds'))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
