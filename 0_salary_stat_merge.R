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

nba_stats <- read_rds('data/nba_stats.rds')
nba_salaries <- read_rds('data/nba_salaries.rds')
cpi_data <- read_csv('data/cpi.csv')


################################################################################

cpi_data <-
cpi_data |> 
  janitor::clean_names() |> 
  select(c(year, oct)) |> 
  rename(season = year,
         cpi = oct)


nba_seasons <-
  nba_stats |> 
  inner_join(nba_salaries, join_by(player, season)) |> 
  relocate(c(pos, salary), .after = team) 
  

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
         
         adj_salary = (salary)/(cpi/307.671)
         
         )|> 
  select(-c(rk, cpi)) |> 
  relocate(ws, .after = e_fg_percent) |> 
  relocate(adj_salary, .after = salary)
  


#|> 
  skimr::skim_without_charts()



