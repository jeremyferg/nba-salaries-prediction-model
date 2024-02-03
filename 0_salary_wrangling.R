########################################
########################################
#######                         ########
######      SALARY WRANGLING     #######
#######                         ########
########################################
########################################

#####################
##### Libraries #####
#####################

library(tidyverse)
library(rvest)
library(here)
#library(stringi)

#####################
##### Data Sets #####
#####################

nba_salaries_1990_2017 <- readxl::read_excel(here('data/nba_salaries_1990_2017.xlsx'))
nba_salaries_1996_2019 <- read_csv(here('data/nba_salaries_1996_2019.csv'))


### webscraping function for salaries on ESPN ###

# since we are scraping a small number of sites, I decided to keep this function 
##in this R script (instead of making an individual script)
get_salaries <- function(year = '2023'){
  
  # where all salaries are stored
  salaries_df <- data.frame()
  
  #where salaries for individual years are stored
  salaries_page <- tibble(c(1))
  
  year_link <- paste0('https://www.espn.com/nba/salaries/_/year/', year, '/page/1')
  page_num <- 1
  
  # keep iterating through pages until you reach a page with an empty salary table
  ##indicating that we have collected all the salaries 
  while(nrow(salaries_page) != 0){
    
    salaries_page <- 
      read_html(year_link) |> 
      html_element('.span-4') |> 
      html_table() |> 
      filter(`X1` != 'RK') |> 
      mutate(year = as.double(year) - 1)
    
    salaries_df <- rbind(salaries_df, salaries_page)
    
    page_num <- page_num + 1
    
    year_link <- paste0('https://www.espn.com/nba/salaries/_/year/', year, '/page/', as.character(page_num))
    
  }
  
  return(salaries_df)
  
}

# implementing get_salaries()
nba_salaries_2020_2022 <- tibble()

for(i in c('2021', '2022', '2023')){
  
  nba_salaries_2020_2022 <- rbind(get_salaries(i), nba_salaries_2020_2022)
  
}

# saving nba_salaries_2020_2022
write_rds(nba_salaries_2020_2022, here('data/nba_salaries_2020_2022.rds'))

################################################################################
################################################################################
################################################################################

## merging datasets ##

# bind all datasets to a sigle salary dataset
nba_salaries <- 
  rbind(

# cleaning nba_salaries_2020_2022
nba_salaries_2020_2022 |> 
  janitor::clean_names() |> 
  select(-c(x1, x3)) |> 
  rename(
    player = x2,
    salary = x4,
    season = year
  ) |>
  # player names also had positions in their string, remove the position
  # change salaries from strings to numeric values
  mutate(
    player = str_remove(player, ',\\s..?$'),
    salary = str_remove(salary, '\\$'),
    salary = as.double(str_replace_all(salary, ',', ''))
  ) |> 
  relocate(season, .after = player),

# cleaning nba_salaries_1996_2019
nba_salaries_1996_2019 |> 
  janitor::clean_names() |>
  # I chose to create my own variable for adjusted salary prices, so this variable
  ##is dropped
  select(-inflation_adj_salary) |> 
  rename(
    player = player_name,
    season = season_start_year
    ) |>
  # change salaries from strings to numeric values
  mutate(
    salary = str_remove(salary, '\\$'),
    salary = as.double(str_replace_all(salary, ',', ''))
  ),

# cleaning nba_salaries_1990_2017
nba_salaries_1990_2017 |> 
  janitor::clean_names() |> 
  select(-c(register_value, season_end, team, full_team_name)) |> 
  rename(
    player = player_name,
    salary = salary_in,
    season = season_start
  ) |> 
  # i chose to only take the year observations missing from the previous two 
  ##datasets
  filter(season %in% c(1990:1995)) |>  
  relocate(season, .after = player)

)

# these names are instances where there are two or more players with the same name
##distinguishing their season salaries here
nba_salaries <-
  nba_salaries |> 
  mutate(
    player = case_when((player == 'Gerald Henderson' & season %in% c(2009:2016)) ~ 'Gerald Henderson_0',
                       (player == 'Brandon Williams' & season == 2021) ~ 'Brandon Williams_0',
                       (player == 'Reggie Williams' & season %in% c(2009:2016)) ~ 'Reggie Williams_0',
                       (player == 'Patrick Ewing' & season == 2010) ~ 'Patrick Ewing_0',
                       (player == 'Chris Smith' & season == 2013) ~ 'Chris Smith_0',
                       (player == 'Dee Brown' & season %in% c(2006, 2008)) ~ 'Dee Brown_0',
                       (player == 'Charles Jones' & season %in% c(1998:1999)) ~ 'Charles Jones_0',
                       (player == 'Charles Smith' & season %in% c(1997:2005)) ~ 'Charles Smith_0',
                       (player == 'Charles Smith' & salary == 225000) ~ 'Charles Smith_1',
                       (player == 'Mike James' & season %in% c(2017, 2020)) ~ 'Mike James_0',
                       (player == 'Michael Smith' & season %in% c(1994:2000)) ~ 'Michael Smith_0',
                       .default = player),
    player = str_replace_all(player, '\\.', ''),
    player = str_replace_all(player, "'", ''))

# save the final dataset
write_rds(nba_salaries, here('data/nba_salaries.rds'))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################