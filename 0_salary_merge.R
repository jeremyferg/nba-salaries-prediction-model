##################################
##################################
#######                    #######
######      STATS MERGE     ######
#######                    #######
##################################
##################################

#####################
##### Libraries #####
#####################

library(tidyverse)
library(rvest)
library(here)

#####################
##### Data Sets #####
#####################

nba_salaries_1990_2017 <- readxl::read_excel('data/nba_salaries_1990_2017.xlsx')
nba_salaries_1996_2019 <- read_csv('data/nba_salaries_1996_2019.csv')


### some webscraping for salaries on ESPN ###

get_salaries <- function(year = '2023'){
  
  salaries_df <- data.frame()
  salaries_page <- tibble(c(1))
  
  year_link <- paste0('https://www.espn.com/nba/salaries/_/year/', year, '/page/1')
  page_num <- 1
  
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

nba_salaries_2020_2022 <- tibble()

for(i in c('2021', '2022', '2023')){
  
  nba_salaries_2020_2022 <- rbind(get_salaries(i), nba_salaries_2020_2022)
  
}

write_rds(nba_salaries_2020_2022, here('data/nba_salaries_2020_2022.rds'))

## cleaning nba_salaries_2020_2022

nba_salaries <- 
  rbind(

nba_salaries_2020_2022 |> 
  janitor::clean_names() |> 
  select(-c(x1, x3)) |> 
  rename(
    player = x2,
    salary = x4,
    season = year
  ) |> 
  mutate(
    player = str_remove(player, ',\\s..?$'),
    salary = str_remove(salary, '\\$'),
    salary = as.double(str_replace_all(salary, ',', ''))
  ) |> 
  relocate(season, .after = player),

## cleaning nba_salaries_2018_2019

nba_salaries_1996_2019 |> 
  janitor::clean_names() |> 
  select(-inflation_adj_salary) |> 
  rename(
    player = player_name,
    season = season_start_year
    ) |>
  filter(season %in% c(2018, 2019)) |> 
  mutate(
    salary = str_remove(salary, '\\$'),
    salary = as.double(str_replace_all(salary, ',', ''))
  ),

## cleaning nba_salaries_1990_2017

nba_salaries_1990_2017 |> 
  janitor::clean_names() |> 
  select(-c(register_value, season_end, team, full_team_name)) |> 
  rename(
    player = player_name,
    salary = salary_in,
    season = season_start
  ) |> 
  relocate(season, .after = player)

)

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
                       .default = player))

write_rds(nba_salaries, here('data/nba_salaries.rds'))

#################################################################################

test <- read_rds('data/nba_salaries.rds')

test2 <-
test |> 
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
                       .default = player))
  
test2 |> 
  filter(player == 'Joe Dumars')

nba_salaries_2020_2022 |> 
  janitor::clean_names() |> 
  select(-c(x1, x3)) |> 
  rename(
    player = x2,
    salary = x4,
    season = year
  ) |> 
  mutate(
    player = str_remove(player, ',\\s..?$'),
    salary = str_remove(salary, '\\$'),
    salary = as.double(str_replace_all(salary, ',', ''))
  ) |> 
  relocate(season, .after = player) |> 
  filter(str_detect(player, 'Brandon'))

problem_1 <-
nba_stats |> 
  left_join(nba_salaries, join_by(player, season)) |> 
  relocate(salary, .after = team) |> 
  filter(is.na(salary)) |> 
  arrange(season)


nba_salaries2 <- 
  rbind(
    
    nba_salaries_2020_2022 |> 
      janitor::clean_names() |> 
      select(-c(x1, x3)) |> 
      rename(
        player = x2,
        salary = x4,
        season = year
      ) |> 
      mutate(
        player = str_remove(player, ',\\s..?$'),
        salary = str_remove(salary, '\\$'),
        salary = as.double(str_replace_all(salary, ',', ''))
      ) |> 
      relocate(season, .after = player),
    
    ## cleaning nba_salaries_2018_2019
    
    nba_salaries_1996_2019 |> 
      janitor::clean_names() |> 
      select(-inflation_adj_salary) |> 
      rename(
        player = player_name,
        season = season_start_year
      ) |>
      mutate(
        salary = str_remove(salary, '\\$'),
        salary = as.double(str_replace_all(salary, ',', ''))
      ),
    
    ## cleaning nba_salaries_1990_2017
    
    nba_salaries_1990_2017 |> 
      janitor::clean_names() |> 
      select(-c(register_value, season_end, team, full_team_name)) |> 
      rename(
        player = player_name,
        salary = salary_in,
        season = season_start
      ) |> 
      filter(season %in% c(1990:1995)) |> 
      relocate(season, .after = player)
    
  )

nba_salaries2 <-
  nba_salaries2 |> 
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
                       .default = player))

nba_stats |> 
  left_join(nba_salaries2, join_by(player, season)) |> 
  relocate(salary, .after = team) |> 
  filter(is.na(salary)) |> 
  arrange(season)
