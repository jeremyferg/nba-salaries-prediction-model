########################################
########################################
#######                         ########
######      STATS WRANGLING      #######
#######                         ########
########################################
########################################

#####################
##### Libraries #####
#####################

library(tidyverse)
library(here)
library(stringi)

#####################
##### Data Sets #####
#####################

nba_stats <- read_csv('data/nba_stats_raw.csv')

# seasons when the player made the playoffs
made_playoffs <- read_csv('data/advanced_made_playoffs.csv')

# players that were playing in 1990
players_1990 <- read_csv('data/1990_year_in_nba.csv')

################################################################################
################################################################################

# this vector represents how many years a player who was in the NBA in 1990
##has played in the league. The order is based on `players_1990`
debuts <- c(rep(1, times = 73), 
            rep(2, times = 135 - 73),
            rep(3, times = 177 - 135),
            rep(4, times = 205 - 177),
            rep(5, times = 234 - 205),
            rep(6, times = 265 - 234),
            rep(7, times = 285 - 265),
            rep(8, times = 311 - 285),
            rep(9, times = 330 - 311),
            rep(10, times = 350 - 330),
            rep(11, times = 361 - 350),
            rep(12, times = 369 - 361),
            rep(13, times = 377 - 369),
            rep(14, times = 383 - 377),
            rep(15, times = 386 - 383),
            17)

#adding the playoffs column to nba_stats
nba_stats <-
nba_stats |> 
  janitor::clean_names() |> 
  mutate(dummy_team = team) |> 
  left_join(
    made_playoffs |> 
      janitor::clean_names() |> 
      select(c(player, age, team, season)),
    join_by(player, age, season)
  ) |> 
  relocate(team.y, .after = team.x) |>
  mutate(team.x = team.y,
         playoffs = if_else(is.na(team.x), 0, 1),
         team.x = if_else(is.na(team.x), dummy_team, team.x),
         team.x = str_extract(team.x, '^...')) |> 
  relocate(playoffs, .after = team.x) |> 
  select(-c(team.y, dummy_team)) |> 
  rename(team = team.x) |> 
  mutate(season = as.numeric(str_extract(season, '^....')),
         # these names are instances where there are two or more players with the same name
         ##distinguishing their season salaries here
         player = case_when((player == 'Gerald Henderson' & season %in% c(2009:2016)) ~ 'Gerald Henderson_0',
                            (player == 'Brandon Williams' & season == 2021) ~ 'Brandon Williams_0',
                            (player == 'Reggie Williams' & season %in% c(2009:2016)) ~ 'Reggie Williams_0',
                            (player == 'Patrick Ewing' & season == 2010) ~ 'Patrick Ewing_0',
                            (player == 'Chris Smith' & season == 2013) ~ 'Chris Smith_0',
                            (player == 'Dee Brown' & season %in% c(2006, 2008)) ~ 'Dee Brown_0',
                            (player == 'Charles Jones' & season %in% c(1998:1999)) ~ 'Charles Jones_0',
                            (player == 'Charles Smith' & season %in% c(1997:2005)) ~ 'Charles Smith_0',
                            (player == 'Charles Smith' & rk %in% c(13259, 14614)) ~ 'Charles Smith_1',
                            (player == 'Mike James' & season %in% c(2017, 2020)) ~ 'Mike James_0',
                            (player == 'Michael Smith' & season %in% c(1994:2000)) ~ 'Michael Smith_0',
                            .default = player))

# finding each players rookie season by filtering by players, arranging data by year,
##and extracting that year
first_season_tibble <- tibble()

for(i in unique(nba_stats$player) ){
  
 
first_season <- 
  nba_stats |> 
  filter(player == i) |> 
  select(c(rk, player, season)) |> 
  arrange(season) #|>
  #slice_head()
  #mutate(season = first_season$season[1]) 

first_season <-
  first_season |> 
  mutate(season = first_season$season[1])
  
  first_season_tibble <- bind_rows(first_season_tibble, first_season)
  
}

# making categorical predictors
nba_stats <-
nba_stats |> 
  # joining nba_stats data with the dataset that has the players' rookie year
  left_join(first_season_tibble, join_by(player, rk)) |> 
  relocate(season.y, .after = season.x) |> 
  mutate(season.x = as.numeric(str_extract(season.x, '^....')),
         season.y = as.numeric(str_extract(season.y, '^....'))) |> 
  rename(season = season.x,
         rookie_season = season.y) |> 
  left_join(
    # by left-joining, we have the number of years a player who played in 1990
    ##has been in the NBA
    players_1990 |> 
      janitor::clean_names() |> 
      mutate(years_in_league = debuts) |> 
      select(player, team, years_in_league),
    join_by(player)
  ) |> 
  # if `years_in_league` is NA, it means you were not in the NBA in 1990, meaning
  ##we can just use the rookie season variable found through `first_season_tibble`
  mutate(years_in_league = if_else(is.na(years_in_league),
                              season - rookie_season + 1,
                              years_in_league + (season - rookie_season)),
         five_years = if_else(years_in_league > 5, 1, 0),
         ten_years = if_else(years_in_league > 10, 1, 0)) |>  
  relocate(c(years_in_league, five_years, ten_years), .after = rookie_season) |> 
  rename(team = team.x) |> 
  # some teams have had name changes or location changes, conforming team names to
  ## modern abbreviations
  mutate(team = case_when(team == 'NOH' ~ 'NOP',
                          team == 'SEA' ~ 'OKC',
                          team == 'NJN' ~ 'BRK',
                          team == 'CHH' ~ 'CHA',
                          team == 'NOK' ~ 'NOP',
                          team == 'CHO' ~ 'CHA',
                          team == 'WSB' ~ 'WAS',
                          team == 'VAN' ~ 'MEM',
                          .default = team),
         # finding NBA conference
         conference = if_else(team %in% c('BOS', 'MIL', 'NYK', 'CLE', 'PHI', 
                                          'IND', 'MIA', 'ORL', 'CHI', 'ATL',
                                          'BRK', 'TOR', 'CHA', 'WAS', 'DET'),
                              'east', 'west'),
         # market size is determined by the value of the team, according to Forbes
         ##large market --> teams ranked 1 - 10
         ##medium market --> teams ranked 11 - 20
         ##small market --> teams ranked 21 - 30
         market_size = case_when(team %in% c('GSW', 'NYK', 'LAL', 'BOS', 'LAC', 'CHI', 'DAL', 'HOU', 'PHI', 'TOR')
                                 ~ 'large',
                                 team %in% c('PHO', 'MIA', 'BRK', 'WAS', 'DEN', 'CLE', 'SAC', 'ATL', 'SAS', 'MIL')
                                 ~ 'medium',
                                 team %in% c('UTA', 'POR', 'DET', 'OKC', 'CHA', 'ORL', 'IND', 'NOP', 'MIN', 'MEM')
                                 ~ 'small'),
         # salary data does not have special Latin letter such as à -- replace them
         ##in this dataset with standard letters
         player = stri_trans_general(player, 'Latin-ASCII'),
         player = str_replace_all(player, '\\.', ''),
         player = str_replace_all(player, "'", '')
         ) |> 
  select(-c(rookie_season, team.y, x9999)) |> 
  relocate(c(conference, market_size), .after = team)

# save the NBA stats data
write_rds(nba_stats, here('data/nba_stats.rds'))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
