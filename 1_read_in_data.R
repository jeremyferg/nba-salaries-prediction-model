

#####################
##### Libraries #####
#####################

library(tidyverse)
library(rvest)

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


read_html('https://www.basketball-reference.com/leagues/NBA_2023.html') |> 
  html_nodes('div') 


##############################################################################################
#read_html('https://stathead.com/basketball/player-season-finder.cgi?request=1&draft_pick_type=overall&comp_type=reg&order_by=ws&match=player_season&season_start=1&year_max=1992&display_type=totals&season_end=-1&comp_id=NBA&year_min=1991&offset=200') |> 
#  html_nodes('table') |> 
#  html_elements('tbody') |> 
#  html_elements('tr')

test <- read_csv('data/nba_stats.csv')

test2 <- read_csv('data/advanced_made_playoffs.csv')

test3 <- read_csv('data/1990_year_in_nba.csv')

test4 <- read_csv('data/nba_stats.csv')

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

#test3 |> 
#  janitor::clean_names() |> 
#  mutate(debut_year = debuts) |> 
#  select(player, age, debut_year)

test <- 
test |> 
  janitor::clean_names()

test2 <-
  test2 |> 
  janitor::clean_names()

#adding the playoffs column
test <-
test |> 
  mutate(dummy_team = team) |> 
  left_join(
    test2 |> 
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


first_season_tibble <- tibble()

for(i in unique(test$player) ){
  
 
first_season <- 
  test |> 
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


######### trying to fix the debut problems
test <-
test |> 
  left_join(first_season_tibble, join_by(player, rk)) |> 
  relocate(season.y, .after = season.x) |> 
  mutate(season.x = as.numeric(str_extract(season.x, '^....')),
         season.y = as.numeric(str_extract(season.y, '^....'))) |> 
  rename(season = season.x,
         rookie_season = season.y) |> 
  #relocate(years_in_league, .after = season.y) |> 
  left_join(
    test3 |> 
      janitor::clean_names() |> 
      mutate(years_in_league = debuts) |> 
      select(player, team, years_in_league),
    join_by(player)
  ) |> 
  mutate(years_in_league = if_else(is.na(years_in_league),
                              season - rookie_season + 1,
                              years_in_league + (season - rookie_season)),
         five_years = if_else(years_in_league > 5, 1, 0),
         ten_years = if_else(years_in_league > 10, 1, 0),
         player = if_else(str_detect(player, '_\\d$'), str_remove(player, '_\\d$'), player)) |> 
  relocate(c(years_in_league, five_years, ten_years), .after = rookie_season) |> 
  rename(team = team.x) |> 
  mutate(team = case_when(team == 'NOH' ~ 'NOP',
                          team == 'SEA' ~ 'OKC',
                          team == 'NJN' ~ 'BRK',
                          team == 'CHH' ~ 'CHA',
                          team == 'NOK' ~ 'NOP',
                          team == 'CHO' ~ 'CHA',
                          team == 'WSB' ~ 'WAS',
                          team == 'VAN' ~ 'MEM',
                          .default = team),
         conference = if_else(team %in% c('BOS', 'MIL', 'NYK', 'CLE', 'PHI', 
                                          'IND', 'MIA', 'ORL', 'CHI', 'ATL',
                                          'BRK', 'TOR', 'CHA', 'WAS', 'DET'),
                              'east', 'west'),
         market_size = case_when(team %in% c('GSW', 'NYK', 'LAL', 'BOS', 'LAC', 'CHI', 'DAL', 'HOU', 'PHI', 'TOR')
                                 ~ 'large',
                                 team %in% c('PHO', 'MIA', 'BRK', 'WAS', 'DEN', 'CLE', 'SAC', 'ATL', 'SAS', 'MIL')
                                 ~ 'medium',
                                 team %in% c('UTA', 'POR', 'DET', 'OKC', 'CHA', 'ORL', 'IND', 'NOP', 'MIN', 'MEM')
                                 ~ 'small')
         ) |> 
  select(-c(rookie_season, team.y)) |> 
  relocate(c(conference, market_size), .after = team)

##########################################################################################
##########################################################################################
test |> 
  distinct(team.x) |> 
  print(n = 40)

test |> 
  filter(team.x == 'WSB')

test4 |> 
  summarise(.by = c(player),
            n = n(),
            league = max(years_in_league),
            debut = min(years_in_league)) |> 
  arrange(desc(league), n) |>  
  filter(n != league) |> 
  print(n = 30)

test4 |> 
  filter(is.na(years_in_league))

test4 |> 
  filter(player == 'Charles Smith')

test4 |> 
  filter(player == 'Reggie Williams') |> 
  arrange(season) 

test |> 
  filter(player == 'Michael Jordan') |> 
  select(c(player, season))
  arrange(season) |> 
  slice_head()
  
test4 |> 
  filter(player == 'Mike James') |> 
  arrange(season) |> 
  print(n = 30)

test5 <-  
test4 |> 
  mutate(debut_age = age - years_in_league, .after = age)

atl_playoffs <- c(1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 
                  2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 
                  2018, 2019, 2020, 2021, 2022, 2023)
bos_playoffs <- c(1990, 1991, 1992, 1993, 1995, 2002, 2003, 2004, 2005, 2008, 2009, 
                  2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018, 2019, 2020, 
                  2021, 2022, 2023)
blk_playoffs <- c(1991, 1992, 1993, 1994, 1998, 2002, 2003, 2004, 2005, 2006, 
                  2007, 2013, 2014, 2015, 2019, 2020, 2021, 2022, 2023)
cha_playoffs <- c(1993, 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2009, 2010, 2014, 
                  2016)
chi_playoffs <- c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2005, 2006, 2007, 2009, 
                  2010, 2011, 2012, 2013, 2014, 2017, 2022)
cle_playoffs <- c(1990, 1992, 1993, 1994, 1995, 1996, 1998, 2006, 2007, 2008, 
                  2009, 2010, 2015, 2016, 2017, 2018, 2020)
det_playoffs <- c(1990, 1991, 1992, 1996, 1997, 1999, 2000, 2001, 2002, 2003, 
                  2004, 2005, 2006, 2007, 2008, 2009, 2016, 2019)
ind_playoffs <- c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 
                  2001, 2002, 2003, 2004, 2005, 2006, 2011, 2012, 2013, 2014, 2016, 
                  2017, 2018, 2019)
mia_playoffs <- c(1992, 1993, 1994, 1996, 1997, 1998, 1999, 2000, 2001, 2004, 
                  2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 
                  2016, 2018, 2020, 2021)
mil_playoffs <- c(1990, 1991, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2010, 2013, 
                  2015, 2018, 2019, 2020, 2021)
nyk_playoffs <- c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 
                  2001, 2004, 2011, 2012, 2013)
orl_playoffs <- c(1994, 1995, 1996, 1997, 1999, 2001, 2002, 2003, 2007, 2008, 
                  2009, 2010, 2019)
phi_playoffs <- c(1990, 1991, 1999, 2000, 2001, 2002, 2003, 2005, 2008, 2009, 
                  2011, 2012, 2018, 2019, 2020, 2021)
tor_playoffs <- c(2000, 2001, 2002, 2007, 2008, 2014, 2015, 2016, 2017, 2018, 
                  2019, 2020, 2021)
was_playoffs <- c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2005, 2006, 2007, 2008, 
                  2014, 2015, 2017, 2018)



dal_playoffs <- c(1990, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 
                  2010, 2011, 2012, 2014, 2015, 2016, 2018, 2019, 2020, 2021)
den_playoffs <- c(1990, 1994, 1995, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
                  2011, 2012, 2019, 2020, 2021)
gsw_playoffs <- c(1991, 1992, 1994, 2007, 2013, 2014, 2015, 2016, 2017, 2018, 
                  2019, 2020, 2021)
hou_playoffs <- c(1990, 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2004, 2005, 
                  2007, 2008, 2009, 2010, 2013, 2014, 2015, 2016, 2017, 2018, 
                  2019, 2020, 2021)
lac_playoffs <- c(1992, 1993, 1997, 2006, 2012, 2012, 2014, 2015, 2016, 2017, 
                  2019, 2020, 2021)
lal_playoffs <- c(1990, 1991, 1992, 1993, 1995, 1996, 1997, 1998, 1999, 2000, 
                  2001, 2002, 2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011, 
                  2012, 2013, 2018, 2020)
mem_playoffs <- c(2004, 2005, 2006, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
min_playoffs <- c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2018)
nop_playoffs <- c(2002, 2003, 2008, 2009, 2011, 2015, 2018)
okc_playoffs <- c(1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2002, 2005, 2010, 
                  2011, 2012, 2013, 2014, 2016, 2017, 2018)
pho_playoffs <- c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 
                  2002, 2003, 2005, 2006, 2007, 2008, 2010, 2011, 2013, 2021)
por_playoffs <- c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 
                  2003, 2009, 2010, 2011, 2014, 2015, 2016, 2018, 2019, 2020, 2021)
sac_playoffs <- c(1996, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006)
sas_playoffs <- c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 2001, 
                  2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                  2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
uta_playoffs <- c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 
                  2003, 2007, 2008, 2009, 2010, 2011, 2012, 2017, 2018, 2019, 
                  2020, 2021)








