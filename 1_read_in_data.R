

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



##############################################################################################
read_html('https://stathead.com/basketball/player-season-finder.cgi?request=1&draft_pick_type=overall&comp_type=reg&order_by=ws&match=player_season&season_start=1&year_max=1992&display_type=totals&season_end=-1&comp_id=NBA&year_min=1991&offset=200') |> 
  html_nodes('table') |> 
  html_elements('tbody') |> 
  html_elements('tr')

test <- read_csv('data/nba_stats.csv')

