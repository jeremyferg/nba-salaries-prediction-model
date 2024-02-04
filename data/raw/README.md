## Raw Datasets

This folder contains the various datasets used to create the rds files that are directly in the `data` folder.

### Salaries

Datasets used to create `nba_salaries.rds`.

- `nba_salaries_2020_2022.rds` --> web-scraped salary data from 2020-2022, collected from [ESPN](https://www.espn.com/nba/salaries)

- `nba_salaries_1996_2019.xlsx` --> salary data from 1996-2019, original dataset by ["patrick" from Kaggle](https://www.kaggle.com/datasets/patrickhallila1994/nba-data-from-basketball-reference)

- `nba_salaries_1990_2017.csv` --> salary data from 1990-2017, original dataset by ["Fernando Blanco" from Kaggle](https://www.kaggle.com/datasets/whitefero/nba-player-salary-19902017?resource=download)

### Season Statistics

Datasets used to create `nba_stats.rds`. All season statistics data comes from [Stathead Basketball’s querying system](https://stathead.com/basketball/). 

- `nba_stats_raw.csv` --> season statistics from players from 1990-2022

- `player_made_playoffs.csv` --> season statistics of players whose team made the playoffs in a given season. Used to create the variable `playoffs`

- `1990_year_in_nba.csv` --> season statistics of players who were in the NBA in 1990, ordered by how many years the player had been in the NBA prior to 1990. Used to calculate the variable `years_in_league`

Please note that the nature of Stathead's querying system did not make it possible for me to find `playoffs` and `years_in_league` using only `nba_stats_raw.csv`.

### Misc.

- `cpi_data.csv` --> Consumer Price Index for All Urban Consumers from the [US Bureau of Labor Statistics.](https://www.bls.gov/data/inflation_calculator.htm). Used in `nba_seasons.rds` to create the target variable, `adj_salary`
