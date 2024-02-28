## Data Cleaning Scripts

This folder contains the scripts used to clean the data that ultimately resulted in the main dataset, `nba_seasons.rds`. The datasets themselves can be found under `data\raw`.

- `0_nba_stats_wrangling.R` --> cleaning NBA season statistics data, ultimately forming `nba_stats.rds`

- `0_salary_wrangling.R` --> cleaning yearly NBA salaries data, ultimately forming `nba_salaries.rds`

- `0_salary_wrangling.R` --> merging the previously-mentioned scripts, ultimately forming `nba_seasons.rds`

