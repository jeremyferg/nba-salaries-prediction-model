## Datasets

This folder contains cleaned versions of NBA salaries and NBA season statistics datasets. The merging of these two files creates the final dataset. `raw`, which holds the raw data used to create these datasets, and `splits_folds`, which holds the training, testing, and fold data, is also located in this folder.

- `nba_salaries.rds` --> yearly salary data of NBA players from 1990-2022

- `nba_stats.rds` --> season statistics of  NBA players from 1990-2022

- `nba_seasons.rds` --> merging of the previous two files and is the main dataset of interest

The code book for `nba_seasons.rds` is also in this folder. Some of the variables may benefit for external resources for interpretation:

- [NBA team abbrevatrions](https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations)

- [True shooting percentage](https://resources.wolframcloud.com/FormulaRepository/resources/Basketball-True-Shooting-Percentage#:~:text=The%20true%20shooting%20percentage%20is,times%20the%20free%20throws%20attempted.)

- [Effective Field Goal Percentage](https://resources.wolframcloud.com/FormulaRepository/resources/Basketball-Effective-Field-Goal-Percentage#:~:text=The%20effective%20field%20goal%20percentage%20equals%20the%20sum%20of%20the,divided%20by%20field%20goals%20attempted.)
