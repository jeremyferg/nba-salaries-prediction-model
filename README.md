## Overview

This project aims to create a predictive model that can estimate the yearly salaries of professional basketball players from the National Basketball Association (NBA). This model will mainly use season statistics from players as its predictors. Other NBA-related factors, such as the location of the player’s team and how long the player has been in the NBA, will be used for this analysis.

The target variable is `adj_salary`, found in the `nba_seasons.rds` dataset. This variable measures a player’s yearly salary adjusted to 2023 prices using the [Consumer Price Index for All Urban Consumers from the US Bureau of Labor Statistics.](https://www.bls.gov/data/inflation_calculator.htm).

## Repo Layout

- `0_data_cleaning_scripts` --> folder containing the R scripts used to clean raw data and create the dataset of interest, `nba_seasons.rds`

- `1_eda_scripts` --> folder containing the R scripts used to perform exploratory data analysis on our main dataset

- `data` --> folder containing used datasets

- `memos` --> folder containing progress reports for this project


