########################################################################
########################################################################
#######                                                         ########
######      DATA QUALITY CHECK / TARGET VARIABLE EXAMINATION     #######
#######                                                         ########
########################################################################
########################################################################

#####################
##### Libraries #####
#####################

library(tidyverse)
library(here)
library(patchwork)
library(pracma)

#####################
##### Data Sets #####
#####################

nba_seasons <- read_rds(here('data/nba_seasons.rds'))

################################################################################
################################################################################
################################################################################

## Data Quality Check ##

nba_seasons |> 
  naniar::miss_var_summary() |> 
  DT::datatable(caption = 'Missingness Summary in nba_season')

nba_seasons |> 
  filter(is.na(x3p_percent)) |> 
  DT::datatable(caption = 'Season Statistics of Players with x3p_percent = NA')

nba_seasons |> 
  filter(is.na(ft_percent)) |> 
  slice_head(n = 10) |> 
  DT::datatable()

## Univariate predictor analysis ##

# adjusted salaries histogram
nba_seasons |> 
  ggplot(aes(adj_salary)) + 
  geom_histogram(bins = 120, color = 'white') +
  scale_x_continuous(limits = c(0, 63095401)) +
  theme_minimal() +
  labs(title = 'Histogram Distribution of Adjusted Annual NBA Salaries',
       subtitle = 'Most salaries are less than 5 million dollars',
       x = 'Adjusted Salary',
       y = '')

salary_desnsity_plot <-
  nba_seasons |> 
  ggplot(aes(adj_salary)) +
  geom_density() +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

salary_box_plot <-
  nba_seasons |> 
  ggplot(aes(adj_salary)) +
  geom_boxplot() +
  theme_void()

# density and boxplot of adjusted salaries
(salary_box_plot +
    labs(
      title = 'Density Distribution and Boxplot of Adjusted Salaries',
      subtitle = 'These graphs give a better understanding of where the middle-50 lies'
    ))/salary_desnsity_plot +
  plot_layout(heights = unit(c(1, 5), c('cm', 'null'))) 


# transforming skew by log base 10
(salary_box_plot + 
  labs(
    title = 'Density Distribution and Boxplot of Adjusted Salaries',
    subtitle = 'log10 transformation'
  ))/salary_desnsity_plot +
  plot_layout(heights = unit(c(1, 5), c('cm', 'null'))) &
  scale_x_log10(name = 'log10 adj_salary') 

## trying an alternate transformation ##

salary_desnsity_plot_root <-
  nba_seasons |> 
  ggplot(aes(nthroot(adj_salary, 7))) +
  geom_density() +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

salary_box_plot_root <-
  nba_seasons |> 
  ggplot(aes(nthroot(adj_salary, 7))) +
  geom_boxplot() +
  theme_void()

# density and boxplot of adjusted salaries, 7th root transformation
(salary_box_plot_root +
    labs(
      title = 'Density Distribution and Boxplot of Adjusted Salaries',
      subtitle = 'Root-7 transformation'))/salary_desnsity_plot_root +
  plot_layout(heights = unit(c(1, 5), c('cm', 'null'))) 

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
