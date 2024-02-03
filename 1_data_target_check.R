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

nba_seasons |> 
  #mutate(adj_salary = sqrt(adj_salary)) |> 
  ggplot(aes(adj_salary)) + 
  geom_histogram(bins = 120, color = 'white') +
  scale_x_continuous(limits = c(0, 63095401)) +
  theme_minimal() +
  labs(title = 'Histogram Distribution of Adjusted Annual NBA Salaries',
       subtitle = 'Most salaries are less than 5 million dollars',
       x = 'Adjusted Salary',
       y = '')
  

nba_seasons |> 
  skimr::skim_without_charts(adj_salary)

nba_seasons |> 
  naniar::miss_var_summary()

quantile(nba_seasons$adj_salary, c(.1,.2,.3,.4,.5,.6,.7,.8,.9))


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

salary_box_plot/salary_desnsity_plot +
  plot_layout(heights = unit(c(1, 5), c('cm', 'null')))

# transforming skew by log base 10
salary_box_plot/salary_desnsity_plot +
  plot_layout(heights = unit(c(1, 5), c('cm', 'null'))) &
  scale_x_log10(name = 'log10 price')


