#########################################
#########################################
#######                          ########
######      TRAINING DATA EDA     #######
#######                          ########
#########################################
#########################################

#####################
##### Libraries #####
#####################

library(tidyverse)
library(here)
library(flextable)

#####################
##### Data Sets #####
#####################

nba_seasons_train <- read_rds(here('data/splits_folds/nba_seasons_train.rds'))

################################################################################
################################################################################
################################################################################

  cor(nba_seasons_train |> 
        select(c(adj_salary, mp:ws)))



simple_density <- function(var){
  
  estdens <- density(unlist(nba_seasons_train |> select({{var}}) |>  filter(!is.na({{var}}))))
  estquant <- quantile(nba_seasons_train |> select({{var}}), probs = probs <- c(0, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE)
  
  df_dens <- data.frame(
    x = estdens$x,
    y = estdens$y,
    quantfct = factor(findInterval(estdens$x, estquant))
  )
  
  dens_plot <- 
  ggplot(data = df_dens)+
    geom_line(aes(x = x, y = y))+
    geom_ribbon(aes(x = x, ymin = 0, ymax = y, fill = quantfct),
      alpha = .7)+
    scale_x_continuous(breaks = estquant)+
    scale_fill_brewer(palette = 'BuPu') +
    labs(
      title = rlang::englue("Density Distribution of {{var}}"),
      y = '',
      x = rlang::englue('{{var}}'))+
    theme_bw()+
    theme(
      axis.text.x = element_text(angle = 45),
      panel.grid = element_blank(),
      legend.position = "none"
    )

  quantiles_tibble <- as_tibble(
    quantile(nba_seasons_train |> select({{var}}), probs = c(0, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE)) |> 
    mutate(quantiles = c('0%', '25%', '50%', '75%', '90%', '100%'), 
           .before = value)
  
  quantiles_table <- grid::rasterGrob(
    as_raster(flextable(quantiles_tibble)),
    interpolate = TRUE,
    width = unit(2, 'in')
  )
  
  quantiles_image <- 
    ggplot() +
    theme_void() +
    annotation_custom(quantiles_table)
  
  cowplot::plot_grid(dens_plot, quantiles_image, rel_widths = c(2.5,1))

}

simple_density(x3p_percent)

nba_seasons_train |> 
  ggplot(aes(x3p)) +
  geom_density()
