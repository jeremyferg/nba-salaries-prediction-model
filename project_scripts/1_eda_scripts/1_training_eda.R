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
library(corrr)

#####################
##### Data Sets #####
#####################

nba_seasons_train <- read_rds(here('data/splits_folds/nba_seasons_train.rds'))

################################################################################
################################################################################
################################################################################

## looking into high correlation ##

  cor(nba_seasons_train |> 
        select(c(adj_salary, mp:ws)))

# correlation example
nba_seasons_train |> 
  select(mp, fg, fga) |> 
  correlate() |> 
  knitr::kable()


# Compute correlation matrix
correlation_matrix <- cor(nba_seasons_train |> 
                            select(c(adj_salary, mp:ws)))

# Extract upper triangle of correlation matrix (excluding the diagonal)
upper_triangle <- upper.tri(correlation_matrix)

# Set a threshold for correlation coefficient
threshold <- 0.8

# Find pairs of highly correlated variables
high_correlation <- which(correlation_matrix > threshold & upper_triangle, arr.ind = TRUE)

# Print pairs of highly correlated variables
for (i in 1:nrow(high_correlation)) {
  row_index <- high_correlation[i, 1]
  col_index <- high_correlation[i, 2]
  cat("Variables:", rownames(correlation_matrix)[row_index], "and", colnames(correlation_matrix)[col_index], "\n")
  cat("Correlation coefficient:", correlation_matrix[row_index, col_index], "\n\n")
}


## density plot for numeric variables ##

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

simple_density(e_fg_percent)


### scatterplot for numeric variables and target  variable 

simple_scatter <- function(some_var, interact, two = FALSE){
  
  if(two == TRUE){
  
  nba_seasons_train |> 
    ggplot(aes({{some_var}}, adj_salary)) +
    geom_point(alpha = .1) +
    geom_smooth(aes(color = {{interact}}), se = FALSE, linewidth = 1.5) +
    theme_bw() +
    labs(
      title = rlang::englue("Scatterplot of adj_salary against {{some_var}}"),
      subtitle = rlang::englue("Grouped by {{interact}}"),
      y = '')
  
  }
  
  else{
    
    nba_seasons_train |> 
      ggplot(aes({{some_var}}, adj_salary)) +
      geom_point(alpha = .1) +
      geom_smooth(se = FALSE, linewidth = 1.5) +
      theme_bw() +
      labs(
        title = rlang::englue("Scatterplot of adj_salary against {{some_var}}"),
        y = '')
    
  }
}

simple_scatter(x3p_percent, all_star, TRUE)
simple_scatter(x2p_percent)

### bar plot distributions

simple_barplot <- function(some_var){
  
  nba_seasons_train |> 
    
    ggplot(aes({{some_var}}, fill = {{some_var}})) +
    geom_bar() +
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white", fontface = "bold") +
    theme_bw() +
    labs(
      title = rlang::englue("Bar plot distribution of {{some_var}}"),
      y = '')
    #theme(legend.position = 'none')
  
}

simple_barplot(pos)


## box plot for predictor-outcome relationships

simple_boxplot <- function(some_var, interact, two = FALSE){
  
  if(two == TRUE){
  
    nba_seasons_train |> 
      ggplot(aes({{some_var}}, adj_salary, fill = {{interact}})) +
      geom_boxplot() +
      theme_bw() +
      labs(
        title = rlang::englue("Bar plot distribution of {{some_var}}"),
        subtitle = rlang::englue("Grouped by {{interact}}"),
        y = '')
    
  }
  else{
    
    nba_seasons_train |> 
      ggplot(aes({{some_var}}, adj_salary, fill = {{some_var}})) +
      geom_boxplot() +
      theme_bw() +
      labs(
        title = rlang::englue("Bar plot distribution of {{some_var}}"),
        y = '')
    
  }
  
}

simple_boxplot(pos, all_star, TRUE)


nba_seasons_train |> 
  ggplot(aes(all_star, adj_salary, fill = market_size)) +
  geom_boxplot() +
  theme_bw() 



## finding the best nonlinear trends ##

### tried....
# orb
# ast

nba_seasons_train |> 
  ggplot(aes(ft_percent, adj_salary)) +
  geom_point(alpha = .1) +
  geom_smooth(
    method = 'lm',
    formula = y ~ splines::bs(x, df = 5),
    se = FALSE,
    color = 'red'
  )

nba_seasons_train |> 
  ggplot(aes(orb, drb)) +
  geom_point(alpha = .1) +
  geom_smooth()


