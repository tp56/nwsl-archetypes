library(tidyverse)
library(dplyr)
library(plotly)
library(GGally)
library(ggcorrplot)
library(factoextra)
library(ggrepel)
library(dendextend)
library(ggthemes)
library(ggradar)
library(mclust)
library(tidyr)

load_filtered_data <- function(file_path) {
  read.csv(file_path) |>
    select(player_id, player_position, player_match_name, games_played,
           season, team_id, team_club_name, team_abbreviation,
           key_passes, through_balls, touches, recoveries, interceptions, 
           tackled, tackles_won, tackles_lost, tackles_total,
           duels_won, duels_lost, duels_aerial, duels_ground,
           crosses_open_play_successful, successful_passes_total,
           aerial_duels_won, aerial_duels_lost, 
           total_fouls_won, fouls_conceded_total,
           passes_forward, passes_backwards, passes_unsuccessful,
           ground_duels_won, duels_ground_lost,
           loss_of_possession_total, short_passes_successful, 
           short_passes_unsuccessful,  shots_total, shots_on_target, 
           long_passess_successful, long_passes_unsuccessful, 
           total_fouls_won,  fouls_conceded_total) |>
    filter(season == 2024 & player_id != '' & games_played > 10 & team_club_name != '' & player_position != 'Goalkeeper')
}


plot_scatter <- function(df, x, y, focus_var_status = FALSE, focus_var = NULL) {
  if (focus_var_status && !is.null(focus_var)){
    p <- ggplot(df, aes(x = {{x}}, y = {{y}}, color = {{focus_var}})) +
      geom_point()
  } else {
    p <- ggplot(df, aes(x = {{x}}, y = {{y}})) +
      geom_point()
  }
  print(p)
  return(p)
}

plot_correlation <- function(df, cols){
  corr_matrix <- cor(dplyr::select(df, all_of(cols)))
  ggcorrplot(corr_matrix, type = "lower", method = "circle", hc.order = TRUE)
  return(p)
}


plot_parallel_coords <- function(df, cols, focus_variable, facet = FALSE, facet_var) {
  p <- ggparcoord(data = df, columns = cols,
                  alphaLines = 0.2, groupColumn = focus_variable,
                  scale = "uniminmax", order = "skewness")
  if (facet) {
    p <- p + facet_wrap(vars(facet_var)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(p)
}

plot_density <- function(df, plot_variable, fill_variable) {
  
  plot_sym <- sym(plot_variable)
  fill_sym <- sym(fill_variable)
  
  p <- ggplot(df, aes(x = !!plot_sym, fill = !!fill_sym))  +
    geom_density(alpha = 0.5) +
    labs(title = paste('Distribution of', plot_variable, 'by', fill_variable),
         x = plot_variable,
         caption = 'of players who player 10 or more games in 2024')
  return(p)
  
}

plot_pca <- function(df, variable_names, focus_var) {
  pca <- prcomp(dplyr::select(df, 
                              all_of(variable_names)))
  summary(pca)
  pca_matrix <- pca$x
  head(pca_matrix)
  
  pca_df <- as.data.frame(pca$x[, 1:2])
  colnames(pca_df) <- c("pc1", "pc2")
  df <- cbind(df, pca_df)
  
  p <- ggplot(data = df, aes(x = pc1, y = pc2, color = .data[[focus_var]])) +
    geom_point(alpha = 0.5) +
    labs(x = "Principal Component 1", y = "Principal Component 2")
  
  return(p)
}

plot_pca_shapes <- function(df, variable_names, focus_var) {
  pca <- prcomp(dplyr::select(df, 
                              all_of(variable_names)))
  summary(pca)
  pca_matrix <- pca$x
  head(pca_matrix)
  
  pca_df <- as.data.frame(pca$x[, 1:2])
  colnames(pca_df) <- c("pc1", "pc2")
  df <- cbind(df, pca_df)
  
  p <- ggplot(data = df, aes(x = pc1, y = pc2, shape = .data[[focus_var]], color = .data[[focus_var]])) +
    geom_point(alpha = 0.5) +
    labs(x = "PC 1", y = "PC 2")
  
  return(p)
}
get_pca_loadings <- function(df, variable_names){
  pca <- prcomp(dplyr::select(df, 
                              all_of(variable_names)))
  
  print(pca$rotation)
  num_components <- min(25, ncol(pca$rotation))
  loadings_df <- as.data.frame(pca$rotation[, 1:num_components])
  return(loadings_df)
}
