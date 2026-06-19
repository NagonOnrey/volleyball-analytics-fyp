library(datavolley)
library(dplyr)
library(tidyr)
library(purrr)

load("aggregate_plays.RData")
load("aggregate_plays_fab.RData")

lineups_by_point <- function(df, h_or_s) {
  point_end <- df %>%
    filter(point == TRUE)
  
  home_or_serving <- paste0(h_or_s, "_team")
  
  # Gets point_by_point lineups and results. 
  lineup_plusminus <- point_end %>%
    mutate(value = ifelse(point_won_by == .data[[home_or_serving]], 1, -1)) %>%
    transmute(
      point_id,
      match_id,
      serving_team, 
      home_team,
      visiting_team,
      value,
      sorted_home = map(pmap(select(., starts_with("home_player_id")), ~ sort(c(...))),
                        ~ set_names(.x, paste0("h_", seq_along(.x)))),
      sorted_visit = map(pmap(select(., starts_with("visiting_player_id")), ~ sort(c(...))),
                         ~ set_names(.x, paste0("v_", seq_along(.x))))
    ) %>% 
    unnest_wider(sorted_home) %>%
    unnest_wider(sorted_visit)
  
  # Gets all the splits and the points. 
  all_splits <- lineup_plusminus %>%
    group_by(across(h_1:v_6)) %>%
    summarise(
      net_margin = sum(value),
      num_points = n(),
      .groups = "drop"
    ) %>%
    mutate(segment_id = row_number())
  
  
  # Attaching the segment id to point_end.
  lineup_segments <- lineup_plusminus %>%
    left_join(all_splits %>% select(segment_id, h_1:v_6), by = c(paste0("h_", 1:6), paste0("v_", 1:6)))
  
  return(lineup_segments)
}

point_based_segments <- lineups_by_point(aggregate_plays, "home")
point_based_segments_fab <- lineups_by_point(aggregate_plays_fab, "home")

save_fun(point_based_segments)
save_fun(point_based_segments_fab)

quick_counts <- aggregate_plays %>%
  mutate(serving_wins = case_when(serving_team == point_won_by ~ TRUE,
                                  serving_team != point_won_by ~FALSE)) %>%
  select(serving_wins) %>%
  count(serving_wins)

m <- lm(value ~ I(ifelse(serving_team == home_team, 1, -1)), data = point_based_segments)
coef(m)[2]


point_based_segments %>%
  mutate(
    home_won = (value == 1),
    serving_is_home = (serving_team == home_team),
    serve_won = if_else(serving_is_home, home_won, !home_won)
  ) %>%
  summarise(
    serve_win_rate = mean(serve_won, na.rm = TRUE),
    home_serve_win = mean(serve_won[serving_is_home], na.rm = TRUE),
    away_serve_win = mean(serve_won[!serving_is_home], na.rm = TRUE),
    receive_win_rate = 1 - mean(serve_won, na.rm = TRUE)
  ) %>%
  mutate(expected_beta = (receive_win_rate - serve_win_rate))


