library(datavolley)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

load("aggregate_plays.RData")
load("aggregate_plays_fab.RData")


lineups_by_hit <- function(aggregate_plays, split = FALSE) {
  hits <- aggregate_plays %>%
    filter(skill == "Attack") %>%
    mutate(
      attack_value = case_when(
        evaluation == "winning attack" ~ 1,
        evaluation %in% c("error", "blocked") ~ -1,
        TRUE ~ 0
      ),
      attack_value_home = if_else(team == home_team, attack_value, 0),
      attack_value_visiting = if_else(team != home_team, attack_value, 0),
      hit_id = row_number()
    )
  
  # Plus minus tabulation ---------------------------------------------------
  
  
  
  lineup_hit_plusminus_offsplit <- hits %>%
    transmute(
      match_id, 
      point_id,
      hit_id,
      home_team,
      visiting_team,
      serving_team,
      team,
      offense_team = ifelse(team == home_team, "home", "visiting"),
      attack_value_home,
      attack_value_visiting,
      value = if (split) {
        ifelse(offense_team == "home", attack_value_home, attack_value_visiting)
      } else {
        ifelse(offense_team == "home", attack_value_home, -attack_value_visiting)
      },
      sorted_home = map(pmap(select(., starts_with("home_player_id")), ~ sort(c(...))),
                        ~ set_names(.x, paste0("h_", seq_along(.x)))),
      sorted_visit = map(pmap(select(., starts_with("visiting_player_id")), ~ sort(c(...))),
                         ~ set_names(.x, paste0("v_", seq_along(.x))))
    ) %>% 
    unnest_wider(sorted_home) %>%
    unnest_wider(sorted_visit)
  
  
  # Offensive and defensive splits - only used for segment_id labeling ------------------------------------------
  
  offensive_splits <- lineup_hit_plusminus_offsplit %>%
    group_by(across(h_1:v_6)) %>%
    summarise(
      offense_team = "home",
      offensive_players = list(c(h_1, h_2, h_3, h_4, h_5, h_6)),
      defensive_players = list(c(v_1, v_2, v_3, v_4, v_5, v_6)),
      successes = sum(attack_value_home == 1, na.rm = TRUE),
      errors = sum(attack_value_home == -1, na.rm = TRUE),
      attempts = sum(team == home_team, na.rm = TRUE),
      efficiency = if_else(attempts > 0, (successes - errors) / attempts, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(segment_id = row_number())
  
  defensive_splits <- lineup_hit_plusminus_offsplit %>%
    group_by(across(h_1:v_6)) %>%
    summarise(
      offense_team = "visiting",
      offensive_players = list(c(v_1, v_2, v_3, v_4, v_5, v_6)),
      defensive_players = list(c(h_1, h_2, h_3, h_4, h_5, h_6)),
      successes = sum(attack_value_visiting == 1, na.rm = TRUE),
      errors = sum(attack_value_visiting == -1, na.rm = TRUE),
      attempts = sum(team == visiting_team, na.rm = TRUE),
      efficiency = if_else(attempts > 0, (successes - errors) / attempts, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(segment_id = row_number() + nrow(offensive_splits))
  
  all_hitting_splits_offsplit <- bind_rows(offensive_splits, defensive_splits)
  
  # Segment definition ------------------------------------------------------
  
  if(split){
    off_term <- "OFF|"
    def_term <- "DEF|"
  } else{
    off_term <- ""
    def_term <- ""
  }
  
  hit_based_segments <- lineup_hit_plusminus_offsplit %>%
    left_join(
      all_hitting_splits_offsplit %>%
        select(segment_id, offense_team, h_1:v_6),
      by = c(paste0("h_", 1:6), paste0("v_", 1:6), "offense_team")
    ) %>%
    mutate(
      across(starts_with("h_"), ~ ifelse(offense_team == "home", paste0(off_term, .), paste0(def_term, .))),
      across(starts_with("v_"), ~ ifelse(offense_team == "visiting", paste0(off_term, .), paste0(def_term, .)))
    )
  return(hit_based_segments)
}

temp <- hit_based_segments %>%
  count(value)

hit_based_segments <- lineups_by_hit(aggregate_plays, split = TRUE)
hit_based_segments_net <- lineups_by_hit(aggregate_plays, split = FALSE)

hit_based_segments_fab <- lineups_by_hit(aggregate_plays_fab, split = TRUE)
hit_based_segments_net_fab <- lineups_by_hit(aggregate_plays_fab, split = FALSE)

save_fun(hit_based_segments)
save_fun(hit_based_segments_net)
save_fun(hit_based_segments_fab)
save_fun(hit_based_segments_net_fab)
