library(datavolley)
library(dplyr)
library(tidyr)
library(stringr)

save_fun <- function(object){
  obj_name <- deparse(substitute(object))
  filename <- paste0(obj_name, ".RData")
  save(list = obj_name, file = filename)
}

load("aggregate_plays.RData")

lineups_by_touch <- function(dataframe, skl_inc, def_inc = FALSE, prev_act = FALSE, pres_hitters = FALSE) {

  touch_plays <- dataframe %>%
    filter(!is.na(player_id)) %>%
    group_by(team_touch_id, match_id) %>%
    mutate(touch_number = row_number(),
           skill_player = paste(skill, player_id, sep = "|")) %>%
    ungroup()
  
  if(skl_inc){player_used <- "skill_player"}
  else{player_used <- "player_id"}
  
  touch_players <- touch_plays %>% 
    mutate(touch_number = paste0("par_", touch_number)) %>%
    pivot_wider(
      id_cols = c(team_touch_id, match_id),
      names_from = touch_number,
      values_from = {{player_used}}
    ) 
  
  
  # Touch point value -------------------------------------------------------
  
  last_two_touches <- dataframe %>%
    filter(!is.na(skill) & skill != "Timeout") %>%
    group_by(point_id, match_id) %>%
    slice_tail(n = 2) %>%
    mutate(team_count = n_distinct(team)) %>%
    ungroup() %>%
    group_by(point_id, match_id) %>%
    mutate(row_num = row_number()) %>%
    ungroup() %>%
    mutate(
      value = case_when(
        team_count == 2 & team == point_won_by ~ 1,
        team_count == 2 & team != point_won_by ~ -1,
        team_count == 1 & team == point_won_by ~ 1,
        team_count == 1 & team != point_won_by ~ -1,
        TRUE ~ 0
      )
    ) %>%
    filter(value != 0)
  
  possession_outcomes <- dataframe %>% # Attaches this to the possession. 
    group_by(team_touch_id, match_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    left_join(
      last_two_touches %>% select(team_touch_id, value, match_id),
      by = c("team_touch_id", "match_id")
    ) %>%
    mutate(value = replace_na(value, 0)) %>%
    distinct(team_touch_id, match_id, .keep_all = TRUE) # Removes duplicates caused by penultimate and ultimate touch (i.e. no 'credited' error)
  
  # Segmenting --------------------------------------------------------------
  
  touch_based_segments_raw <- left_join(touch_players, possession_outcomes, by = c("team_touch_id", "match_id")) %>%
    mutate(overall_touch_id = row_number())
  
  # Defensive inclusion -----------------------------------------------------
  
  touch_based_segments <- touch_based_segments_raw
  
  if(def_inc){
    touch_based_segments <- touch_based_segments %>%
      rowwise() %>%
      mutate(defenders = list(
        if(team == home_team){paste0("DEF|", c_across(starts_with("visiting_player_id")))} 
        else {paste0("DEF|", c_across(starts_with("home_player_id")))}
      )) %>%
      ungroup() %>%
      unnest_wider(defenders, names_sep = "_") %>%
      rename_with(~ paste0("def_", seq_along(.)), starts_with("defenders_"))}
  else{touch_based_segments <- touch_based_segments_raw}

  if(pres_hitters){
    par_cols <- grep("^par_", colnames(touch_based_segments), value = TRUE)
    
    touch_based_segments <- touch_based_segments %>%
      rowwise() %>%
      mutate(
        team_players = list(
          if(team == home_team) {
            list(
              player_ids = c_across(starts_with("home_player_id")),
              roles = c_across(starts_with("home_roles_role"))
            )
          } else {
            list(
              player_ids = c_across(starts_with("visiting_player_id")),
              roles = c_across(starts_with("visiting_roles_role"))
            )
          }
        ),
        
        hitters = list({
          ids <- team_players$player_ids
          roles <- team_players$roles
          if (roles[1] %in% c("middle", "middle-f")) {
            ids <- ids[-1]
            roles <- roles[-1]
          }
          
          ids[roles %in% c("outside-f", "outside-b", "middle-f", "opposite", "middle", "outside")]
        }),
        hitters_tagged = list(paste0("HIT|", hitters)),
        hitters_padded = list(c(hitters_tagged, rep(NA, 4 - length(hitters_tagged)))),
        hit_1 = hitters_padded[[1]],
        hit_2 = hitters_padded[[2]],
        hit_3 = hitters_padded[[3]],
        hit_4 = hitters_padded[[4]],
        attack_present = any(grepl("^Attack\\|", c_across(all_of(par_cols)))) # Just for ease.
      ) %>%
      ungroup()%>%
      mutate(across(all_of(par_cols), ~ ifelse(grepl("^Attack\\|", .x), NA, .x)),
             across(starts_with("hit_"), ~ ifelse(attack_present, ., NA_character_))
             )
    }
    
  if(prev_act){
    prev_touch <- touch_based_segments %>%
      group_by(match_id, point_id) %>%
      mutate(
        prev_opp_1 = lag(par_1),
        prev_opp_2 = lag(par_2),
        prev_opp_3 = lag(par_3),
        prev_opp_4 = lag(par_4)
      ) %>%
      ungroup() %>%
      select(match_id, team_touch_id, prev_opp_1, prev_opp_2, prev_opp_3, prev_opp_4)
    
    #filter(!is.na(prev_opp))
    
    touch_based_segments <- touch_based_segments %>%
      left_join(prev_touch, join_by(match_id, team_touch_id))
  }
  
  touch_based_segments <- align_prev_opps(touch_based_segments)
  
  return(touch_based_segments)
}

align_prev_opps <- function(df) {
  if (!all(paste0("prev_opp_", 1:4) %in% names(df))) return(df)
  
  df %>%
    rowwise() %>%
    mutate(
      # grab all previous opp touches for this row
      prevs = list(na.omit(c(prev_opp_1, prev_opp_2, prev_opp_3, prev_opp_4))),
      n = length(prevs),
      # right-align the vector into 4 slots
      prevs_padded = list(c(rep(NA_character_, 4 - n), prevs)),
      prev_opp_1 = prevs_padded[[4]],
      prev_opp_2 = prevs_padded[[3]],
      prev_opp_3 = prevs_padded[[2]],
      prev_opp_4 = prevs_padded[[1]]
    ) %>%
    ungroup() %>%
    select(-prevs, -n, -prevs_padded)
}


touch_based_segments_raw <- lineups_by_touch(aggregate_plays, skl_inc = FALSE, def_inc = TRUE, prev_act = TRUE)
touch_based_segments_skills <- lineups_by_touch(aggregate_plays, skl_inc = TRUE, def_inc = TRUE, prev_act = TRUE)
touch_based_segments_skills_phitters <- lineups_by_touch(aggregate_plays, skl_inc = TRUE, def_inc = TRUE, prev_act = TRUE, pres_hitters = TRUE)

touch_based_segments_raw_fab <- lineups_by_touch(aggregate_plays_fab, skl_inc = FALSE, def_inc = TRUE, prev_act = TRUE)
touch_based_segments_skills_fab <- lineups_by_touch(aggregate_plays_fab, skl_inc = TRUE, def_inc = TRUE, prev_act = TRUE)
touch_based_segments_skills_phitters_fab <- lineups_by_touch(aggregate_plays_fab, skl_inc = TRUE, def_inc = TRUE, prev_act = TRUE, pres_hitters = TRUE)

temp <- touch_based_segments_raw %>%
  count(value)









save_fun(touch_based_segments_raw)
save_fun(touch_based_segments_skills)
save_fun(touch_based_segments_skills_phitters)

save_fun(touch_based_segments_raw_fab)
save_fun(touch_based_segments_skills_fab)
save_fun(touch_based_segments_skills_phitters_fab)

# Aligning stuff ----------------------------------------------------------


touch_based_segments_raw <- align_prev_opps(touch_based_segments_raw)
touch_based_segments_skills <- align_prev_opps(touch_based_segments_skills)
touch_based_segments_skills_phitters <- align_prev_opps(touch_based_segments_skills_phitters)


touch_based_segments_raw_fab <- align_prev_opps(touch_based_segments_raw_fab)
touch_based_segments_skills_fab <- align_prev_opps(touch_based_segments_skills_fab)
touch_based_segments_skills_phitters_fab <- align_prev_opps(touch_based_segments_skills_phitters_fab)



quick_counting <- aggregate_plays %>%
  count(player_id, role)


viewfinder <- touch_based_segments_skills_phitters %>%
  select(team_touch_id, par_1, par_2, par_3, par_4, prev_opp_1, hit_1, hit_2, hit_3, hit_4, def_1, def_2, def_3, def_4, def_5, def_6, value)


# Fixing the prev_opp stuff -----------------------------------------------





# Just to count some stuff

mythingo <- aggregate_plays_fab %>%
  filter(role == "libero")

front_back_summary <- mythingo %>%
  mutate(
    court_type = if_else(str_detect(player_id, "BK"), "Back-court", "Front-court")
  ) %>%
  count(court_type) %>%
  mutate(percentage = round(100 * n / sum(n), 2))


counting_possession_outcomes <- touch_based_segments_raw_fab %>%
  count(value)

counting_hitting_outcomes <- hit_based_segments %>%
  count(value)


out_of_rotation <- aggregate_plays_fab %>%
  filter(
    !if_any(
      c(home_player_id1, home_player_id2, home_player_id3,
        home_player_id4, home_player_id5, home_player_id6,
        visiting_player_id1, visiting_player_id2, visiting_player_id3,
        visiting_player_id4, visiting_player_id5, visiting_player_id6),
      ~ player_id == .
    )
  )

dim(out_of_rotation)
dim(aggregate_plays_fab)

# Possession Endings ------------------------------------------------------



my_sequence_plays <- aggregate_plays %>%
  group_by(point_id) %>%
  mutate(
    prev_skill = lag(skill),
    prev_role = lag(role),
    prev_evaluation = lag(evaluation),
    prevprev_skill = lag(skill, 2),
    prevprev_evaluation = lag(evaluation, 2)
  ) %>%
  ungroup()

role_skill_counts <- my_sequence_plays %>%
  count(prev_role, prev_skill, role, skill, sort = TRUE)

skill_counts <- my_sequence_plays %>%
  count(prev_skill, skill, sort = TRUE) %>%
  filter(skill == "Serve")



looking <- my_sequence_plays %>%
  filter(evaluation == "Ball directly back over net") %>%
  select(match_id, point_id, team, player_name, prev_skill, prev_evaluation, prevprev_skill, prevprev_evaluation, skill, evaluation)

# To count how many rallies are decided by certain actions. E.g. Rallies decided by serves, attacks, dig errors, etc. 



skill_outcomes <- aggregate_plays %>%
  filter(!is.na(skill)) %>%
  count(skill, evaluation) 

prev_skill_outcomes <- my_sequence_plays %>%
  count(prev_skill, prev_evaluation, skill, evaluation)

prevprev_skill_outcomes <- my_sequence_plays %>%
  filter(is.na(skill), prev_skill != "Timeout") %>%
  count(prevprev_skill, prevprev_evaluation, prev_skill, prev_evaluation)

attack_outcomes <- prevprev_skill_outcomes %>%
  filter(prevprev_evaluation == "Winning attack" | prev_evaluation == "Winning attack")

sum(prevprev_skill_outcomes$n) # Pretty much matches the amount of points!
sum(attack_outcomes$n)

# Number of things determined by attacks and serves. 

sum(skill_outcomes$n[
  skill_outcomes$evaluation %in% c("Winning attack", "Blocked") |
    (skill_outcomes$skill == "Attack" & skill_outcomes$evaluation == "Error")
  ])

sum(skill_outcomes$n[
  (skill_outcomes$skill == "Serve" & skill_outcomes$evaluation %in% c("Ace", "Error"))])

rally_outcomes %>%
  filter(point == TRUE) %>%
  pull(n)
