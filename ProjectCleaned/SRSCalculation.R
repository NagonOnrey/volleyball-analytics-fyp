library(glmnet)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(purrr)

load("aggregate_files.RData")
load("player_points_added.RData")

# Game Extraction ---------------------------------------------------------

temp <- aggregate_plays %>%
  distinct(home_team)

teams_meta <- aggregate_plays %>%
  group_by(match_id, set_number) %>%
  slice_tail() %>%
  ungroup() %>%
  select(home_team, visiting_team, home_team_score, visiting_team_score) %>%
  mutate(score_diff = home_team_score - visiting_team_score)

# Record Calculation ---------------------------------------------------------

home_teams <- teams_meta %>%  mutate(team = home_team, opponent = visiting_team, team_score = home_team_score, opponent_score = visiting_team_score, home_or_away = "home") %>% select(-home_team, -visiting_team, -home_team_score, -visiting_team_score)
visiting_teams <- teams_meta %>%  mutate(team = visiting_team, opponent = home_team, team_score = visiting_team_score, opponent_score = home_team_score, home_or_away = "visiting") %>% select(-home_team, -visiting_team, -home_team_score, -visiting_team_score)
all_teams <- bind_rows(home_teams, visiting_teams)

records <- all_teams %>%
  group_by(team) %>%
  summarise(
    sets_played = n(),
    sets_won = sum(team_score > opponent_score),
    sets_lost = sum(team_score < opponent_score),
    win_pct = sets_won / sets_played
  ) %>%
  arrange(desc(win_pct))

# SRS Calculation ---------------------------------------------------------

team_names <- unique(teams_meta$home_team)

team_matrix <- matrix(0, nrow = nrow(teams_meta), ncol = length(team_names))
colnames(team_matrix) <- team_names

for (i in seq_len(nrow(teams_meta))) {
  home <- teams_meta$home_team[i]
  visiting <- teams_meta$visiting_team[i]
  
  team_matrix[i, home] <- 1
  team_matrix[i, visiting] <- -1
}

srs_cv <- cv.glmnet(team_matrix, teams_meta$score_diff, alpha = 0)
best_lambda <- srs_cv$lambda.min

srs_model <- glmnet(team_matrix, teams_meta$score_diff, alpha = 0, lambda = best_lambda)
b_srs <- setNames(as.vector(coef(srs_model)[-1]), team_names)
b_srs <- b_srs - mean(b_srs)
srs_df <- enframe(b_srs, name = "team", value = "srs")

team_stats <- left_join(srs_df, records, by="team")

save(team_stats, file = "team_stats.RData")
