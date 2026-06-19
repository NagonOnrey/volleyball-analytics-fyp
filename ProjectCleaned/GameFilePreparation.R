library(datavolley)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(readr)
library(stringr)

game_files <- list.files("Data/parsed_matches", pattern = "\\.csv$", full.names = TRUE)

extract_file <- function(file_path) {
  x <- read_csv(file_path)
  return(x)
}

aggregate_files <- map(game_files, extract_file)

# Annoying naming conventions; sometimes uses full name, other times not:

normalize_name <- function(x) {
  # Trim and collapse whitespace
  x <- str_squish(x)
  
  # Split on ANY non-letter (handles dots, hyphens, multiple initials, etc.)
  # \p{L} = any Unicode letter
  parts_list <- str_split(x, "[^\\p{L}]+", simplify = FALSE)
  
  vapply(parts_list, function(tokens) {
    tokens <- tokens[tokens != ""]
    if (length(tokens) == 0) return(NA_character_)
    
    # First initial from the FIRST token
    first_initial <- str_sub(tokens[1], 1, 1)
    
    # Last token as surname
    last_name <- tokens[length(tokens)]
    

    paste0(first_initial, ".", last_name)
  }, FUN.VALUE = character(1))
}



process_positions <- function(x) {
  
  plays <- x %>%
    rename(player_id = player_name) %>%
    select(-time)
    
  
# Positions relative to setter --------------------------------------------
  
  relative_roles <- c("setter", "outside-f", "middle-f", "opposite", "outside-b", "middle-b")
  rotate_roles <- function(setter_pos) {relative_roles[ ( (0:5 + (7 - setter_pos)) %% 6 ) + 1 ]}
  
  plays_with_rotation <- plays %>%
    mutate(
      home_roles = map(home_setter_position, rotate_roles),
      visiting_roles = map(visiting_setter_position, rotate_roles)
    ) %>%
    unnest_wider(home_roles, names_sep = "_role") %>%
    unnest_wider(visiting_roles, names_sep = "_role")
  

  # Libero substitution -----------------------------------------------------

  # Defining the function to swap liberos.  
  
  libero_swap <- function(h_v, current_serving_team, df, i){
    
    team_col <- paste0(h_v, "_team")
    
    for (pos in c(1, 5, 6)) {
      
      # Gets the associated columns for each of these. 
      role_col <- paste0(h_v, "_roles_role", pos)
      
      is_middle <- !is.na(df[[role_col]][i]) &&
        df[[role_col]][i] %in% c("middle") 
      
      is_serving_pos_and_team_serving <- FALSE
      if (!is.na(current_serving_team) && !is.na(df[[team_col]][i])) {
        is_serving_pos_and_team_serving <- (pos == 1 && current_serving_team == df[[team_col]][i])
      }
      
      if (is_middle && !is_serving_pos_and_team_serving) {
        df[[role_col]][i] <- "libero"
      }
    }
    return(df)
  }  
  
  # Performing the libero swap. 
  
  plays_modified_libero <- plays_with_rotation
  
  for (i in 1:nrow(plays_modified_libero)) {
    current_serving_team <- plays_modified_libero$serving_team[i]
    plays_modified_libero <- libero_swap("home", current_serving_team, plays_modified_libero, i)
    plays_modified_libero <- libero_swap("visiting", current_serving_team, plays_modified_libero, i)
  }
  
# Position assignment -----------------------------------------------------

  
  plays_and_position_roles <- plays_modified_libero %>%
    mutate(
      position_role = case_when(
        player_id == home_player_id1 ~ home_roles_role1,
        player_id == home_player_id2 ~ home_roles_role2,
        player_id == home_player_id3 ~ home_roles_role3,
        player_id == home_player_id4 ~ home_roles_role4,
        player_id == home_player_id5 ~ home_roles_role5,
        player_id == home_player_id6 ~ home_roles_role6,
        player_id == visiting_player_id1 ~ visiting_roles_role1,
        player_id == visiting_player_id2 ~ visiting_roles_role2,
        player_id == visiting_player_id3 ~ visiting_roles_role3,
        player_id == visiting_player_id4 ~ visiting_roles_role4,
        player_id == visiting_player_id5 ~ visiting_roles_role5,
        player_id == visiting_player_id6 ~ visiting_roles_role6,
        TRUE ~ NA_character_ 
      )
    )
  
  # Displaying listed positions. 
  
  player_positions <- read_csv("Data/player_positions.csv") %>%
    select(player_id, role) %>%
    mutate(player_id = normalize_name(player_id))

  plays_and_roles <- plays_and_position_roles %>%
    mutate(player_id = normalize_name(player_id)) %>%
    left_join(player_positions, by = "player_id")
  
  return(plays_and_roles)
  
}

# Creating the initial aggregate_plays. 

aggregate_plays <- map_dfr(aggregate_files, process_positions)

# Renaming some weird teams and fixing a whole bunch of implementation errors from scraping. 
aggregate_plays <- aggregate_plays %>%
  mutate(
    across(
      .cols = matches("(^|_)team$"),  # matches "team", "home_team", "visiting_team", "serving_team"
      .fns = ~ case_when(
        TRUE ~ .x
      )
    )
  ) %>%
  mutate(skill = case_when(
    skill == "Pass" ~ "Reception",
    skill == "Setting" ~ "Set",
    TRUE ~ skill
  )) %>%
  filter(player_id != "NA.NA")

# Fixing the score --------------------------------------------------------

points_fixed <- aggregate_plays %>%
  group_by(match_id, set_number, point_id) %>%
  slice_tail(n = 1) %>%  # get one row per point
  arrange(match_id, set_number, point_id) %>%
  group_by(match_id, set_number) %>%
  mutate(
    home_score_start_of_point = lag(home_team_score, default = 0),
    visiting_score_start_of_point = lag(visiting_team_score, default = 0),
    point_won_by = case_when(
      home_team_score > home_score_start_of_point ~ home_team,
      visiting_team_score > visiting_score_start_of_point ~ visiting_team,
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

aggregate_plays <- aggregate_plays %>%
  select(-any_of(c(
    "home_score_start_of_point",
    "visiting_score_start_of_point",
    "point_won_by"
  ))) %>%  # remove old versions if they exist
  left_join(
    points_fixed %>%
      select(match_id, point_id,
             home_score_start_of_point,
             visiting_score_start_of_point,
             point_won_by),
    by = c("match_id", "point_id")
  ) %>% 
  group_by(match_id, point_id) %>%
  mutate(
    is_last_in_point = row_number() == n(),
    evaluation = case_when(
      winning_attack ~ "winning attack",
      is_last_in_point & team == point_won_by ~ "winning attack",
      TRUE ~ evaluation
    )
  ) %>%
  ungroup()


# Managing front and back court -------------------------------------------

aggregate_plays_fab <- aggregate_plays

aggregate_plays_fab <- aggregate_plays %>%
  mutate(across(matches("^(home|visiting)_player_id[1-6]$"),
      .fns = ~ {
        pos <- as.numeric(gsub(".*player_id", "", cur_column()))
        prefix <- ifelse(pos %in% c(1, 5, 6), "BK|", "FT|")
        paste0(prefix, .x)
      },
      .names = "{.col}")) %>%
  mutate(
    bk_flag = pmap_lgl(
      list(player_id,
           home_player_id1, home_player_id5, home_player_id6,
           visiting_player_id1, visiting_player_id5, visiting_player_id6),
      function(pid, h1, h5, h6, v1, v5, v6) {
        if (is.na(pid)) return(FALSE)
        # strip first 3 chars (e.g. "FT|" or "BK|") safely
        strip3 <- function(x) if (is.na(x)) NA_character_ else str_sub(x, 4)
        pid %in% c(strip3(h1), strip3(h5), strip3(h6), strip3(v1), strip3(v5), strip3(v6))
      }
    ),
    
    player_id = if_else(
      !is.na(player_id) & !str_detect(player_id, "\\|"),
      paste0(if_else(bk_flag, "BK|", "FT|"), player_id),
      player_id
    )
  ) %>%
  select(-bk_flag)


aggregate_plays_fab <- aggregate_plays_fab %>%
  mutate(
    player_id = if_else(
      role == "libero",
      paste0("BK|", str_remove(player_id, "^[A-Z]{2}\\|")),  # remove first two letters + '|', add BK|
      player_id
    )
  )


quick_count <- aggregate_plays %>%
  count(player_id, role)

save(aggregate_files, file = "aggregate_files.RData")
save(aggregate_plays, file = "aggregate_plays.RData")
save(aggregate_plays_fab, file = "aggregate_plays_fab.RData")
