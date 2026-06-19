library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)

load("aggregate_files.RData")
load("touch_based_segments_skills.RData")

load("RAPM_raw.RData")
load("RAPM_raw_sadj.RData")

load("RAPM_hitting.RData")
load("RAPM_hitting_sadj.RData")
load("RAPM_hitting_net.RData")
load("RAPM_hitting_net_sadj.RData")

load("TPM_raw.RData")
load("TPM_raw_def.RData")

load("TPM_skills.RData")
load("TPM_skills_def.RData")
load("TPM_skills_stone.RData")

load("TPM_skills_prevact.RData")
load("TPM_skills_prevact_def.RData")
load("TPM_skills_prevact_stone.RData")

load("TPM_skills_prevseq.RData")
load("TPM_skills_prevseq_def.RData")
load("TPM_skills_prevseq_stone.RData")

load("TPM_skills_phitters.RData")
load("TPM_skills_phitters_def.RData")


# FAB Load ----------------------------------------------------------------

load("RAPM_raw_fab.RData")
load("RAPM_raw_sadj_fab.RData")

load("RAPM_hitting_fab.RData")
load("RAPM_hitting_sadj_fab.RData")

load("RAPM_hitting_net_fab.RData")
load("RAPM_hitting_net_sadj_fab.RData")

load("TPM_raw_fab.RData")
load("TPM_raw_def_fab.RData")

load("TPM_skills_fab.RData")
load("TPM_skills_def_fab.RData")
load("TPM_skills_stone_fab.RData")

load("TPM_skills_prevact_fab.RData")
load("TPM_skills_prevact_def_fab.RData")
load("TPM_skills_prevact_stone_fab.RData")

load("TPM_skills_prevseq_fab.RData")
load("TPM_skills_prevseq_def_fab.RData")
load("TPM_skills_prevseq_stone_fab.RData")

load("TPM_skills_phitters_fab.RData")
load("TPM_skills_phitters_def_fab.RData")

# Meta information (team and role) ----------------------------------------

skill_order <- c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball")

players_meta <- aggregate_plays %>%
  distinct(player_id, role, team) %>%
  filter(!(player_id == "D.Ogórek" & team != "GKS Katowice")) # Cause this one guy plays on two teams. 

produce_colnames <- function(input_players) {
  stat_cols <- names(input_players)[sapply(input_players, is.numeric) & !grepl("^number_", names(input_players)) & !grepl("avg$", names(input_players))]
  log_cols <- stat_cols[grepl("^TPM", stat_cols)]
  lin_cols <- setdiff(stat_cols, log_cols)
  
  skill_cols <- stat_cols[str_detect(stat_cols, paste0("\\|(", paste(skill_order, collapse="|"), ")"))]
  role_rel_cols <- stat_cols[grepl("role_rel$", stat_cols)]
  skill_rel_cols <- stat_cols[grepl("skill_rel$", stat_cols)]
  rel_cols <- unique(c(role_rel_cols, skill_rel_cols))
  
  def_cols <- stat_cols[grepl("DEF", stat_cols)]
  
  pos_cols <- stat_cols[grepl("pos", stat_cols)]
  neg_cols <- stat_cols[grepl("neg", stat_cols)]
  
  
  return(list(
    stat = stat_cols,
    log = log_cols,
    lin = lin_cols,
    skill = skill_cols,
    role_rel = role_rel_cols,
    skill_rel = skill_rel_cols,
    rel = rel_cols,
    def = def_cols,
    pos = pos_cols,
    neg = neg_cols
  ))
}


# Coalescing players from their tags --------------------------------------

all_player_tags <- list(RAPM_raw, RAPM_raw_sadj, 
                        RAPM_hitting, RAPM_hitting_sadj, RAPM_hitting_net, RAPM_hitting_net_sadj, 
                        TPM_raw, TPM_raw_def, 
                        TPM_skills, TPM_skills_def, TPM_skills_stone, 
                        TPM_skills_prevact, TPM_skills_prevact_def, TPM_skills_prevact_stone,
                        TPM_skills_prevseq, TPM_skills_prevseq_def, TPM_skills_prevseq_stone, 
                        TPM_skills_phitters, TPM_skills_phitters_def,
                        
                        RAPM_raw_fab, RAPM_raw_sadj_fab, 
                        RAPM_hitting_fab, RAPM_hitting_sadj_fab, RAPM_hitting_net_fab, RAPM_hitting_net_sadj_fab, 
                        TPM_raw_fab, TPM_raw_def_fab, 
                        TPM_skills_fab, TPM_skills_def_fab, TPM_skills_stone_fab,
                        TPM_skills_prevact_fab, TPM_skills_prevact_def_fab, TPM_skills_prevact_stone_fab,
                        TPM_skills_prevseq_fab, TPM_skills_prevseq_def_fab, TPM_skills_prevseq_stone_fab, 
                        TPM_skills_phitters_fab, TPM_skills_phitters_def_fab)

coalesce_player_results <- function(player_tags_df){
 player_tags_df %>%
    mutate(parts = str_split(player_id, "\\|")) %>% # Categorises player tags. 
    rowwise() %>%
    mutate(
      player_id_clean = tail(parts, 1),
      stat_tag = if (length(parts) > 1) paste(head(parts, -1), collapse = "_") else "BASE" # Applies 'Base' if no prefix. 
    ) %>%
    ungroup() %>%
    select(-parts, -player_id) %>%
    rename(player_id = player_id_clean) %>%
    
    mutate(player_id = normalize_name(player_id)) %>%
    
    pivot_longer(cols = -c(player_id, stat_tag), # Apply to everything except player_id and stat_prefix. 
                 names_to = "measure", values_to = "val") %>% # Temporary naming for pivot_wider later. 
    mutate(prefixed_name = paste0(measure, "|", stat_tag)) %>%
    select(player_id, prefixed_name, val) %>%
    pivot_wider(names_from = prefixed_name, values_from = val)
}

processed_list <- map(all_player_tags, coalesce_player_results) 
coalesced_players_raw <- reduce(processed_list, left_join, by = "player_id") %>%
  left_join(players_meta, by = "player_id") %>%
  relocate(player_id, role, team)

coalesced_players_dupes_removed <- coalesced_players_raw %>%
  # Ensure number_ columns are numeric and non-negative
  mutate(across(starts_with("number_"), ~ abs(as.numeric(.x)))) %>%
  
  # Create pos/neg copies explicitly and rename them robustly
  mutate(
    across(
      starts_with("number_"),
      .fns = list(pos = identity, neg = identity),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  rename_with(
    ~ str_replace(.x, "^(number_[^|]*)(.*)_(pos|neg)$", "\\1_\\3\\2"),
    .cols = matches("_(pos|neg)$")
  )

# Getting rid of these 1 or 2 posession guys. 
coalesced_players_culled <- coalesced_players_dupes_removed %>%
  filter(!is.na(role))

coalesced_players <- coalesced_players_culled

# Average level of play - team/role/skill/misc ----------------------------------

cols <- produce_colnames(coalesced_players)

compute_group_values <- function(input_players, grouping_vars, stat_cols) {
  input_players %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      across(
        all_of(stat_cols),
        .fns = list(
          avg = ~ {
            w_col <- paste0("number_", cur_column())
            if (w_col %in% names(cur_data())) {
              sum(.x * cur_data()[[w_col]], na.rm = TRUE) / sum(cur_data()[[w_col]], na.rm = TRUE)
            } else {
              NA_real_
            }
          },
          total = ~ {
            w_col <- paste0("number_", cur_column())
            if (w_col %in% names(cur_data())) {
              sum(cur_data()[[w_col]], na.rm = TRUE)
            } else {
              NA_real_
            }
          }
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

ugh <- coalesced_players %>%
  select(player_id, role, team, contains("RAPM_raw"))

role_avgs <- compute_group_values(coalesced_players, grouping_vars = c("role"), cols$stat)
# team_avgs <- compute_group_values(coalesced_players, grouping_vars = c("team"), cols$stat)

save_fun(role_avgs)

role_temp <- role_avgs %>%
  select(role, contains("TPM_skills"))

compute_skill_averages <- function(input_players, skill_cols) {
  skill_summary_df <- tibble(
    statname = skill_cols,
    avg = map_dbl(skill_cols, function(col_name) {
      weight_col <- paste0("number_", col_name)
      weighted.mean(input_players[[col_name]], input_players[[weight_col]], na.rm = TRUE)
    })
  )
  return(skill_summary_df)
}

my_col <- cols$skill

skill_avgs <- compute_skill_averages(coalesced_players, my_col)

temp_skills <- skill_avgs %>%
  filter(if_any(everything(), ~ grepl("^(TPM_skills_pos|TPM_skills_prevact_pos|TPM_skills_prevseq_pos)", .)))



save_fun(skill_avgs)

# Relative level of play --------------------------------------------------

# These are in the format      [skill_name]|[TAG]_rel_[role/skill]

role_relative <- coalesced_players %>%
  left_join(role_avgs %>% select(role, ends_with("_avg")), by = "role", suffix = c("", "_role_avg")) %>%
  mutate(across(
    all_of(cols$stat),
    ~ .x - get(paste0(cur_column(), "_avg")),
    .names = "{.col}_role_rel"
  ))

skill_relative <- role_relative %>%
  mutate(across(
    all_of(cols$stat),
    ~ .x - skill_avgs$avg[match(cur_column(), skill_avgs$statname)],
    .names = "{.col}_skill_rel"
  ))

# Now computing some 'league averages' for various things. 
compute_outcomes <- function(segments){
  long_df <- segments %>%
    select(par_1, par_2, par_3, par_4, par_5, value) %>%
    pivot_longer(
      cols = starts_with("par_"),
      names_to = "slot",
      values_to = "skill_player"
    ) %>%
    filter(!is.na(skill_player)) %>%
    mutate(
      skill = str_extract(skill_player, "^[^|]+"),
      player_id = str_extract(skill_player, "(?<=\\|).*")) %>%
    left_join(players_meta, by = "player_id") %>%
    mutate(value = case_when(
      value ==  1 ~ "pos",
      value == -1 ~ "neg",
      value ==  0 ~ "zero",
      TRUE        ~ as.character(value)  # fallback
    )) %>%
    select(role, skill, value)
  
  return_probs <- long_df %>%
    group_by(role, skill, value) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(role, skill) %>%
    mutate(percentage = count/sum(count)*100) %>%
    mutate(logit_base = log(percentage/(100-percentage)))
  
  return(return_probs)
}

combine_outcomes <- function(outcomes_list, grouping) {
  return_outcomes <- outcomes_list %>%
    group_by({{grouping}}, value) %>%        # group by skill and outcome
    summarise(count = sum(count), .groups = "drop") %>%
    group_by({{grouping}}) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    mutate(logit_base = log(percentage/(100-percentage)))
  return(return_outcomes)
}

skill_role_outcomes <- compute_outcomes(touch_based_segments_skills) %>%
  filter(!is.na(role))
skill_outcomes <- combine_outcomes(skill_role_outcomes, skill)
role_outcomes <- combine_outcomes(skill_role_outcomes, role)

skill_role_lookup <- skill_role_outcomes %>%
  filter(value %in% c("pos", "neg")) %>%
  select(skill, role, value, logit_base) %>%
  pivot_wider(
    id_cols = role,
    names_from = c(skill, value),
    values_from = logit_base,
    names_sep = "_"
  )

generate_lookup <- function(outcomes_df, by_cols, prefix = NULL) {
  lookup <- outcomes_df %>%
    filter(value %in% c("pos", "neg")) %>%
    select(all_of(by_cols), value, logit_base) %>%
    pivot_wider(
      names_from = value,
      values_from = logit_base,
      names_glue = if (!is.null(prefix)) {
        paste0(prefix, "_BASE_{value}_logit")
      } else {
        "{value}_logit"
      }
    )
  
  return(lookup)
}
generate_wide <- function(df) {
  df %>%
    filter(value %in% c("pos", "neg")) %>%
    select(skill, value, logit_base) %>%
    pivot_wider(
      names_from = c(skill, value),
      values_from = logit_base,
      names_glue = "{skill}_{value}_logit"
    )
}
generate_role_skill_lookup <- function(skill_role_outcomes) {
  skill_role_outcomes %>%
    filter(value %in% c("pos", "neg")) %>%
    select(role, skill, value, logit_base) %>%
    # pivot wider: one row per role, columns for each skill×outcome
    pivot_wider(
      id_cols = role,
      names_from = c(skill, value),
      values_from = logit_base,
      names_glue = "role_{skill}_{value}_logit"
    )
}

role_lookup <- generate_lookup(role_outcomes, by_cols = "role", prefix = "role")
skill_lookup <- generate_wide(skill_outcomes)
role_skill_lookup <- generate_role_skill_lookup(skill_role_outcomes)

relativised_players <- skill_relative %>%
  left_join(role_lookup, by = "role") %>%
  bind_cols(skill_lookup) %>%
  left_join(role_skill_lookup, by = "role") %>%
  mutate(role_DEF_pos_logit = 0,
         role_DEF_neg_logit = 0,
         DEF_pos_logit = 0,
         DEF_neg_logit = 0,
         role_HIT_pos_logit = 0,
         role_HIT_neg_logit = 0,
         HIT_pos_logit = 0,
         HIT_neg_logit = 0)



temp <- relativised_players %>%
  select(player_id, team, role, contains("RAPM_raw_sadj"))



# Points added  ----------------------------

logistic <- function(x) 1/(1+exp(-x))
logit <- function(x) log(x/(1-x))

cols <- produce_colnames(relativised_players)

# LINEAR POINTS ADDED

lin_cols <- cols$lin
role_rel_cols <- cols$role_rel

inter_linear_df <- relativised_players %>%
  transmute(
    player_id, 
    role,
    team,
    across(
      all_of(intersect(lin_cols, role_rel_cols)),
      ~ .x * get(paste0("number_", str_remove(cur_column(), "_role_rel$"))),
      .names = "{.col}_points_added"
    ),
    )

compute_points_added_total <- function(df, id_cols = c("player_id", "role", "team")) {
  # Find all columns ending with _points_added
  pts_cols <- grep("_points_added$", colnames(df), value = TRUE)
  
  # Extract base metric name (everything before first |)
  # Remove _front, _back, _NA to unify front/back variants
  base_names <- unique(sub("(_front|_back|_NA)?\\|.*$", "", pts_cols))
  
  for (base in base_names) {
    # Find all columns for this base metric (including front/back/NA variants)
    cols_to_sum <- grep(paste0("^", base, "(_front|_back|_NA)?\\|.*_points_added$"), colnames(df), value = TRUE)
    
    # Create total column
    df[[paste0(base, "_points_added_total")]] <- rowSums(df[cols_to_sum], na.rm = TRUE)
  }
  
  return(df)
}

# LINEAR POINTS ADDED
linear_df <- compute_points_added_total(inter_linear_df)


# LOGISTIC POINTS ADDED ====

compute_metric_points_added <- function(
    metric_name, 
    suffixes, 
    baseline = c("role", "skill"), 
    input_players = relativised_players,
    front_back = FALSE  # NEW: handle metrics split into front/back
) {
  baseline <- match.arg(baseline)
  points_list <- list()
  
  for (suf in suffixes) {
    # message("Suffix:", suf)
    
    col_baseline <- if (suf %in% c("DEF", "HIT")) "role" else baseline
    
    # Handle front/back if requested
    fb_cols <- if (front_back) c("front", "back") else ""
    
    for (fb in fb_cols) {
      fb_prefix <- if (front_back) paste0("_", fb) else ""
      
      pos_col <- paste0(metric_name, fb_prefix, "_pos|", suf, "_", col_baseline, "_rel")
      neg_col <- paste0(metric_name, fb_prefix, "_neg|", suf, "_", col_baseline, "_rel")
      num_col <- paste0("number_", metric_name, fb_prefix, "|", suf)
      
      pos_logit_col <- case_when(
        col_baseline == "role" ~ paste0("role_", suf, "_pos_logit"),
        col_baseline == "skill" ~ paste0(suf, "_pos_logit")
      )
      
      neg_logit_col <- case_when(
        col_baseline == "role" ~ paste0("role_", suf, "_neg_logit"),
        col_baseline == "skill" ~ paste0(suf, "_neg_logit")
      )
      
      # Skip if columns don't exist
      missing_cols <- setdiff(c(pos_col, neg_col, num_col, pos_logit_col, neg_logit_col), colnames(input_players))
      if (length(missing_cols) > 0) {
        warning("Skipping ", suf, " ", fb, " due to missing columns: ", paste(missing_cols, collapse = ", "))
        next
      }
      
      df <- input_players %>%
        mutate(
          pos_increase_col = logistic(.data[[pos_logit_col]] + .data[[pos_col]]) - logistic(.data[[pos_logit_col]]),
          neg_increase_col = logistic(.data[[neg_logit_col]] - .data[[neg_col]]) - logistic(.data[[neg_logit_col]]),
          !!paste0(metric_name, "_", baseline, fb_prefix, "|", suf, "_points_added") :=
            (pos_increase_col - neg_increase_col) * .data[[num_col]]
        ) %>%
        select(player_id, role, team, ends_with("points_added"))
      
      points_list[[paste0(suf, fb)]] <- df
    }
  }
  
  points_df <- reduce(points_list, left_join, by = c("player_id", "role", "team")) %>%
    mutate(
      !!paste0(metric_name, "_", baseline, "_points_added_total") :=
        rowSums(across(ends_with("points_added")), na.rm = TRUE)
    )
  
  return(points_df)
}

# TPM RAW STUFF

tpm_raw_df <- compute_metric_points_added("TPM_raw", suffixes = c("BASE"), baseline = "role")
tpm_raw_def_df <- compute_metric_points_added("TPM_raw_def", suffixes = c("BASE", "DEF"), baseline = "role")
tpm_raw_fab_df <- compute_metric_points_added("TPM_raw_fab", suffixes = c("BASE"), baseline = "role", front_back = TRUE)
tpm_raw_def_fab_df <- compute_metric_points_added("TPM_raw_def_fab", suffixes = c("BASE", "DEF"), baseline = "role", front_back = TRUE)

tpm_points_df <- tpm_raw_df %>%
  left_join(tpm_raw_def_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_raw_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_raw_def_fab_df, by = c("player_id", "role", "team"))

temp <- tpm_raw_def_df %>%
  left_join(linear_df, by = c("player_id", "role", "team"))

cor(
  temp$`TPM_raw_def_role|DEF_points_added`,
  temp$`RAPM_raw_sadj|BASE_role_rel_points_added`,
  use = "complete.obs"
)

# TPM SKILLS STUFF (ROLE RELATIVE)

tpm_skills_role_rel_df <- compute_metric_points_added("TPM_skills", suffixes = c(skill_order), baseline = "role")
tpm_skills_def_role_rel_df <- compute_metric_points_added("TPM_skills_def", suffixes = c(skill_order, "DEF"), baseline = "role")
tpm_skills_stone_role_rel_df <- compute_metric_points_added("TPM_skills_stone", suffixes = c(skill_order), baseline = "role")

tpm_skills_prevact_role_rel_df <- compute_metric_points_added("TPM_skills_prevact", suffixes = c(skill_order), baseline = "role")
tpm_skills_prevact_def_role_rel_df <- compute_metric_points_added("TPM_skills_prevact_def", suffixes = c(skill_order, "DEF"), baseline = "role")
tpm_skills_prevact_stone_role_rel_df <- compute_metric_points_added("TPM_skills_prevact_stone", suffixes = c(skill_order), baseline = "role")

tpm_skills_prevseq_role_rel_df <- compute_metric_points_added("TPM_skills_prevseq", suffixes = c(skill_order), baseline = "role")
tpm_skills_prevseq_def_role_rel_df <- compute_metric_points_added("TPM_skills_prevseq_def", suffixes = c(skill_order, "DEF"), baseline = "role")
tpm_skills_prevseq_stone_role_rel_df <- compute_metric_points_added("TPM_skills_prevseq_stone", suffixes = c(skill_order), baseline = "role")                                         

tpm_skills_phitters_role_rel_df <- compute_metric_points_added("TPM_skills_phitters", suffixes <- c(setdiff(skill_order, "Attack"), "HIT"), baseline = "role")
tpm_skills_phitters_def_role_rel_df <- compute_metric_points_added("TPM_skills_phitters_def", suffixes <- c(setdiff(skill_order, "Attack"), "HIT", "DEF"), baseline = "role")

#FAB
tpm_skills_role_rel_fab_df <- compute_metric_points_added("TPM_skills_fab", suffixes = c(skill_order), baseline = "role", front_back = TRUE)
tpm_skills_def_role_rel_fab_df <- compute_metric_points_added("TPM_skills_def_fab", suffixes = c(skill_order, "DEF"), baseline = "role", front_back = TRUE)
tpm_skills_stone_role_rel_fab_df <- compute_metric_points_added("TPM_skills_stone_fab", suffixes = c(skill_order), baseline = "role", front_back = TRUE)

tpm_skills_prevact_role_rel_fab_df <- compute_metric_points_added("TPM_skills_prevact_fab", suffixes = c(skill_order), baseline = "role", front_back = TRUE)
tpm_skills_prevact_def_role_rel_fab_df <- compute_metric_points_added("TPM_skills_prevact_def_fab", suffixes = c(skill_order, "DEF"), baseline = "role", front_back = TRUE)
tpm_skills_prevact_stone_role_rel_fab_df <- compute_metric_points_added("TPM_skills_prevact_stone_fab", suffixes = c(skill_order), baseline = "role", front_back = TRUE)

tpm_skills_prevseq_role_rel_fab_df <- compute_metric_points_added("TPM_skills_prevseq_fab", suffixes = c(skill_order), baseline = "role", front_back = TRUE)
tpm_skills_prevseq_def_role_rel_fab_df <- compute_metric_points_added("TPM_skills_prevseq_def_fab", suffixes = c(skill_order, "DEF"), baseline = "role", front_back = TRUE)
tpm_skills_prevseq_stone_role_rel_fab_df <- compute_metric_points_added("TPM_skills_prevseq_stone_fab", suffixes = c(skill_order), baseline = "role", front_back = TRUE)

tpm_skills_phitters_role_rel_fab_df <- compute_metric_points_added("TPM_skills_phitters_fab", suffixes <- c(setdiff(skill_order, "Attack"), "HIT"), baseline = "role", front_back = TRUE)
tpm_skills_phitters_def_role_rel_fab_df <- compute_metric_points_added("TPM_skills_phitters_def_fab", suffixes <- c(setdiff(skill_order, "Attack"), "HIT", "DEF"), baseline = "role", front_back = TRUE)

temp <- tpm_skills_role_rel_df %>%
  select(contains("total"))
colnames(temp)

role_relative_points_df <- tpm_skills_role_rel_df %>%
  left_join(tpm_skills_def_role_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_stone_role_rel_df, by = c("player_id", "role", "team")) %>%
  
  left_join(tpm_skills_prevact_role_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_def_role_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_stone_role_rel_df, by = c("player_id", "role", "team")) %>%
  
  left_join(tpm_skills_prevseq_role_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_def_role_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_stone_role_rel_df, by = c("player_id", "role", "team")) %>%
  
  left_join(tpm_skills_phitters_role_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_phitters_def_role_rel_df, by = c("player_id", "role", "team")) %>%
  
  left_join(tpm_skills_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_def_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_stone_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  
  left_join(tpm_skills_prevact_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_def_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_stone_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  
  left_join(tpm_skills_prevseq_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_def_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_stone_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  
  left_join(tpm_skills_phitters_role_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_phitters_def_role_rel_fab_df, by = c("player_id", "role", "team"))

# TPM SKILL STUFF (SKILL RELATIVE)

tpm_skills_skill_rel_df <- compute_metric_points_added("TPM_skills", suffixes = c(skill_order), baseline = "skill")
tpm_skills_def_skill_rel_df <- compute_metric_points_added("TPM_skills_def", suffixes = c(skill_order, "DEF"), baseline = "skill")
tpm_skills_stone_skill_rel_df <- compute_metric_points_added("TPM_skills_stone", suffixes = c(skill_order), baseline = "skill")

tpm_skills_prevact_skill_rel_df <- compute_metric_points_added("TPM_skills_prevact", suffixes = c(skill_order), baseline = "skill")
tpm_skills_prevact_def_skill_rel_df <- compute_metric_points_added("TPM_skills_prevact_def", suffixes = c(skill_order, "DEF"), baseline = "skill")
tpm_skills_prevact_stone_skill_rel_df <- compute_metric_points_added("TPM_skills_prevact_stone", suffixes = c(skill_order), baseline = "skill")

tpm_skills_prevseq_skill_rel_df <- compute_metric_points_added("TPM_skills_prevseq", suffixes = c(skill_order), baseline = "skill")
tpm_skills_prevseq_def_skill_rel_df <- compute_metric_points_added("TPM_skills_prevseq_def", suffixes = c(skill_order, "DEF"), baseline = "skill")
tpm_skills_prevseq_stone_skill_rel_df <- compute_metric_points_added("TPM_skills_prevseq_stone", suffixes = c(skill_order), baseline = "skill")

tpm_skills_phitters_skill_rel_df <- compute_metric_points_added("TPM_skills_phitters", suffixes <- c(setdiff(skill_order, "Attack"), "HIT"), baseline = "skill")
tpm_skills_phitters_def_skill_rel_df <- compute_metric_points_added("TPM_skills_phitters_def", suffixes <- c(setdiff(skill_order, "Attack"), "HIT", "DEF"), baseline = "skill")


# FAB SKILL RELATIVE

tpm_skills_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_fab", suffixes = c(skill_order), baseline = "skill", front_back = TRUE)
tpm_skills_def_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_def_fab", suffixes = c(skill_order, "DEF"), baseline = "skill", front_back = TRUE)
tpm_skills_stone_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_stone_fab", suffixes = c(skill_order), baseline = "skill", front_back = TRUE)

tpm_skills_prevact_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_prevact_fab", suffixes = c(skill_order), baseline = "skill", front_back = TRUE)
tpm_skills_prevact_def_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_prevact_def_fab", suffixes = c(skill_order, "DEF"), baseline = "skill", front_back = TRUE)
tpm_skills_prevact_stone_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_prevact_stone_fab", suffixes = c(skill_order), baseline = "skill", front_back = TRUE)

tpm_skills_prevseq_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_prevseq_fab", suffixes = c(skill_order), baseline = "skill", front_back = TRUE)
tpm_skills_prevseq_def_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_prevseq_def_fab", suffixes = c(skill_order, "DEF"), baseline = "skill", front_back = TRUE)
tpm_skills_prevseq_stone_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_prevseq_stone_fab", suffixes = c(skill_order), baseline = "skill", front_back = TRUE)

tpm_skills_phitters_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_phitters_fab", suffixes <- c(setdiff(skill_order, "Attack"), "HIT"), baseline = "skill", front_back = TRUE)
tpm_skills_phitters_def_skill_rel_fab_df <- compute_metric_points_added("TPM_skills_phitters_def_fab", suffixes <- c(setdiff(skill_order, "Attack"), "HIT", "DEF"), baseline = "skill", front_back = TRUE)


skill_relative_points_df <- tpm_skills_skill_rel_df %>%
  left_join(tpm_skills_def_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_stone_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_def_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_stone_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_def_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_stone_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_phitters_skill_rel_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_phitters_def_skill_rel_df, by = c("player_id", "role", "team")) %>%
  
  left_join(tpm_skills_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_def_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_stone_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_def_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevact_stone_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_def_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_prevseq_stone_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_phitters_skill_rel_fab_df, by = c("player_id", "role", "team")) %>%
  left_join(tpm_skills_phitters_def_skill_rel_fab_df, by = c("player_id", "role", "team"))



# For return and plotting -------------------------------------------------

player_points_added <- linear_df %>%
  left_join(tpm_points_df, by = c("player_id", "role", "team")) %>%
  left_join(role_relative_points_df, by = c("player_id", "role", "team")) %>%
  left_join(skill_relative_points_df, by = c("player_id", "role", "team"))

temp <- player_points_added %>%
  select(player_id, role, team, contains("RAPM_raw_sadj"))


player_vault <- relativised_players %>%
  left_join(player_points_added, by = c("player_id", "role", "team"))

#Removing all the block points in the back,
player_vault <- player_vault %>%
  mutate(
    # Set Block_points_added to NA if the column name contains "_back"
    across(
      .cols = contains("Block_points_added") & contains("_back"),
      ~ NA_real_
    ),
    # Set Serve_points_added to NA if the column name contains "_front"
    across(
      .cols = contains("Serve_points_added") & contains("_front"),
      ~ NA_real_
    )
  )

temp <- player_vault %>%
  select(matches("points_added.*back|back.*points_added"))

colnames(vault_readable)

save_fun(player_vault)
