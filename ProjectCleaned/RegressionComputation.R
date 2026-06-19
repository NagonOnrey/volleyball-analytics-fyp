library(glmnet)
library(tidyr)
library(dplyr)
library(tibble)
library(ggrepel)
library(purrr)
library(aod)
library(ggplot2)

load("p_trix_pm_raw.RData")
load("p_trix_pm_raw_sadj.RData")

load("p_trix_pm_hit.RData")
load("p_trix_pm_hit_sadj.RData")
load("p_trix_pm_hit_net.RData")
load("p_trix_pm_hit_net_sadj.RData")

load("p_trix_pm_touch_raw.RData")
load("p_trix_pm_touch_raw_def.RData")

load("p_trix_pm_touch_skills.RData")
load("p_trix_pm_touch_skills_def.RData")
load("p_trix_pm_touch_skills_stone.RData")

load("p_trix_pm_touch_skills_prevact.RData")
load("p_trix_pm_touch_skills_prevact_def.RData")
load("p_trix_pm_touch_skills_prevact_stone.RData")

load("p_trix_pm_touch_skills_prevseq.RData")
load("p_trix_pm_touch_skills_prevseq_def.RData")
load("p_trix_pm_touch_skills_prevseq_stone.RData")

load("p_trix_pm_touch_skills_phitters.RData")
load("p_trix_pm_touch_skills_phitters_def.RData")

# FAB LOAD

load("p_trix_pm_raw_fab.RData")
load("p_trix_pm_raw_sadj_fab.RData")

load("p_trix_pm_hit_fab.RData")
load("p_trix_pm_hit_sadj_fab.RData")
load("p_trix_pm_hit_net_fab.RData")
load("p_trix_pm_hit_net_sadj_fab.RData")

load("p_trix_pm_touch_raw_fab.RData")
load("p_trix_pm_touch_raw_def_fab.RData")

load("p_trix_pm_touch_skills_fab.RData")
load("p_trix_pm_touch_skills_def_fab.RData")
load("p_trix_pm_touch_skills_stone_fab.RData")

load("p_trix_pm_touch_skills_prevact_fab.RData")
load("p_trix_pm_touch_skills_prevact_def_fab.RData")
load("p_trix_pm_touch_skills_prevact_stone_fab.RData")

load("p_trix_pm_touch_skills_prevseq_fab.RData")
load("p_trix_pm_touch_skills_prevseq_def_fab.RData")
load("p_trix_pm_touch_skills_prevseq_stone_fab.RData")

load("p_trix_pm_touch_skills_phitters_fab.RData")
load("p_trix_pm_touch_skills_phitters_def_fab.RData")

regression_computation <- function(input_matrix, touchBased, threshold, alp_val, name, type, prevact = FALSE) {
  X_matrix <- input_matrix$X
  Y_matrix <- input_matrix$Y
  
  X_matrix <- X_matrix[which(!is.na(Y_matrix)), , drop = FALSE]   # Removing NA rows. 
  Y_matrix <- Y_matrix[which(!is.na(Y_matrix))]
  
  # Forming positive and negative matrices - only for touchBased models. 
  if(touchBased){
    Y_pos <- ifelse(Y_matrix == -1, 0, Y_matrix)
    Y_neg <- -ifelse(Y_matrix == 1, 0, Y_matrix)
  }
  
  
  # Number Calculation ------------------------------------------------------
  
  # --- Number Calculation (correct for STONE; no double counting) ----------
  # name is the model name string you pass in, e.g. "TPM_skills_stone" or "..._prevact_stone"
  is_stone <- grepl("_stone", name)
  
  # columns flagged as DEF (never weighted; just count occurrences)
  def_cols <- grepl("(^DEF\\||^def_)", colnames(X_matrix))
  
  if (touchBased && is_stone) {
    # For STONE models: use only the POSITIVE mass in X (no negatives),
    # because rows already sum to 1 across contributors.
    # Non-DEF: sum(X>0 weights); DEF: simple occurrence count
    pos_mass <- Matrix::colSums(pmax(X_matrix, 0), na.rm = TRUE)
    
    number <- numeric(ncol(X_matrix))
    names(number) <- colnames(X_matrix)
    
    # non-DEF: positive mass only
    number[!def_cols] <- pos_mass[!def_cols]
    
    # DEF: count occurrences (don’t weight)
    if (any(def_cols)) {
      number[def_cols] <- Matrix::colSums(X_matrix[, def_cols, drop = FALSE] != 0, na.rm = TRUE)
    }
    
  } else {
    # Non-stone logic (unchanged from your intent).
    # Default: count non-zero entries; prevact: only positives; DEF still occurrence count.
    if (isTRUE(prevact)) {
      number <- Matrix::colSums(X_matrix > 0, na.rm = TRUE)
    } else {
      number <- Matrix::colSums(abs(X_matrix) > 0, na.rm = TRUE)
    }
    names(number) <- colnames(X_matrix)
    
    if (any(def_cols)) {
      number[def_cols] <- Matrix::colSums(X_matrix[, def_cols, drop = FALSE] != 0, na.rm = TRUE)
    }
  }
  
  # Preserve serving_adj as occurrence count if present
  if ("serving_adj" %in% colnames(X_matrix)) {
    number["serving_adj"] <- Matrix::colSums(X_matrix[, "serving_adj", drop = FALSE] != 0, na.rm = TRUE)
  }
  
  # Pass to Computation Function -------------------------------------------------------------
  
  low_inv_players <- names(number[number <= threshold])
  keep_players <- setdiff(colnames(X_matrix), low_inv_players)
  orig_names <- colnames(X_matrix)
  
  X_matrix <- X_matrix[, keep_players, drop=FALSE]
  
  if(touchBased){
    pos_outcome_df <- calculate_coefficients(X_matrix, Y_pos, low_inv_players, keep_players, alp_val, name, type)
    neg_outcome_df <- calculate_coefficients(X_matrix, Y_neg, low_inv_players, keep_players, alp_val, name, type)
    net_df <- left_join(pos_outcome_df, neg_outcome_df, by = "player_id", suffix = c("_pos", "_neg")) %>%
      mutate(
        !!paste0(name, "_neg") := -.data[[paste0(name, "_neg")]]
      )
    
    attributes(net_df)[c("pos_attributes", "neg_attributes")] <- list(
      attributes(pos_outcome_df),
      attributes(neg_outcome_df)
    )
  }
  else{net_df <- calculate_coefficients(X_matrix, Y_matrix, low_inv_players, keep_players, alp_val, name, type)}
  
  net_df <- net_df %>%
    mutate(!!paste0("number_", name) := number[player_id])
  
  return(net_df)
}

calculate_coefficients <- function(X_prepped, Y_prepped, low_inv_players, keep_players, alp_val, name, type) {
  penalty <- ifelse(grepl("serving_adj", colnames(X_prepped)), 0, 1)
  
  cv_prep <- cv.glmnet(X_prepped, Y_prepped, family = type, alpha = alp_val, penalty.factor = penalty) # lambda.min.ratio = 1e-3 if it's dying...
  plot(cv_prep)
  best_lambda <- cv_prep$lambda.min
  model <- glmnet(X_prepped, Y_prepped, family = type, alpha = alp_val, lambda = best_lambda, penalty.factor = penalty)
  
  intercept <- as.vector(coef(model))[1]
  coeffs <- setNames(as.vector(coef(model)[-1]), keep_players) # Extracting player information as a dataframe. 
  player_coeffs <- c(coeffs, setNames(rep(0, length(low_inv_players)), low_inv_players)) # all low_inv_players have '0' impact
  df <- enframe(player_coeffs, name = "player_id", value = name)
  
  attr(df, "intercept") <- intercept # Adds the intercept as an 'attribute'. 
  attr(df, "best_lambda") <- best_lambda
  attr(df, "cv_results") <- cv_prep
  
  return(df)
}

# LINEAR METRICS

RAPM_raw <- regression_computation(p_trix_pm_raw, touchBased = FALSE, threshold = 1000, alp_val = 0, name = "RAPM_raw", type = "gaussian")
RAPM_raw_sadj <- regression_computation(p_trix_pm_raw_sadj, touchBased = FALSE, threshold = 1000, alp_val = 0, name = "RAPM_raw_sadj", type = "gaussian")

RAPM_raw %>%
  arrange(desc(RAPM_raw)) %>%
  filter(number_RAPM_raw > 50) %>%
  slice_head(n = 5)

RAPM_hitting <- regression_computation(p_trix_pm_hit, touchBased = FALSE, threshold = 500, alp_val = 0, name = "RAPM_hitting", type = "gaussian")
RAPM_hitting_sadj <- regression_computation(p_trix_pm_hit_sadj, touchBased = FALSE, threshold = 500, alp_val = 0, name = "RAPM_hitting_sadj", type = "gaussian")
RAPM_hitting_net <- regression_computation(p_trix_pm_hit_net, touchBased = FALSE, threshold = 1000, alp_val = 0, name = "RAPM_hitting_net", type = "gaussian")
RAPM_hitting_net_sadj <- regression_computation(p_trix_pm_hit_net_sadj, touchBased = FALSE, threshold = 1000, alp_val = 0, name = "RAPM_hitting_net_sadj", type = "gaussian")

# LOGISTIC METRICS

TPM_raw <- regression_computation(p_trix_pm_touch_raw, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_raw", type = "binomial")
TPM_raw_def <- regression_computation(p_trix_pm_touch_raw_def, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_raw_def", type = "binomial")

TPM_raw %>%
  arrange(desc(TPM_raw_pos)) %>%
  filter(number_TPM_raw > 50) %>%
  slice_head(n = 5)

TPM_raw %>%
  summarise(avg_pos = mean(TPM_raw_pos, na.rm = TRUE))


TPM_skills <- regression_computation(p_trix_pm_touch_skills, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills", type = "binomial")
TPM_skills_def <- regression_computation(p_trix_pm_touch_skills_def, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_def", type = "binomial")
TPM_skills_stone <- regression_computation(p_trix_pm_touch_skills_stone, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_stone", type = "binomial")

TPM_skills_prevact <- regression_computation(p_trix_pm_touch_skills_prevact, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevact", type = "binomial", prevact = TRUE)
TPM_skills_prevact_def <- regression_computation(p_trix_pm_touch_skills_prevact_def, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevact_def", type = "binomial", prevact = TRUE)
TPM_skills_prevact_stone <- regression_computation(p_trix_pm_touch_skills_prevact_stone, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevact_stone", type = "binomial", prevact = TRUE)

TPM_skills_prevseq <- regression_computation(p_trix_pm_touch_skills_prevseq, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevseq", type = "binomial", prevact = TRUE)
TPM_skills_prevseq_def <- regression_computation(p_trix_pm_touch_skills_prevseq_def, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevseq_def", type = "binomial", prevact = TRUE)
TPM_skills_prevseq_stone <- regression_computation(p_trix_pm_touch_skills_prevseq_stone, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevseq_stone", type = "binomial", prevact = TRUE)

TPM_skills_phitters <- regression_computation(p_trix_pm_touch_skills_phitters, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_phitters", type = "binomial")
TPM_skills_phitters_def <- regression_computation(p_trix_pm_touch_skills_phitters_def, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_phitters_def", type = "binomial")


# FAB Operations ----------------------------------------------------------

# LINEAR METRICS

RAPM_raw_fab_p <- regression_computation(p_trix_pm_raw_fab, touchBased = FALSE, threshold = 100, alp_val = 0, name = "RAPM_raw_fab", type = "gaussian")
RAPM_raw_sadj_fab_p <- regression_computation(p_trix_pm_raw_sadj_fab, touchBased = FALSE, threshold = 100, alp_val = 0, name = "RAPM_raw_sadj_fab", type = "gaussian")

RAPM_hitting_fab_p <- regression_computation(p_trix_pm_hit_fab, touchBased = FALSE, threshold = 50, alp_val = 0, name = "RAPM_hitting_fab", type = "gaussian")
RAPM_hitting_sadj_fab_p <- regression_computation(p_trix_pm_hit_sadj_fab, touchBased = FALSE, threshold = 50, alp_val = 0, name = "RAPM_hitting_sadj_fab", type = "gaussian")
RAPM_hitting_net_fab_p <- regression_computation(p_trix_pm_hit_net_fab, touchBased = FALSE, threshold = 100, alp_val = 0, name = "RAPM_hitting_net_fab", type = "gaussian")
RAPM_hitting_net_sadj_fab_p <- regression_computation(p_trix_pm_hit_net_sadj_fab, touchBased = FALSE, threshold = 100, alp_val = 0, name = "RAPM_hitting_net_sadj_fab", type = "gaussian")

# LOGISTIC METRICS

TPM_raw_fab_p <- regression_computation(p_trix_pm_touch_raw_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_raw_fab", type = "binomial")
TPM_raw_def_fab_p <- regression_computation(p_trix_pm_touch_raw_def_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_raw_def_fab", type = "binomial")

TPM_skills_fab_p <- regression_computation(p_trix_pm_touch_skills_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_fab", type = "binomial")
TPM_skills_def_fab_p <- regression_computation(p_trix_pm_touch_skills_def_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_def_fab", type = "binomial")
TPM_skills_stone_fab_p <- regression_computation(p_trix_pm_touch_skills_stone_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_stone_fab", type = "binomial")

TPM_skills_prevact_fab_p <- regression_computation(p_trix_pm_touch_skills_prevact_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevact_fab", type = "binomial", prevact = TRUE)
TPM_skills_prevact_def_fab_p <- regression_computation(p_trix_pm_touch_skills_prevact_def_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevact_def_fab", type = "binomial", prevact = TRUE)
TPM_skills_prevact_stone_fab_p <- regression_computation(p_trix_pm_touch_skills_prevact_stone_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevact_stone_fab", type = "binomial", prevact = TRUE)

TPM_skills_prevseq_fab_p <- regression_computation(p_trix_pm_touch_skills_prevseq_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevseq_fab", type = "binomial", prevact = TRUE)
TPM_skills_prevseq_def_fab_p <- regression_computation(p_trix_pm_touch_skills_prevseq_def_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevseq_def_fab", type = "binomial", prevact = TRUE)
TPM_skills_prevseq_stone_fab_p <- regression_computation(p_trix_pm_touch_skills_prevseq_stone_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_prevseq_stone_fab", type = "binomial", prevact = TRUE)

TPM_skills_phitters_fab_p <- regression_computation(p_trix_pm_touch_skills_phitters_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_phitters_fab", type = "binomial")
TPM_skills_phitters_def_fab_p <- regression_computation(p_trix_pm_touch_skills_phitters_def_fab, touchBased = TRUE, threshold = 0, alp_val = 0, name = "TPM_skills_phitters_def_fab", type = "binomial")


# SAVING EVERYTHING!

save_fun(RAPM_raw)
save_fun(RAPM_raw_sadj)

save_fun(RAPM_hitting)
save_fun(RAPM_hitting_sadj)
save_fun(RAPM_hitting_net)
save_fun(RAPM_hitting_net_sadj)

save_fun(TPM_raw)
save_fun(TPM_raw_def)
save_fun(TPM_raw_prevact)

save_fun(TPM_skills)
save_fun(TPM_skills_def)
save_fun(TPM_skills_stone)

save_fun(TPM_skills_prevact)
save_fun(TPM_skills_prevact_def)
save_fun(TPM_skills_prevact_stone)

save_fun(TPM_skills_prevseq)
save_fun(TPM_skills_prevseq_def)
save_fun(TPM_skills_prevseq_stone)

save_fun(TPM_skills_phitters)
save_fun(TPM_skills_phitters_def)


# FAB Split ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Apply to a list of dataframes

split_front_back_metrics <- function(df) {
  metric_cols <- setdiff(colnames(df), "player_id")
  
  df <- df %>%
    mutate(
      type = case_when(
        str_detect(player_id, "FT\\|") ~ "front",
        str_detect(player_id, "BK\\|") ~ "back",
        TRUE ~ NA_character_
      ),
      base_player_id = str_remove(player_id, "FT\\||BK\\|")
    )
  
  out <- df %>% distinct(base_player_id) %>% rename(player_id = base_player_id)
  
  for (col in metric_cols) {
    tmp <- df %>%
      select(base_player_id, type, !!sym(col)) %>%
      pivot_wider(
        names_from = type,
        values_from = !!sym(col)
      )
    
    # Rebuild column names
    names(tmp) <- names(tmp) %>% sapply(function(n) {
      if (n == "base_player_id") return(n)
      
      # Extract _pos/_neg if present
      pos_neg <- str_extract(col, "_pos$|_neg$")
      if (is.na(pos_neg)) pos_neg <- ""
      
      base <- str_remove(col, "_pos$|_neg$")
      
      paste0(base, "_", n, pos_neg)
    })
    
    tmp <- tmp %>% rename(player_id = base_player_id)
    out <- left_join(out, tmp, by = "player_id")
  }
  
  return(out)
}

split_front_back_list <- function(df_list) {
  map(df_list, split_front_back_metrics)
}

fab_list <- list(
  RAPM_raw_fab_p, RAPM_raw_sadj_fab_p, 
  RAPM_hitting_fab_p, RAPM_hitting_sadj_fab_p, RAPM_hitting_net_fab_p, RAPM_hitting_net_sadj_fab_p, 
  TPM_raw_fab_p, TPM_raw_def_fab_p, 
  TPM_skills_fab_p, TPM_skills_def_fab_p, TPM_skills_stone_fab_p, 
  TPM_skills_prevact_fab_p, TPM_skills_prevact_def_fab_p, TPM_skills_prevact_stone_fab_p, 
  TPM_skills_prevseq_fab_p, TPM_skills_prevseq_def_fab_p, TPM_skills_prevseq_stone_fab_p,
  TPM_skills_phitters_fab_p, TPM_skills_phitters_def_fab_p
)

split_results <- split_front_back_list(fab_list)

names(split_results) <- c(
  "RAPM_raw_fab", "RAPM_raw_sadj_fab", 
  "RAPM_hitting_fab", "RAPM_hitting_sadj_fab", "RAPM_hitting_net_fab", "RAPM_hitting_net_sadj_fab",
  "TPM_raw_fab", "TPM_raw_def_fab", 
  "TPM_skills_fab", "TPM_skills_def_fab", "TPM_skills_stone_fab",
  "TPM_skills_prevact_fab", "TPM_skills_prevact_def_fab", "TPM_skills_prevact_stone_fab", 
  "TPM_skills_prevseq_fab", "TPM_skills_prevseq_def_fab", "TPM_skills_prevseq_stone_fab",
  "TPM_skills_phitters_fab", "TPM_skills_phitters_def_fab"
)

list2env(split_results, envir = .GlobalEnv)

save_fun(RAPM_raw_fab)
save_fun(RAPM_raw_sadj_fab)

save_fun(RAPM_hitting_fab)
save_fun(RAPM_hitting_sadj_fab)
save_fun(RAPM_hitting_net_fab)
save_fun(RAPM_hitting_net_sadj_fab)

save_fun(TPM_raw_fab)
save_fun(TPM_raw_def_fab)

save_fun(TPM_skills_fab)
save_fun(TPM_skills_def_fab)
save_fun(TPM_skills_stone_fab)

save_fun(TPM_skills_prevact_fab)
save_fun(TPM_skills_prevact_def_fab)
save_fun(TPM_skills_prevact_stone_fab)

save_fun(TPM_skills_prevseq_fab)
save_fun(TPM_skills_prevseq_def_fab)
save_fun(TPM_skills_prevseq_stone_fab)

save_fun(TPM_skills_phitters_fab)
save_fun(TPM_skills_phitters_def_fab)

