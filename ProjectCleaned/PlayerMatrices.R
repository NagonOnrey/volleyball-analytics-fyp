library(datavolley)
library(dplyr)
library(tidyr)
library(purrr)
library(Matrix)

# stone  = Sum To ONE

load("point_based_segments.RData")
load("hit_based_segments.RData")
load("hit_based_segments_net.RData")
load("touch_based_segments_raw.RData")
load("touch_based_segments_skills.RData")
load("touch_based_segments_skills_phitters.RData")

load("point_based_segments_fab.RData")
load("hit_based_segments_fab.RData")
load("hit_based_segments_net_fab.RData")
load("touch_based_segments_raw_fab.RData")
load("touch_based_segments_skills_fab.RData")
load("touch_based_segments_skills_phitters_fab.RData")

create_blank_matrix <- function(df_seg, pos, neg) {
  pos_cols <- unlist(lapply(pos, function(p) grep(paste0("^", p), names(df_seg), value = TRUE)))
  neg_cols <- unlist(lapply(neg, function(p) grep(paste0("^", p), names(df_seg), value = TRUE)))
  
  df_seg[pos_cols] <- lapply(df_seg[pos_cols], as.character)
  df_seg[neg_cols] <- lapply(df_seg[neg_cols], as.character)
  
  all_player_ids <- unique(unlist(df_seg %>% select(all_of(c(pos_cols, neg_cols))))) %>% sort()
  
  num_segments <- nrow(df_seg)
  X <- matrix(0, nrow = num_segments, ncol = length(all_player_ids))
  colnames(X) <- all_player_ids
  
  return(X)
}

plain_matrix <- function(df_seg, X_f, pos, neg, prevact = FALSE) {
  n_rows <- nrow(df_seg)
  
  pos_cols <- unlist(lapply(pos, function(p) grep(paste0("^", p), names(df_seg), value = TRUE)))
  neg_cols <- unlist(lapply(neg, function(p) grep(paste0("^", p), names(df_seg), value = TRUE)))
  
  df_seg[pos_cols] <- lapply(df_seg[pos_cols], as.character)
  df_seg[neg_cols] <- lapply(df_seg[neg_cols], as.character)
  
  pos_mat <- as.matrix(df_seg[, pos_cols, drop = FALSE])
  neg_mat <- as.matrix(df_seg[, neg_cols, drop = FALSE])
  
  if (prevact && "prev_opp" %in% names(df_seg)) {
    neg_mat <- cbind(neg_mat, prev_opp = df_seg$prev_opp)
  }
  
  # Flatten to indices — only non-NAs
  pos_idx <- which(!is.na(pos_mat) & pos_mat != "")
  neg_idx <- which(!is.na(neg_mat) & neg_mat != "")
  
  # Build sparse index vectors
  i_pos <- ((pos_idx - 1L) %% n_rows) + 1L
  j_pos <- match(pos_mat[pos_idx], colnames(X_f))
  i_neg <- ((neg_idx - 1L) %% n_rows) + 1L
  j_neg <- match(neg_mat[neg_idx], colnames(X_f))
  
  i <- c(i_pos, i_neg)
  j <- c(j_pos, j_neg)
  x <- c(rep(1, length(i_pos)), rep(-1, length(i_neg)))
  
  X_sparse <- Matrix::sparseMatrix(
    i = i,
    j = j,
    x = x,
    dims = c(n_rows, ncol(X_f)),
    dimnames = list(NULL, colnames(X_f))
  )
  
  Y_f <- df_seg$value
  list(X = X_sparse, Y = Y_f)
}

off_def_matrix <- function(df_seg, X_f, pos, neg) {
  n_rows <- nrow(df_seg)
  
  # Determine if row is home team (TRUE = home, FALSE = away)
  is_home <- df_seg$team == df_seg$home_team
  
  # Find offensive/defensive columns
  pos_cols <- unlist(lapply(pos, function(p) grep(paste0("^", p), names(df_seg), value = TRUE)))
  neg_cols <- unlist(lapply(neg, function(p) grep(paste0("^", p), names(df_seg), value = TRUE)))
  
  # Force to character BEFORE matrix conversion (avoids factor codes issue)
  df_seg[pos_cols] <- lapply(df_seg[pos_cols], as.character)
  df_seg[neg_cols] <- lapply(df_seg[neg_cols], as.character)
  
  pos_mat <- as.matrix(df_seg[, pos_cols, drop = FALSE])
  neg_mat <- as.matrix(df_seg[, neg_cols, drop = FALSE])
  
  # Initialize row and player vectors
  all_rows <- rep(1:n_rows, times = ncol(pos_mat))
  
  # Offensive assignments
  off_players <- ifelse(
    is_home[all_rows],
    as.vector(pos_mat),
    as.vector(neg_mat)
  )
  off_keep <- !is.na(off_players) & off_players != ""
  off_cols <- match(off_players[off_keep], colnames(X_f))
  X_f[cbind(all_rows[off_keep], off_cols)] <- 1
  
  # Defensive assignments
  def_players <- ifelse(
    is_home[all_rows],
    as.vector(neg_mat),
    as.vector(pos_mat)
  )
  def_keep <- !is.na(def_players) & def_players != ""
  def_cols <- match(def_players[def_keep], colnames(X_f))
  X_f[cbind(all_rows[def_keep], def_cols)] <- -1
  
  # Sparse conversion
  X_sparse <- Matrix(X_f, sparse = TRUE)  
  Y_f <- df_seg$value
  
  list(
    X = X_sparse,
    Y = Y_f
  )
}

produce_player_matrix <- function(df_seg, pos, neg, identifier = "plain", sadj = FALSE, prevact = FALSE, phitters = FALSE, stone = FALSE) {
  df_seg <- df_seg %>% filter(!is.na(serving_team))
  
  X_blank <- create_blank_matrix(df_seg, pos, neg)
  
  if(identifier == "plain") {return_matrix <- plain_matrix(df_seg, X_blank, pos, neg, prevact = prevact)}
  if(identifier == "off_def") {return_matrix <- off_def_matrix(df_seg, X_blank, pos, neg)}
  
  if(sadj){
    return_matrix$X <- cbind(return_matrix$X, serving_adj = 0)
    serving_effect <- ifelse(df_seg$serving_team == df_seg$home_team, 1,
                             ifelse(df_seg$serving_team == df_seg$visiting_team, -1, 0))
    return_matrix$X[, "serving_adj"] <- serving_effect
  }
  
if (stone) {
  X <- return_matrix$X
  # skip defensive columns
  def_idx <- grep("^def_", colnames(X))
  work_idx <- setdiff(seq_len(ncol(X)), def_idx)
  X_work <- X[, work_idx, drop = FALSE]

  # positive and negative parts
  X_pos <- X_work; X_pos@x[X_pos@x <= 0] <- 0
  X_neg <- X_work; X_neg@x[X_neg@x >= 0] <- 0

  # row sums (abs values for negatives)
  pos_sums <- Matrix::rowSums(X_pos)
  neg_sums <- Matrix::rowSums(abs(X_neg))
  pos_sums[pos_sums == 0] <- 1
  neg_sums[neg_sums == 0] <- 1

  # scale rows so positives sum to +1, negatives sum to –1
  X_scaled <- Matrix::Diagonal(x = 1 / pos_sums) %*% X_pos + 
              Matrix::Diagonal(x = 1 / neg_sums) %*% X_neg

  # put defensive columns back, keeping original order
  if (length(def_idx) > 0) {
    X <- cbind(X_scaled, X[, def_idx, drop = FALSE])
    X <- X[, colnames(return_matrix$X), drop = FALSE]
  } else {
    X <- X_scaled
  }

  return_matrix$X <- X
}
  
  Y_pos <- ifelse(return_matrix$Y == 1, 1, 0)
  Y_neg <- ifelse(return_matrix$Y == -1, 1, 0)
  
  return(list(
    X = return_matrix$X,
    Y = return_matrix$Y,
    Y_pos = Y_pos,
    Y_neg = Y_neg
  ))
}

# Linear

p_trix_pm_raw <- produce_player_matrix(point_based_segments, "h_", "v_")
p_trix_pm_raw_sadj <- produce_player_matrix(point_based_segments, "h_", "v_", sadj = TRUE)

p_trix_pm_hit <- produce_player_matrix(hit_based_segments, "h_", "v_", identifier = "off_def")
p_trix_pm_hit_sadj <- produce_player_matrix(hit_based_segments, "h_", "v_", identifier = "off_def", sadj = TRUE)
p_trix_pm_hit_net <- produce_player_matrix(hit_based_segments_net, "h_", "v_")
p_trix_pm_hit_net_sadj <- produce_player_matrix(hit_based_segments_net, "h_", "v_", sadj = TRUE)

# Logistic

p_trix_pm_touch_raw <- produce_player_matrix(touch_based_segments_raw, "par_", "NULL")
p_trix_pm_touch_raw_def <- produce_player_matrix(touch_based_segments_raw, "par_", "def_")
p_trix_pm_touch_raw_prevact <- produce_player_matrix(touch_based_segments_raw, "par_", "prev_opp_1")

# Skill based

p_trix_pm_touch_skills <- produce_player_matrix(touch_based_segments_skills, "par_", "NULL")
p_trix_pm_touch_skills_def <- produce_player_matrix(touch_based_segments_skills, "par_", "def_")
p_trix_pm_touch_skills_stone <- produce_player_matrix(touch_based_segments_skills, "par_", "NULL", stone = TRUE)

p_trix_pm_touch_skills_prevact <- produce_player_matrix(touch_based_segments_skills, "par_", "prev_opp_1")
p_trix_pm_touch_skills_prevact_def <- produce_player_matrix(touch_based_segments_skills, "par_", c("prev_opp_1", "def_"))
p_trix_pm_touch_skills_prevact_stone <- produce_player_matrix(touch_based_segments_skills, "par_", "prev_opp_1", stone = TRUE)

p_trix_pm_touch_skills_prevseq <- produce_player_matrix(touch_based_segments_skills, "par_", "prev_opp_")
p_trix_pm_touch_skills_prevseq_def <- produce_player_matrix(touch_based_segments_skills, "par_", c("prev_opp_", "def_"))
p_trix_pm_touch_skills_prevseq_stone <- produce_player_matrix(touch_based_segments_skills, "par_", "prev_opp_", stone = TRUE)

p_trix_pm_touch_skills_phitters <- produce_player_matrix(touch_based_segments_skills_phitters, c("par_", "hit_"), "NULL")
p_trix_pm_touch_skills_phitters_def <- produce_player_matrix(touch_based_segments_skills_phitters, c("par_", "hit_"), "def_")


# FAB Version -------------------------------------------------------------

p_trix_pm_raw_fab <- produce_player_matrix(point_based_segments_fab, "h_", "v_")
p_trix_pm_raw_sadj_fab <- produce_player_matrix(point_based_segments_fab, "h_", "v_", sadj = TRUE)

p_trix_pm_hit_fab <- produce_player_matrix(hit_based_segments_fab, "h_", "v_", identifier = "off_def")
p_trix_pm_hit_sadj_fab <- produce_player_matrix(hit_based_segments_fab, "h_", "v_", identifier = "off_def", sadj = TRUE)
p_trix_pm_hit_net_fab <- produce_player_matrix(hit_based_segments_net_fab, "h_", "v_")
p_trix_pm_hit_net_sadj_fab <- produce_player_matrix(hit_based_segments_net_fab, "h_", "v_", sadj = TRUE)

# Logistic

p_trix_pm_touch_raw_fab <- produce_player_matrix(touch_based_segments_raw_fab, "par_", "NULL")
p_trix_pm_touch_raw_def_fab <- produce_player_matrix(touch_based_segments_raw_fab, "par_", "def_")
p_trix_pm_touch_raw_prevact_fab <- produce_player_matrix(touch_based_segments_raw_fab, "par_", "prev_opp_1")

# Skill based

p_trix_pm_touch_skills_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", "NULL")
p_trix_pm_touch_skills_def_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", "def_")
p_trix_pm_touch_skills_stone_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", "NULL", stone = TRUE)

p_trix_pm_touch_skills_prevact_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", "prev_opp_1")
p_trix_pm_touch_skills_prevact_def_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", c("prev_opp_1", "def_"))
p_trix_pm_touch_skills_prevact_stone_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", "prev_opp_1", stone = TRUE)

p_trix_pm_touch_skills_prevseq_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", "prev_opp_")
p_trix_pm_touch_skills_prevseq_def_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", c("prev_opp_", "def_"))
p_trix_pm_touch_skills_prevseq_stone_fab <- produce_player_matrix(touch_based_segments_skills_fab, "par_", "prev_opp_", stone = TRUE)

p_trix_pm_touch_skills_phitters_fab <- produce_player_matrix(touch_based_segments_skills_phitters_fab, c("par_", "hit_"), "NULL")
p_trix_pm_touch_skills_phitters_def_fab <- produce_player_matrix(touch_based_segments_skills_phitters_fab, c("par_", "hit_"), "def_")

# SAVING

save_fun(p_trix_pm_raw)
save_fun(p_trix_pm_raw_sadj)

save_fun(p_trix_pm_hit)
save_fun(p_trix_pm_hit_sadj)
save_fun(p_trix_pm_hit_net)
save_fun(p_trix_pm_hit_net_sadj)

save_fun(p_trix_pm_touch_raw)
save_fun(p_trix_pm_touch_raw_def)
save_fun(p_trix_pm_touch_raw_prevact)

save_fun(p_trix_pm_touch_skills)
save_fun(p_trix_pm_touch_skills_def)
save_fun(p_trix_pm_touch_skills_stone)

save_fun(p_trix_pm_touch_skills_prevact)
save_fun(p_trix_pm_touch_skills_prevact_def)
save_fun(p_trix_pm_touch_skills_prevact_stone)

save_fun(p_trix_pm_touch_skills_prevseq)
save_fun(p_trix_pm_touch_skills_prevseq_def)
save_fun(p_trix_pm_touch_skills_prevseq_stone)

save_fun(p_trix_pm_touch_skills_phitters)
save_fun(p_trix_pm_touch_skills_phitters_def)

# FAB SAVING

save_fun(p_trix_pm_raw_fab)
save_fun(p_trix_pm_raw_sadj_fab)

save_fun(p_trix_pm_hit_fab)
save_fun(p_trix_pm_hit_sadj_fab)
save_fun(p_trix_pm_hit_net_fab)
save_fun(p_trix_pm_hit_net_sadj_fab)

save_fun(p_trix_pm_touch_raw_fab)
save_fun(p_trix_pm_touch_raw_def_fab)
save_fun(p_trix_pm_touch_raw_prevact_fab)

save_fun(p_trix_pm_touch_skills_fab)
save_fun(p_trix_pm_touch_skills_def_fab)
save_fun(p_trix_pm_touch_skills_stone_fab)

save_fun(p_trix_pm_touch_skills_prevact_fab)
save_fun(p_trix_pm_touch_skills_prevact_def_fab)
save_fun(p_trix_pm_touch_skills_prevact_stone_fab)

save_fun(p_trix_pm_touch_skills_prevseq_fab)
save_fun(p_trix_pm_touch_skills_prevseq_def_fab)
save_fun(p_trix_pm_touch_skills_prevseq_stone_fab)

save_fun(p_trix_pm_touch_skills_phitters_fab)
save_fun(p_trix_pm_touch_skills_phitters_def_fab)


# Sanity Checking

dim(p_trix_pm_raw$X)
summary((p_trix_pm_touch_skills_prevact_stone$X))

Matrix::nnzero(p_trix_pm_touch_skills_prevact_stone$X) / (prod(dim(p_trix_pm_touch_skills_prevact_stone$X)))
summary(Matrix::rowSums(p_trix_pm_touch_skills_prevact$X))

inspect_rows <- sample(1:10, 3)
as.matrix(p_trix_pm_touch_skills_prevact_stone$X[inspect_rows, 1:20])
print(p_trix_pm_touch_skills_prevact_stone$X[1:20, 1:100], sparse = TRUE)

p_trix_pm_touch_skills_prevact_stone$Y

colnames(p_trix_pm_raw_sadj$X)[1:238]


dim(p_trix_pm_touch_raw$X)
dim(p_trix_pm_raw$X)
dim(p_trix_pm_hit$X)


# Best Checker ------------------------------------------------------------

matrix <- p_trix_pm_touch_skills_stone

observation <- matrix$X[9, , drop = FALSE]
nz_cols <- which(observation[1, ] != 0)
named_values <- observation[1, nz_cols]
names(named_values) <- colnames(matrix$X)[nz_cols]
named_values

