library(glmnet)
library(tidyr)
library(dplyr)
library(purrr)

# This program has a few functions. But ultimately I generate a prediction matrix, and use that to generate MSE or a calibration curve. 

load("p_trix_pm_raw.RData")
load("p_trix_pm_raw_sadj.RData")

load("p_trix_pm_hit.RData")
load("p_trix_pm_hit_sadj.RData")
load("p_trix_pm_hit_net.RData")
load("p_trix_pm_hit_net_sadj.RData")

load("p_trix_pm_touch_raw.RData")
load("p_trix_pm_touch_raw_def.RData")
load("p_trix_pm_touch_raw_prevact.RData")

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

# MAIN FUNCTION BEGINS

produce_folds <- function(X){
  K <- 10
  fold_id <- sample(rep(1:K, length.out = nrow(X)))
  return(fold_id)
}

point_folds <- produce_folds(p_trix_pm_raw$X)
hit_folds <- produce_folds(p_trix_pm_hit$X)
poss_folds <- produce_folds(p_trix_pm_touch_raw$X)

get_stored_lambda <- function(model_name, pos = TRUE) {
  model_obj <- get(model_name, envir = .GlobalEnv)
  
  if (grepl("^TPM", model_name)) {
    # Logistic models (TPM)
    if (pos) {
      return(attr(model_obj, "pos_attributes")$best_lambda)
    } else {
      return(attr(model_obj, "neg_attributes")$best_lambda)
    }
  } else if (grepl("^RAPM", model_name)) {
    # Linear models (RAPM)
    return(attr(model_obj, "best_lambda"))
  } else {
    stop(paste("Unknown model type for", model_name))
  }
}


calculate_min_lambda <- function(X, Y, pos) {
  if(pos){
    Y <- ifelse(Y == -1, 0, Y)
  } else{Y <- -ifelse(Y == 1, 0, Y)}
  
  cv_model <- cv.glmnet(X, Y, family = "binomial", alpha = 0)
  return(cv_model$lambda.min)
}

gen_predictions_log <- function(X, Y, fold_id, test_lambda, pos){
  pred <- numeric(nrow(X))
  K <- max(fold_id)
  
  if(pos){
    Y <- ifelse(Y == -1, 0, Y)
  } else{Y <- -ifelse(Y == 1, 0, Y)}
  
  for(i in 1:K){
    X_cv <- X[fold_id != i, ]
    Y_cv <- Y[fold_id != i]
    model_cv <- glmnet(X_cv, Y_cv, family = "binomial", alpha = 0, lambda = test_lambda)
    pred[fold_id == i] <- predict(model_cv, X[fold_id == i, ], type = "response")
  }
  
  return(pred)
}
gen_predictions_lin <- function(X, Y, fold_id, test_lambda){
  pred <- numeric(nrow(X))
  K <- max(fold_id)
  
  penalty <- ifelse(grepl("serving_adj", colnames(X)), 0, 1)
  
  for(i in 1:K){
    X_cv <- X[fold_id != i, ]
    Y_cv <- Y[fold_id != i]
    model_cv <- glmnet(X_cv, Y_cv, alpha = 0, lambda = test_lambda, penalty.factor = penalty)
    pred[fold_id == i] <- predict(model_cv, X[fold_id == i, ])
  }
  
  return(pred)
}

calculate_pred_log <- function(player_matrix, fold_id, model_name) {
  X <- player_matrix$X
  Y <- player_matrix$Y
  
  min_lambda_pos <- get_stored_lambda(model_name, pos = TRUE)
  min_lambda_neg <- get_stored_lambda(model_name, pos = FALSE)
  
  message(model_name, ": Using precomputed λ_pos=", round(min_lambda_pos, 6),
          ", λ_neg=", round(min_lambda_neg, 6))
  
  pos_pred <- gen_predictions_log(X, Y, fold_id, min_lambda_pos, pos = TRUE)
  neg_pred <- gen_predictions_log(X, Y, fold_id, min_lambda_neg, pos = FALSE)
  
  return(list(pos_pred = pos_pred, neg_pred = neg_pred))
}
calculate_pred_lin <- function(player_matrix, fold_id, model_name) {
  X <- player_matrix$X
  Y <- player_matrix$Y
  
  test_lambda <- get_stored_lambda(model_name)
  
  message(model_name, ": Using precomputed λ=", round(test_lambda, 6))
  
  pred <- gen_predictions_lin(X, Y, fold_id, test_lambda)
  return(list(pred = pred))
}



pred_jobs <- list(
  RAPM_raw          = list(p_mat = p_trix_pm_raw,              fun = calculate_pred_lin, fold = point_folds),
  RAPM_raw_sadj     = list(p_mat = p_trix_pm_raw_sadj,         fun = calculate_pred_lin, fold = point_folds),
  
  RAPM_hitting      = list(p_mat = p_trix_pm_hit,              fun = calculate_pred_lin, fold = hit_folds),
  RAPM_hitting_sadj = list(p_mat = p_trix_pm_hit_sadj,         fun = calculate_pred_lin, fold = hit_folds),
  RAPM_hitting_net      = list(p_mat = p_trix_pm_hit_net,              fun = calculate_pred_lin, fold = hit_folds),
  RAPM_hitting_net_sadj = list(p_mat = p_trix_pm_hit_net_sadj,         fun = calculate_pred_lin, fold = hit_folds),
  
  TPM_raw           = list(p_mat = p_trix_pm_touch_raw,        fun = calculate_pred_log, fold = poss_folds),
  TPM_raw_def           = list(p_mat = p_trix_pm_touch_raw_def,        fun = calculate_pred_log, fold = poss_folds),
  
  TPM_skills        = list(p_mat = p_trix_pm_touch_skills,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_def        = list(p_mat = p_trix_pm_touch_skills_def,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_stone        = list(p_mat = p_trix_pm_touch_skills_stone,     fun = calculate_pred_log, fold = poss_folds),
  
  TPM_skills_prevact =           list(p_mat = p_trix_pm_touch_skills_prevact,fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_prevact_def        = list(p_mat = p_trix_pm_touch_skills_prevact_def,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_prevact_stone        = list(p_mat = p_trix_pm_touch_skills_prevact_stone,     fun = calculate_pred_log, fold = poss_folds),
  
  TPM_skills_prevseq =          list(p_mat = p_trix_pm_touch_skills_prevseq,fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_prevseq_def        = list(p_mat = p_trix_pm_touch_skills_prevseq_def,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_prevseq_stone        = list(p_mat = p_trix_pm_touch_skills_prevseq_stone,     fun = calculate_pred_log, fold = poss_folds),
  
  TPM_skills_phitters        = list(p_mat = p_trix_pm_touch_skills_phitters,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_phitters_def        = list(p_mat = p_trix_pm_touch_skills_phitters_def,     fun = calculate_pred_log, fold = poss_folds),
  
  # Now for FAB
  
  RAPM_raw_fab          = list(p_mat = p_trix_pm_raw_fab,              fun = calculate_pred_lin, fold = point_folds),
  RAPM_raw_sadj_fab     = list(p_mat = p_trix_pm_raw_sadj_fab,         fun = calculate_pred_lin, fold = point_folds),
  
  RAPM_hitting_fab      = list(p_mat = p_trix_pm_hit_fab,              fun = calculate_pred_lin, fold = hit_folds),
  RAPM_hitting_sadj_fab = list(p_mat = p_trix_pm_hit_sadj_fab,         fun = calculate_pred_lin, fold = hit_folds),
  RAPM_hitting_net_fab      = list(p_mat = p_trix_pm_hit_net_fab,              fun = calculate_pred_lin, fold = hit_folds),
  RAPM_hitting_net_sadj_fab = list(p_mat = p_trix_pm_hit_net_sadj_fab,         fun = calculate_pred_lin, fold = hit_folds),
  
  TPM_raw_fab           = list(p_mat = p_trix_pm_touch_raw_fab,        fun = calculate_pred_log, fold = poss_folds),
  TPM_raw_def_fab           = list(p_mat = p_trix_pm_touch_raw_def_fab,        fun = calculate_pred_log, fold = poss_folds),
  
  TPM_skills_fab        =  list(p_mat = p_trix_pm_touch_skills_fab,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_stone_fab        = list(p_mat = p_trix_pm_touch_skills_stone_fab,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_def_fab        = list(p_mat = p_trix_pm_touch_skills_def_fab,     fun = calculate_pred_log, fold = poss_folds),
  
  TPM_skills_prevact_fab =         list(p_mat = p_trix_pm_touch_skills_prevact_fab, fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_prevact_def_fab        = list(p_mat = p_trix_pm_touch_skills_prevact_def_fab,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_prevact_stone_fab        = list(p_mat = p_trix_pm_touch_skills_prevact_stone_fab,     fun = calculate_pred_log, fold = poss_folds),
  
  TPM_skills_prevseq_fab =        list(p_mat = p_trix_pm_touch_skills_prevseq_fab, fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_prevseq_def_fab        = list(p_mat = p_trix_pm_touch_skills_prevseq_def_fab,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_prevseq_stone_fab        = list(p_mat = p_trix_pm_touch_skills_prevseq_stone_fab,     fun = calculate_pred_log, fold = poss_folds),
  
  TPM_skills_phitters_fab        = list(p_mat = p_trix_pm_touch_skills_phitters_fab,     fun = calculate_pred_log, fold = poss_folds),
  TPM_skills_phitters_def_fab        = list(p_mat = p_trix_pm_touch_skills_phitters_def_fab,     fun = calculate_pred_log, fold = poss_folds)
  
)

pred_df_init <- imap(pred_jobs, ~ .x$fun(.x$p_mat, .x$fold, .y))
pred_df <- imap(pred_df_init, ~{ .x$p_mat <- pred_jobs[[.y]]$p_mat; .x })

save(pred_df, file = "pred_df.RData")
