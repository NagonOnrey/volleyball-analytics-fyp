library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)

load("pred_df.RData")

mse_results <- map_dfr(names(pred_df), function(name) {
  job <- pred_df[[name]]
  
  if (grepl("TPM", name)) {
    # logistic-style models
    y_pos <- job$p_mat$Y_pos
    y_neg <- job$p_mat$Y_neg
    p_pos <- job$pos_pred
    p_neg <- job$neg_pred
    
    logLik_model_pos <- sum(y_pos * log(p_pos + 1e-9) + (1 - y_pos) * log(1 - p_pos + 1e-9))
    logLik_model_neg <- sum(y_neg * log(p_neg + 1e-9) + (1 - y_neg) * log(1 - p_neg + 1e-9))
    
    # null models with mean outcome only
    logLik_null_pos <- sum(y_pos * log(mean(y_pos)) + (1 - y_pos) * log(1 - mean(y_pos)))
    logLik_null_neg <- sum(y_neg * log(mean(y_neg)) + (1 - y_neg) * log(1 - mean(y_neg)))
    
    n_pos <- length(y_pos)
    n_neg <- length(y_neg)
    
    # McFadden pseudo-R²
    pseudoR2_pos <- 1 - (logLik_model_pos / logLik_null_pos)
    pseudoR2_neg <- 1 - (logLik_model_neg / logLik_null_neg)
    
    
    tibble(
      model   = name,
      r2_pos = pseudoR2_pos,
      r2_neg = pseudoR2_neg
    )
    
  } else {
    # linear models unchanged
    Y <- job$p_mat$Y
    pred <- job$pred
    r2 <- 1 - sum((Y - pred)^2, na.rm = TRUE) / sum((Y - mean(Y, na.rm = TRUE))^2, na.rm = TRUE)
    
    tibble(model = name, r2 = r2)
  }
})


mse_results_inter <- mse_results %>%
  mutate(
    r2net = case_when(
      !is.na(r2_pos) & !is.na(r2_neg) ~ (r2_pos + r2_neg) / 2, # both available → average
      !is.na(r2_pos) & is.na(r2_neg)  ~ r2_pos,                # only pos → use it
      is.na(r2_pos) & !is.na(r2_neg)  ~ r2_neg,                # only neg → use it
      TRUE                            ~ r2                     # both NA → use base r2
    )
  )

calibrate_jobs <- function(job, job_name = NULL, bins = 100){
  is_logistic <- grepl("TPM|touch", job_name, ignore.case = TRUE)
  
  if(is_logistic){
    pred_list <- list(pos = job$pos_pred, neg = job$neg_pred)
    Y_list    <- list(pos = job$p_mat$Y_pos, neg = job$p_mat$Y_neg)  # <- use Y_neg properly
    
    calib <- imap_dfr(pred_list, ~ {
      pred <- as.numeric(.x)
      Y    <- as.numeric(Y_list[[.y]])
      stopifnot(length(pred) == length(Y))
      bin  <- cut(pred, breaks = seq(0,1,length.out=bins+1), include.lowest=TRUE)
      
      tibble(
        bin       = bin,
        mean_pred = tapply(pred, bin, mean, na.rm=TRUE)[bin],
        obs_rate  = tapply(Y, bin, mean, na.rm=TRUE)[bin],
        n_obs     = tapply(Y, bin, length)[bin],
        type      = .y          # <- correctly assigns "pos" or "neg"
      )
    })
    
  } else {
    pred <- as.numeric(job$pred)
    Y    <- as.numeric(job$p_mat$Y)
    bin  <- cut(pred, breaks = seq(-1,1,length.out=bins+1), include.lowest=TRUE)
    
    stopifnot(length(pred) == length(Y))
    
    calib <- tibble(
      bin       = bin,
      mean_pred = tapply(pred, bin, mean, na.rm=TRUE)[bin],
      obs_rate  = tapply(Y, bin, mean, na.rm=TRUE)[bin],
      n_obs     = tapply(Y, bin, length)[bin],
      type      = "linear"
    )
  }
  
  calib <- calib %>% distinct()
  return(calib)
}

calibration_df <- imap_dfr(pred_df, ~ {
  calib <- calibrate_jobs(.x, job_name = .y)   # pass the name
  calib$model <- .y
  calib
})

mse_calib <- calibration_df %>%
  group_by(model, type) %>%
  summarise(
    mse_calib = sum(n_obs * (mean_pred - obs_rate)^2, na.rm = TRUE) / sum(n_obs, na.rm = TRUE),
    var_obs   = var(obs_rate, na.rm = TRUE),
    mse_calib_norm = mse_calib / var_obs,
    r2_calib  = 1 - mse_calib / var_obs,     # <- calibration R²
    .groups = "drop"
  ) %>%
  mutate(type = case_when(
    type == "pos" ~ "pos",
    type == "neg" ~ "neg",
    type == "linear" ~ "net",
    TRUE ~ type
  )) %>%
  pivot_wider(
    id_cols = model,
    names_from = type,
    values_from = c(mse_calib, mse_calib_norm, r2_calib),
    names_sep = "_"
  ) %>%
  mutate(
    mse_calib_net = coalesce(mse_calib_net, (mse_calib_pos + mse_calib_neg)/2),
    mse_calib_norm_net = coalesce(mse_calib_norm_net, (mse_calib_norm_pos + mse_calib_norm_neg)/2),
    r2_calib_net = coalesce(r2_calib_net, (r2_calib_pos + r2_calib_neg)/2)
  )

mse_net <- left_join(mse_results_inter, mse_calib, by = "model")

mse_net_table <- mse_net %>%
  mutate(
    fab_status = case_when(
      str_detect(model, "_fab") ~ "FAB",
      TRUE ~ "Non-FAB"
    ),
    base_model = str_remove(model, "_fab$")
  ) %>%
  select(base_model, fab_status, r2net, r2_calib_net) %>%
  rename(
    R2 = r2net,
    R2_Calib = r2_calib_net
  ) %>%
  pivot_wider(
    names_from = fab_status,
    values_from = c(R2, R2_Calib),
    names_sep = "_"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x * 100, 2)),
    across(everything(), ~ ifelse(is.na(.x), "", .x))
  )

mse_net_table %>%
  kable(format = "latex", booktabs = TRUE, digits = 2, escape = TRUE, caption = "The $R^2$ value of all models in terms of predicting outcomes and calibration") %>%
  save_kable("Exports/mse_net_table.tex")


# Plotting  ---------------------------------------------------------------
#All plots
ggplot(calibration_df, aes(x = mean_pred, y = obs_rate, color = type)) +
  geom_point(aes(size = n_obs), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~model, scales = "free_x") +
  labs(color = "Type") +
  theme_minimal()

# Just one plot

target_model <- "TPM_skills"

# subset calibration data for that model
calib_single <- calibration_df %>%
  filter(model == target_model)

# plot calibration curve
single_calibration_curve <- ggplot(calib_single, aes(x = mean_pred, y = obs_rate, color = type)) +
  geom_point(aes(size = n_obs), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_size_continuous(range = c(1, 5)) +
  labs(
    title = paste("Calibration Curve for Skill-Touch Plus-Minus"),
    x = "Predicted Probability (binned mean)",
    y = "Observed Rate",
    color = "Type"
  ) +
  theme_minimal(base_size = 13)

plot(single_calibration_curve)

ggsave(
  filename = paste0("Exports/CALIBRATION_", target_model, ".png"),
  plot = single_calibration_curve,    
  width = 10, height = 6, dpi = 300         
)



# Prediction SD -----------------------------------------------------------

sd(pred_df$RAPM_hitting_sadj$pred)
sd(pred_df$TPM_skills$pos_pred)

