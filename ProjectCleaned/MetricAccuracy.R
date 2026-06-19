library(glmnet)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(purrr)
library(kableExtra)
library(stringr)

load("player_vault.RData")
load("team_stats.Rdata") 

# players_readable <- player_summary %>%
#   select(player_id, team, role, num_touches, relativeTPM_Net)

vault_readable <- player_vault %>%
  select(player_id, role, team, contains("points_added_total"))

teams_players <- player_vault %>%
  group_by(team) %>%
  summarise(across(contains("points_added_total"), ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  rename_with(~str_replace(.x, "_points_added_total", ""), contains("points_added_total")) %>%
  left_join(team_stats, by = "team") %>%
  mutate(
    label = team,
    srs_points_added = srs * sets_played
  )



# Finding correlation between metric and team quality ---------------------

points_cols <- teams_players %>%
  select(contains("PM")) %>%
  names()

accuracy_results <- lapply(points_cols, function(col) {
  model <- lm(as.formula(paste("srs_points_added ~", col)), data = teams_players)
  s <- summary(model)
  data.frame(
    model = col,
    R_squared = s$r.squared,
    Coefficient = 1/s$coefficients[2, 1]
  )
})


accuracy_df <- do.call(rbind, accuracy_results) %>%
  select(model, R_squared)


# Table for Display

team_success_table_fab <- accuracy_df %>%
  mutate(
    fab_status = case_when(
      str_detect(model, "_fab") ~ "FAB",
      TRUE                      ~ ""
    ),
    type = case_when(
      str_ends(model, "_role")  ~ "RB",
      str_ends(model, "_skill") ~ "SB",
      str_detect(model, "_role_")  ~ "RB",
      str_detect(model, "_skill_") ~ "SB",
      TRUE ~ "RB"
    ),
    base_model = str_remove(model, "_fab$|_role$|_skill$|_fab_role$|_fab_skill$")
  ) %>%
  select(base_model, type, fab_status, R_squared) %>%
  pivot_wider(
    names_from  = c(type, fab_status),
    values_from = R_squared,
    names_sep   = " "
  )

team_success_table <- team_success_table_fab %>%
  mutate(across(where(is.numeric), ~ round(.x * 100, 2))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

# Export to LaTeX
team_success_table %>%
  kable(format = "latex", booktabs = TRUE, digits = 2, escape = TRUE, caption = "The $R^2$ value of all models in relation to predicting team success") %>%
  save_kable("Exports/team_success_table.tex")



# Plotting Just One Model -------------------------------------------------

teams_players_anon <- teams_players %>%
  mutate(team_letter = substr(team, 1, 1))

model_to_compute <- "RAPM_raw"

team_success_single_plot <- ggplot(teams_players_anon, aes(x = srs_points_added, y = .data[[model_to_compute]])) +
  geom_point(color = "red", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black",
              linewidth = 0.9, linetype = "dotted") +
  geom_text_repel(aes(label = team_letter), size = 3.2, max.overlaps = 25) +
  labs(
    title = paste("Predicted vs Actual Team Success: RAPM"),
    x = "Actual Team Success",
    y = "Predicted Team Success"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85")
  ) +
  # Draw x/y axes manually since theme_minimal removes them
  theme(panel.border = element_blank()) +
  coord_cartesian(clip = "off")

plot(team_success_single_plot)

ggsave(
  filename = "Exports/team_success_plots_RAPM_raw.png",
  plot = team_success_single_plot,    
  width = 10, height = 6, dpi = 300         
)

# Plotting team success vs models -----------------------------------------

# Get all model columns
points_cols <- teams_players %>%
  select(contains("PM")) %>%
  names()

# Pivot all models long
plot_df <- teams_players %>%
  select(team, srs_points_added, all_of(points_cols)) %>%
  pivot_longer(cols = all_of(points_cols), names_to = "model", values_to = "predicted") %>%
  # normalize each model and actual srs_points_added
  group_by(model) %>%
  mutate(
    predicted_scaled = (predicted - mean(predicted, na.rm = TRUE)) / sd(predicted, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    actual_scaled = (srs_points_added - mean(srs_points_added)) / sd(srs_points_added)
  )

# Plot all models
team_success_plots_ALLMODELS <- ggplot(plot_df, aes(x = actual_scaled, y = predicted_scaled, color = model)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  labs(
    title = "Normalized Model Predictions vs Actual Team Success",
    x = "Actual Team Success (scaled)",
    y = "Predicted Team Success (scaled)",
    color = "Model"
  ) +
  theme_minimal(base_size = 13) + 
  scale_color_viridis_d(option = "plasma") +
  theme(legend.position = "none")

plot(team_success_plots_ALLMODELS)

ggsave(
  filename = "Exports/team_success_plots_ALLMODELS.png",
  plot = team_success_plots_ALLMODELS,    
  width = 10, height = 6, dpi = 300         
)
