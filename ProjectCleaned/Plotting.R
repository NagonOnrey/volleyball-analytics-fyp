library(glmnet)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(purrr)

# Might be a good idea to have a weighted average and compare that to the team's overall net rating.
# See how well it lines up. 

load("player_vault.RData")

plot_df <- player_vault %>%
  mutate(
    # total_attacks = rowSums(across(starts_with("Attack_")), na.rm = TRUE),
    # total_passes = rowSums(across(starts_with("Dig_") | starts_with("Reception_")), na.rm = TRUE),
    # aces = `Serve_#`,
    # kills = `Attack_#`,
    # hitting_efficiency = (`Attack_#` - `Attack_=` - `Attack_/`)/total_attacks,
    # trad_points = (aces + kills + `Block_#`), 
    # trad_point_rate = (aces + kills + `Block_#`)/num_touches,
    
    # good_passing = `Reception_#` + `Reception_+` + `Dig_#` + `Dig_+`,
    # bad_passing = `Reception_=` + `Reception_/` + `Dig_/` + `Dig_=`,
    # good_passing_rate = good_passing/(total_passes),
    # bad_passing_rate = bad_passing/(total_passes),
    
    label = paste0(player_id, "\n", role),
    highlight = player_id=="SOU-DAR-02"
  ) %>%
  filter(
    `number_RAPM_raw|BASE` > 100,
    #total_attacks > 100
  )


# Plotting ----------------------------------------------------------------

model <- lm(skill_points_added_total ~ role_points_added_total, data = plot_df)
summary(model)$sigma

ggplot(plot_df, aes(x = RAPM_raw_sadj_points_added_total, y = `number_RAPM_raw|BASE`, color = highlight)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = label), size = 3, max.overlaps = 100) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "blue")) +
  labs(
    title = "Vs Graph",
    subtitle = "Labeled with Player ID, Position, and Team"
  ) +
  theme_minimal()
