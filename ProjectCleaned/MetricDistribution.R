library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(purrr)
library(plotly)
library(stringr)
library(GGally)
library(knitr)
library(kableExtra)
library(reshape2)
library(stringr)

load("player_vault.RData")
load("role_avgs.RData")

skill_names <- c("Serve","Reception","Set","Attack", "Block","Dig", "Freeball")

cols <- names(player_vault)

# Combining front and back in player vault --------------------------------


combine_front_back_columns <- function(df) {
  nm <- names(df)
  
  # Find all columns that have _front or _back
  fb_cols <- nm[str_detect(nm, "_front|_back")]
  
  # Create base names (remove _front/_back)
  base_names <- str_remove(fb_cols, "_front|_back")
  
  # Group front/back variants
  groups <- split(fb_cols, base_names)
  
  for (bn in names(groups)) {
    cols <- groups[[bn]]
    
    # Only take numeric columns
    vals <- lapply(cols, function(cn) {
      if (is.numeric(df[[cn]])) df[[cn]] else rep(0, nrow(df))
    })
    
    # Skip if no valid columns
    if (length(vals) == 0) next
    
    # Replace NAs with 0 and sum them together
    mat <- do.call(cbind, vals)
    mat[is.na(mat)] <- 0
    df[[bn]] <- rowSums(mat)
  }
  
  # Remove the original _front/_back columns
  df <- df %>% select(-any_of(fb_cols))
  
  return(df)
}

player_vault_combined <- combine_front_back_columns(player_vault)

aous <- player_vault_combined %>%
  select(`TPM_skills_prevact_def_fab_role|Attack_points_added`)

blundsa <- player_vault %>%
  select(`TPM_skills_prevact_def_fab_role_front|Attack_points_added`, `TPM_skills_prevact_def_fab_role_back|Attack_points_added`)




# Role Share -----------------------------------------------------------

role_poss <- player_vault %>%
  group_by(role) %>%
  summarise(
    total_possession = sum(`number_RAPM_raw|BASE`, na.rm = TRUE)
  )

# Has each role and the variance for its metrics. 
role_dist <- player_vault %>%
  group_by(role) %>%
  summarise(
    across(
      contains("points_added_total"),
      list(
        absum = ~sum(abs(.x), na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  left_join(role_poss, by = "role") %>%
  mutate(
    across(contains("absum"), ~ .x / total_possession * 100)
  ) %>%
  select(-total_possession)


role_dist_flipped <- role_dist %>%
  pivot_longer(
    cols = -role,
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = role,
    values_from = value
  )

# Gets the percentage of variance. 
role_dist_flipped_norm <- role_dist_flipped %>%
  rowwise() %>%
  mutate(
    across(
      where(is.numeric),
      ~ {
        vals <- c_across(where(is.numeric))
        total <- sum(vals, na.rm = TRUE)
        if (total == 0 || is.na(total)) NA_real_ else .x / total
      }
    )
  ) %>%
  ungroup() %>%
  rename(model = stat)

role_dist_clean <- role_dist_flipped_norm %>%
  mutate(
    model_clean = str_remove_all(model, "_role"),
    model_clean = str_remove_all(model_clean, "_points_added"),
    model_clean = str_remove_all(model_clean, "_total"),
    model_clean = str_remove_all(model_clean, "_absum"),
    model_clean = str_remove_all(model_clean, "_rel"),
    modifier_list = str_split(model_clean, "_|\\|")
  ) %>%
  filter(!map_lgl(modifier_list, ~ any(str_detect(.x, regex(paste(c("\\bskill(?!s)\\b","Serve","Reception","Set","Attack","Hit","Block","Dig","Freeball"),collapse = "|"))))
  )
  ) %>%
  
  mutate(
    modifier_0 = map_chr(modifier_list, ~ .x[1] %||% NA), 
    modifier_1 = map_chr(modifier_list, ~ .x[2] %||% NA), 
    modifier_2 = map_chr(modifier_list, ~ .x[3] %||% NA), 
    modifier_3 = map_chr(modifier_list, ~ .x[4] %||% NA), 
    modifier_4 = map_chr(modifier_list, ~ .x[5] %||% NA),
    modifier_5 = map_chr(modifier_list, ~ .x[6] %||% NA)
  )



role_share_table <- role_dist_clean %>%
  mutate(across(c(libero, middle, opposite, outside, setter), ~round(.x*100, 1)),
         across(c(libero, middle, opposite, outside, setter), ~ifelse(is.na(.x), "", as.character(.x)))) %>%
  select(-model) %>%
  rename(model = model_clean,
         Libero = libero,
         Middle = middle,
         Opposite = opposite,
         Outside = outside,
         Setter = setter
  ) %>%
  select(model, Libero, Middle, Opposite, Outside, Setter) %>%
  filter(!str_detect(model, "fab"))

role_share_table %>%
  kable(format = "latex", booktabs = TRUE, longtable = TRUE, escape = TRUE, caption = "Percentage of points explained by role for each model") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/role_share_table.tex")


View(TPM)

# Skill Share -----------------------------------------------------------

# Getting the number of touches for each skill - might help. 

skill_pos_row <- aggregate_plays %>%
  count(skill) %>%
  filter(skill %in% setdiff(skill_names, "Hit")) %>%
  mutate(prop = n / sum(n)) %>%
  select(skill, prop) %>%
  pivot_wider(names_from = skill, values_from = prop) %>%
  mutate(
    across(everything(), ~ round(.x * 100, 1)),
    across(where(is.numeric), as.character),
    model_modifiers = "% of Touches",
  ) %>%
  select(model_modifiers, setdiff(skill_names, "Hit"))



metric_cols <- names(player_vault)[str_detect(names(player_vault), "points_added")]

# Only columns that contain any typical skill string
skill_cols_matching <- metric_cols[str_detect(metric_cols, paste(c("Serve","Reception","Set","Attack","Hit","Block","Dig"), collapse = "|"))]
model_order <- str_extract(skill_cols_matching, "^[^|]+") %>% unique()

# Compute absum per model per extracted "skill label"
absum_skill_per_model <- map_df(model_order, function(model_prefix) {
  
  # Columns for this model
  model_cols <- metric_cols[str_starts(metric_cols, model_prefix)]
  
  map_df(model_cols, function(col_name) {
    
    # Extract the "skill label" as the text after "|" and before the first "_" or end of string
    skill_label <- str_extract(col_name, "(?<=\\|)[^_]+")  
    if (is.na(skill_label)) skill_label <- "NAn"  # fallback if pattern doesn't match
    
    # Absolute sum
    absum_val <- player_vault %>%
      pull(col_name) %>% 
      abs() %>%
      sum(na.rm = TRUE)
    
    tibble(
      skill = skill_label,
      absum = absum_val,
      model = model_prefix
    )
  })
})

# Normalize absum per model
absum_skill_per_model_norm <- absum_skill_per_model %>%
  filter(skill != "NAn") %>%
  group_by(model) %>%
  mutate(
    absum_norm = absum / sum(absum[!skill %in% c("DEF")], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    skill = factor(skill, levels = unique(skill)),
    model = factor(model, levels = model_order)
  )


absum_skill_per_model_long <- absum_skill_per_model_norm %>%
  mutate(
    model_clean = str_remove(model, "^.+?_skills"),
    model_clean = str_remove_all(model_clean, "_role"),
    modifier_list = str_split(model_clean, "_")
  ) %>%
  filter(
    !map_lgl(modifier_list, ~ any(str_detect(.x, regex("skill", ignore_case = TRUE))))
  ) %>%
  mutate(
    modifier_0 = map_chr(modifier_list, ~ .x[1] %||% NA),
    modifier_1 = map_chr(modifier_list, ~ .x[2] %||% NA),
    modifier_2 = map_chr(modifier_list, ~ .x[3] %||% NA),
    modifier_3 = map_chr(modifier_list, ~ .x[4] %||% NA),
    modifier_4 = map_chr(modifier_list, ~ .x[5] %||% NA)
  ) %>%
  select(-model, -model_clean, -modifier_list, -modifier_0)


absum_skill_per_model_wide <- absum_skill_per_model_long %>%
  select(modifier_1:modifier_4, skill, absum_norm) %>%
  unite("model_modifiers", modifier_1:modifier_4, sep = "_", na.rm = TRUE) %>%
  pivot_wider(
    names_from = skill,
    values_from = absum_norm,
    values_fill = NA
  ) %>%
  mutate(
    Block = ifelse(str_detect(model_modifiers, regex("back", ignore_case = TRUE)), NA, Block),
    Serve = ifelse(str_detect(model_modifiers, regex("front", ignore_case = TRUE)), NA, Serve)
  ) %>%
  select(-DEF) %>%
  mutate(
    across(c(Serve, Reception, Set, Attack, Block, Dig, Freeball, HIT),
           ~ round(.x * 100, 1)),      # keep numeric for now
    across(c(Serve, Reception, Set, Attack, Block, Dig, Freeball, HIT),
           ~ ifelse(is.na(.x), "", as.character(.x)))  # convert NA -> blank
  )

# Renaming everything for presentation

skill_share_table <- skill_pos_row %>%
  bind_rows(absum_skill_per_model_wide)



# I have removed all the just 'skill'
skill_share_table %>%
  kable(format = "latex", booktabs = TRUE, longtable = TRUE, caption = "Proportion of points explained by skill for each model", escape = TRUE) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/skill_share_table.tex")

# Looking at just the defensive/phitters version of this. 

absum_skill_defphit_norm <- absum_skill_per_model %>%
  filter(skill != "NAn") %>%
  group_by(model) %>%
  mutate(
    absum_norm = absum / sum(absum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    skill = factor(skill, levels = unique(skill)),
    model = factor(model, levels = model_order)
  )

absum_skill_defphit_long <- absum_skill_defphit_norm %>%
  mutate(
    model_clean = str_remove(model, "^.+?_skills"),
    model_clean = str_remove_all(model_clean, "_role"),
    modifier_list = str_split(model_clean, "_")
  ) %>%
  filter(
    !map_lgl(modifier_list, ~ any(str_detect(.x, regex("skill", ignore_case = TRUE))))
  ) %>%
  mutate(
    modifier_0 = map_chr(modifier_list, ~ .x[1] %||% NA),
    modifier_1 = map_chr(modifier_list, ~ .x[2] %||% NA),
    modifier_2 = map_chr(modifier_list, ~ .x[3] %||% NA),
    modifier_3 = map_chr(modifier_list, ~ .x[4] %||% NA),
    modifier_4 = map_chr(modifier_list, ~ .x[5] %||% NA)
  ) %>%
  select(-model, -model_clean, -modifier_list, -modifier_0)

DEF_HIT_share_table <- absum_skill_defphit_long %>%
  select(modifier_1:modifier_4, skill, absum_norm) %>%
  unite("model_modifiers", modifier_1:modifier_4, sep = "_", na.rm = TRUE) %>%
  pivot_wider(
    names_from = skill,
    values_from = absum_norm,
    values_fill = NA
  ) %>%
  mutate(
    Block = ifelse(str_detect(model_modifiers, regex("back", ignore_case = TRUE)), NA, Block),
    Serve = ifelse(str_detect(model_modifiers, regex("front", ignore_case = TRUE)), NA, Serve)
  ) %>%
  select(-c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball")) %>%
  filter(str_detect(model_modifiers, regex("def|phitters", ignore_case = TRUE))) %>%
  mutate(
    across(c(DEF, HIT),
           ~ round(.x * 100, 1)),      # keep numeric for now
    across(c(DEF, HIT),
           ~ ifelse(is.na(.x), "", as.character(.x)))  # convert NA -> blank
  )

DEF_HIT_share_table %>%
  kable(format = "latex", booktabs = TRUE, longtable = TRUE, caption = "Proportion of points explained by DEF and HIT for each model", escape = TRUE) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/DEF_HIT_share_table.tex")




# Plot
p <- ggplot(absum_skill_per_model_norm, aes(x = skill, y = absum_norm, fill = skill)) +
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  facet_wrap(~ model, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Skill",
    y = "Normalized absum of Points Added",
    title = "Skill Variability Across All Models"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("skill_variability_models.pdf", width = 48, height = 40)



# RAW and DEF Correlation -------------------------------------------------

metric_cols_combined <- names(player_vault_combined)[str_detect(names(player_vault_combined), "points_added")]
skill_cols_matching_combined <- metric_cols_combined[str_detect(metric_cols_combined, paste(c("Serve","Reception","Set","Attack","Hit","Block","Dig"), collapse = "|"))]
model_order_combined <- str_extract(skill_cols_matching_combined, "^[^|]+") %>% unique()


model_to_include <- "_role"
model_to_exclude <- "_skill"

clean_models <- model_order_combined %>%
  discard(~ str_detect(.x, paste0(model_to_exclude, "($|_)")))

def_models <- clean_models[str_detect(model_order_combined, "def")]

# Identify base vs defense
paired_def_models <- tibble(
  def_model = def_models,
  base_model = str_remove(def_models, "_def")   # corresponding base model
) %>%
  filter(base_model %in% clean_models) %>%
  select(base_model, def_model)

# Correlate defense models
cor_def_results <- map_df(paired_def_models$base_model, function(base_model) {
  
  def_model <- paired_def_models$def_model[paired_def_models$base_model == base_model]
  
  # Get all metric columns for base and def models
  base_cols <- metric_cols_combined[str_detect(metric_cols_combined, paste0("^", base_model, "($|\\|)"))]
  def_cols  <- metric_cols_combined[str_detect(metric_cols_combined, paste0("^", def_model, "($|\\|)"))]
  
  # Extract the skill names (anything after the first "|", if present)
  base_skills <- str_extract(base_cols, "(?<=\\|).*")
  def_skills  <- str_extract(def_cols,  "(?<=\\|).*")
  
  # Keep only skills that appear in both
  common_skills <- intersect(base_skills, def_skills)
  
  # Skip models that have no overlap
  if (length(common_skills) == 0) return(NULL)
  
  tibble(
    Model = base_model,
    Skill = common_skills,
    Correlation = map_dbl(common_skills, ~ {
      base_col <- paste0(base_model, "|", .x)
      def_col  <- paste0(def_model,  "|", .x)
      
      # Try both possible naming variants (pipe or no pipe)
      if (!(base_col %in% names(player_vault_combined))) {
        base_col <- paste0(base_model, "_", .x, "_points_added")
      }
      if (!(def_col %in% names(player_vault_combined))) {
        def_col <- paste0(def_model, "_", .x, "_points_added")
      }
      
      if (all(c(base_col, def_col) %in% names(player_vault_combined))) {
        cor(player_vault_combined[[base_col]], player_vault_combined[[def_col]], use = "pairwise.complete.obs")
      } else {
        NA_real_
      }
    })
  )
})

# Wide format
cor_def_results_wide <- cor_def_results %>%
  pivot_wider(
    names_from = Skill,
    values_from = Correlation
  ) %>%
  mutate(
    # clean Model for display
    Model = str_remove(Model, "_def"),
    Model = str_remove(Model, paste0(model_to_include, "($|_)")),
    Mean_Correlation = rowMeans(select(., -Model), na.rm = TRUE)
  )%>%
  mutate(Model = str_replace(Model, "^TPM_skills_?", ""))

# Long format for plotting
cor_def_results_long <- cor_def_results_wide %>%
  mutate(
    Model = factor(Model, levels = rev(Model))  # reverse the order
  ) %>%
  pivot_longer(
    cols = -c(Model, Mean_Correlation),
    names_to = "Skill",
    values_to = "Correlation"
  )

# Min/max for color scale
corr_min_def <- min(cor_def_results_long$Correlation, na.rm = TRUE)
corr_max_def <- max(cor_def_results_long$Correlation, na.rm = TRUE)

# Heatmap
OPP_VS_BASE_correlation <- ggplot(cor_def_results_long, aes(x = Skill, y = Model, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient(low = "blue", high = "red",
                      limits = c(corr_min_def, corr_max_def)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    title = "Correlation of BASE vs Opponent-Inclusive Models (OPP)",
    x = "Skill",
    y = "Model",
    fill = "Correlation"
  )


plot(OPP_VS_BASE_correlation)

ggsave(
  filename = "Exports/OPP_VS_BASE_correlation.png",
  plot = OPP_VS_BASE_correlation,    
  width = 10, height = 6, dpi = 300         
)



# RAW and STONE Correlation -------------------------------------------------

metric_cols_combined <- names(player_vault_combined)[str_detect(names(player_vault_combined), "points_added")]
skill_cols_matching_combined <- metric_cols_combined[str_detect(metric_cols_combined, paste(c("Serve","Reception","Set","Attack","Hit","Block","Dig"), collapse = "|"))]
model_order_combined <- str_extract(skill_cols_matching_combined, "^[^|]+") %>% unique()

clean_models <- model_order_combined %>%
  discard(~ str_detect(.x, paste0("_skill", "($|_)")))

clean_models

model_order_combined

stone_models <- clean_models[str_detect(clean_models, "_stone")]
stone_models

base_models = str_remove(stone_models, "_stone")
base_models

# Identify base vs defense
paired_stone_models <- tibble(
  stone_model = stone_models,
  base_model = str_remove_all(stone_models, "_stone")   # corresponding base model
) %>%
  filter(base_model %in% clean_models) %>%
  select(base_model, stone_model)

# Correlate stone models
cor_stone_results <- map_df(paired_stone_models$base_model, function(base_model) {
  
  stone_model <- paired_stone_models$stone_model[paired_stone_models$base_model == base_model]
  
  # Get all metric columns for base and stone models
  base_cols <- metric_cols_combined[str_detect(metric_cols_combined, paste0("^", base_model, "($|\\|)"))]
  stone_cols  <- metric_cols_combined[str_detect(metric_cols_combined, paste0("^", stone_model, "($|\\|)"))]
  
  # Extract the skill names (anything after the first "|", if present)
  base_skills <- str_extract(base_cols, "(?<=\\|).*")
  stone_skills  <- str_extract(stone_cols,  "(?<=\\|).*")
  
  # Keep only skills that appear in both
  common_skills <- intersect(base_skills, stone_skills)
  
  # Skip models that have no overlap
  if (length(common_skills) == 0) return(NULL)
  
  tibble(
    Model = base_model,
    Skill = common_skills,
    Correlation = map_dbl(common_skills, ~ {
      base_col <- paste0(base_model, "|", .x)
      stone_col  <- paste0(stone_model,  "|", .x)
      
      # Try both possible naming variants (pipe or no pipe)
      if (!(base_col %in% names(player_vault_combined))) {
        base_col <- paste0(base_model, "_", .x, "_points_added")
      }
      if (!(stone_col %in% names(player_vault_combined))) {
        stone_col <- paste0(stone_model, "_", .x, "_points_added")
      }
      
      if (all(c(base_col, stone_col) %in% names(player_vault_combined))) {
        cor(player_vault_combined[[base_col]], player_vault_combined[[stone_col]], use = "pairwise.complete.obs")
      } else {
        NA_real_
      }
    })
  )
})

# Wide format
cor_stone_results_wide <- cor_stone_results %>%
  pivot_wider(
    names_from = Skill,
    values_from = Correlation
  ) %>%
  mutate(
    # clean Model for display
    Model = str_remove(Model, "_stone"),
    Model = str_remove(Model, paste0("_role", "($|_)")),
    Mean_Correlation = rowMeans(select(., -Model), na.rm = TRUE)
  )%>%
  mutate(Model = str_replace(Model, "^TPM_skills_?", ""))

# Long format for plotting
cor_stone_results_long <- cor_stone_results_wide %>%
  mutate(
    Model = factor(Model, levels = rev(Model))  # reverse the order
  ) %>%
  pivot_longer(
    cols = -c(Model, Mean_Correlation),
    names_to = "Skill",
    values_to = "Correlation"
  )

# Min/max for color scale
corr_min_stone <- min(cor_stone_results_long$Correlation, na.rm = TRUE)
corr_max_stone <- max(cor_stone_results_long$Correlation, na.rm = TRUE)

# Heatmap
ggplot(cor_stone_results_long, aes(x = Skill, y = Model, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient(low = "blue", high = "red",
                      limits = c(corr_min_stone, corr_max_stone)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    title = "Correlation of Base vs stone-Included Models for role relative",
    x = "Skill",
    y = "Model",
    fill = "Correlation"
  )




# Summary Tables for Correlations -----------------------------------------

# --- BASE vs DEFENSE ---
AVG_correlation_BASE_VS_DEF <- cor_def_results %>%
  group_by(Skill) %>%
  summarise(Average_Correlation = mean(Correlation, na.rm = TRUE)) %>%
  pivot_wider(
    names_from = Skill,
    values_from = Average_Correlation
  ) %>%
  mutate(across(everything(), round, 3)) %>%
  rename_with(~ str_remove(.x, "_points_added$")) %>%
  select(Serve, Reception, Set, Attack, Block, Dig, Freeball, HIT)

# --- EXPORT ---
AVG_correlation_BASE_VS_DEF %>%
  kable(format = "latex", booktabs = TRUE,
        caption = "Average Skill Correlations — Base vs Defense Models", escape = TRUE) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/AVG_correlation_BASE_VS_DEF.tex")



# ROLE and SKILL Correlation base model -----------------------------------

model_a <- "TPM_skills_role"
model_b <- "TPM_skills_skill"

BASE_cor_rolewise <- player_vault_combined %>%
  filter(!is.na(role)) %>%
  group_split(role) %>%
  set_names(unique(player_vault_combined$role)) %>%
  map_dfr(function(role_df) {
    role_name <- unique(role_df$role)
    cor_values <- map_dfr(skill_names, function(skill) {
      col_a <- paste0(model_a, "|", skill, "_points_added")
      col_b <- paste0(model_b, "|", skill, "_points_added")
      
      if (all(c(col_a, col_b) %in% names(role_df))) {
        tibble(
          Skill = skill,
          Correlation = cor(role_df[[col_a]], role_df[[col_b]], use = "pairwise.complete.obs")
        )
      } else {
        tibble(Skill = skill, Correlation = NA_real_)
      }
    })
    
    cor_values %>% mutate(role = role_name)
  })

BASE_correlation_ROLE_VS_SKILL <- BASE_cor_rolewise %>%
  pivot_wider(
    names_from = Skill,
    values_from = Correlation
  ) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  arrange(role)


BASE_correlation_ROLE_VS_SKILL %>%
  kable(format = "latex", booktabs = TRUE, escape = TRUE, caption = "Correlation between ROLE and SKILL for TPM skills") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/BASE_correlation_ROLE_VS_SKILL.tex")


quick_avg_look <- role_avgs %>%
  select(role, matches("TPM_skills_(pos|neg)"))

quick_avg_look

thingo <- aggregate_plays %>%
  filter(role == "libero") %>%
  filter(skill == "Block")

thingo2 <- aggregate_plays %>%
  filter(match_id == 1103815, point_id==158)


# DEF and HIT vs RAPM Correlations ------------------------------------------------

# 1️⃣ Identify columns
def_col <- "TPM_skills_def_role|DEF_points_added"
hit_col <- "TPM_skills_phitters_role|HIT_points_added"

# all RAPM-type columns
rapm_cols <- names(player_vault_combined)[
  # everything ending with points_added_total
  str_detect(names(player_vault_combined), "RAPM_raw_sadj_points_added_total|RAPM_raw_points_added_total") |
    
    # the base hitting / hitting_sadj DEF and OFF
    str_detect(names(player_vault_combined),
               "RAPM_hitting(_sadj)\\|(DEF|OFF)_role_rel_points_added$") | 
    str_detect(names(player_vault_combined),
               "RAPM_hitting(_sadj)\\|(DEF|OFF)_role_rel_points_added$")
]
rapm_cols

calc_corrs <- function(base_col, label) {
  map_dfr(rapm_cols, function(rc) {
    tibble(
      model = rc,
      TPM_metric = label,
      Correlation = cor(
        player_vault_combined[[base_col]],
        player_vault_combined[[rc]],
        use = "pairwise.complete.obs"
      )
    )
  }) %>%
    mutate(Correlation = round(Correlation, 3))
}

cor_def <- calc_corrs(def_col, "DEF")
cor_hit <- calc_corrs(hit_col, "HIT")

cor_combined <- bind_rows(cor_def, cor_hit)

cor_grouped <- cor_combined %>%
  mutate(
    base_model = str_remove(model, "_points_added_total$"),
    base_model = str_remove(base_model, "_role_rel_points_added$")
  ) %>%
  select(base_model, TPM_metric, Correlation) %>%
  pivot_wider(
    names_from  = c(TPM_metric),
    values_from = Correlation
  )

OPP_HIT_VS_DEF_OFF_correlation_table <- cor_grouped %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

OPP_HIT_VS_DEF_OFF_correlation_table %>%
  kable(format = "latex", booktabs = TRUE, escape = TRUE, caption = "Correlation between DEF, HIT and RAPM Metrics") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/OPP_HIT_VS_DEF_OFF_correlation_table.tex")



# DEF AND HIT AND DEF AND OFF CORRELATIONS --------------------------------


selected_cols <- c(
  "TPM_skills_def_role|DEF_points_added",
  "TPM_skills_phitters_role|HIT_points_added",
  "RAPM_hitting_sadj|DEF_role_rel_points_added",
  "RAPM_hitting_sadj|OFF_role_rel_points_added",
  "RAPM_raw_sadj_points_added_total"
)

corr_df <- player_vault_combined %>%
  select(all_of(selected_cols)) %>%
  rename(
    `STPM OPP`   = "TPM_skills_def_role|DEF_points_added",
    `STPM HIT`   = "TPM_skills_phitters_role|HIT_points_added",
    `RAPM DEF`  = "RAPM_hitting_sadj|DEF_role_rel_points_added",
    `RAPM OFF`  = "RAPM_hitting_sadj|OFF_role_rel_points_added",
    `RAPM Net`  = "RAPM_raw_sadj_points_added_total"
  )

corr_matrix <- cor(corr_df, use = "pairwise.complete.obs")

corr_long <- reshape2::melt(corr_matrix)

PRESENCE_correlations <- ggplot(corr_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-1, 1)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    fill = "Correlation",
    title = "Correlation Between STPM and RAPM Models"
  )

plot(PRESENCE_correlations)

ggsave(
  filename = "Exports/PRESENCE_correlations.png",
  plot = PRESENCE_correlations,    
  width = 10, height = 6, dpi = 300         
)


# Lambda Plots ------------------------------------------------------------

Ridge_RAPM_raw <- attr(RAPM_raw, "cv_results")
Ridge_RAPM_raw_sadj <- attr(RAPM_raw_sadj, "cv_results")

png("Exports/Ridge_RAPM_raw.png", width = 1000, height = 600, res = 150)
plot(Ridge_RAPM_raw)
dev.off()

png("Exports/Ridge_RAPM_raw_sadj.png", width = 1000, height = 600, res = 150)
plot(Ridge_RAPM_raw_sadj)
dev.off()


# Biggest difference between FAB ------------------------------------------

biggest_change_rapm_fab <- player_vault %>%
  select(player_id, team, role, `RAPM_raw_sadj_fab_back|BASE_role_rel`, `RAPM_raw_sadj_fab_front|BASE_role_rel`, `number_RAPM_raw_sadj_fab_back|BASE`, `number_RAPM_raw_sadj_fab_front|BASE`) %>%
  mutate(diff = `RAPM_raw_sadj_fab_front|BASE_role_rel` - `RAPM_raw_sadj_fab_back|BASE_role_rel`) %>%
  mutate(across(where(is.numeric), ~ .x * 100))

# J.Finoli is the player I reference as 'Henry'. 
# Replacing them should give +0.24 points per set. 

srs_win_model <- lm(win_pct ~ srs, data = team_stats)
summary(srs_win_model)
0.071057*0.24*100

dim(hit_based_segments)
dim(point_based_segments)
dim(touch_based_segments_raw)


# MISC --------------------------------------------------------------------

# Prevact/prevseq and defense
cor(player_vault$TPM_skills_role_points_added_total, player_vault$TPM_skills_def_role_points_added_total, use = "complete.obs")
cor(player_vault$TPM_skills_prevact_role_points_added_total, player_vault$TPM_skills_def_role_points_added_total, use = "complete.obs")
cor(player_vault$TPM_skills_prevseq_role_points_added_total, player_vault$TPM_skills_def_role_points_added_total, use = "complete.obs")

cor(player_vault$TPM_skills_prevact_stone_role_points_added_total, player_vault$TPM_skills_prevseq_role_points_added_total, use = "complete.obs")

cor(player_vault$TPM_skills_role_points_added_total, player_vault$TPM_skills_phitters_role_points_added_total, use = "complete.obs")

cor(player_vault$TPM_skills_role_points_added_total, player_vault$`TPM_skills_def_role|DEF_points_added`, use = "complete.obs")


#Prevseq and the normal values
cor(player_vault$`TPM_skills_role|Serve_points_added`, player_vault$`TPM_skills_prevseq_role|Serve_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Reception_points_added`, player_vault$`TPM_skills_prevseq_role|Reception_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Set_points_added`, player_vault$`TPM_skills_prevseq_role|Set_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Attack_points_added`, player_vault$`TPM_skills_prevseq_role|Attack_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_roloe|Block_points_added`, player_vault$`TPM_skills_prevseq_role|Block_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Dig_points_added`, player_vault$`TPM_skills_prevseq_role|Dig_points_added`, use = "complete.obs")

#Phitters and the normal values
cor(player_vault$`TPM_skills_role|Serve_points_added`, player_vault$`TPM_skills_phitters_role|Serve_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Reception_points_added`, player_vault$`TPM_skills_phitters_role|Reception_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Set_points_added`, player_vault$`TPM_skills_phitters_role|Set_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Attack_points_added`, player_vault$`TPM_skills_phitters_role|HIT_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Block_points_added`, player_vault$`TPM_skills_phitters_role|Block_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Dig_points_added`, player_vault$`TPM_skills_phitters_role|Dig_points_added`, use = "complete.obs")





# DEF between models. 
cor(player_vault$`TPM_skills_def_role|DEF_points_added`, player_vault$`TPM_skills_phitters_def_role|DEF_points_added`, use = "complete.obs")

# BASE vs DEF adjusted model 
cor(player_vault$`TPM_skills_role|Attack_points_added`, player_vault$`TPM_skills_def_role|Attack_points_added`, use = "complete.obs")



# Comparing role vs skill baseline
cor(player_vault$`TPM_skills_role|Attack_points_added`, player_vault$`TPM_skills_skill|Attack_points_added`, use = "complete.obs")


# Player Award Stuff ------------------------------------------------------

# LAST YEAR

# Best By Position
# Patry = Opposite.
# Toniutti = Setter (Good RAPM - 60. AWFUL setting value)
# Bieniek = Middle (Good RAPM - 40. Neutral touch value)
# Huber = Middle (Neutral RAPM - 10. Great touch value)

# Kwolek = Outside (Decent RAPM - 20. Nearly WORST touch value).

# Best By Skill
# Souza - not in here.
# Butryn - Server. (16 serve points added; 4th. Which is good)
# Rossard - Reception - not in here.
# Rajsner - Spiker (like 5 attacking points added)

# YEAR OF DATASET

# Skill/Overall
# Venero (Leon) (Tied highest RAPM at +90; best serve; has a -6, but with prevact is a +10 and like 8th or so; TPM is like...15th or something)
# Indra - Scorer (not in here...)
# Vasina - Receiver (not in here...)
# Huber - Blocker (Very good! +26 or 3rd)
# Brehme - Spiker (+41! Very good. 5th. If you take it skill relative he's 2nd at like +67. But he's a middle so...)

# Position
# Butryn - Opposite (+40. Good! -40 in TPM skills though...)
# Rodriguez - Setter (+40. Good! +193 in TPM skills first by FAR!)
# Kochanowski - Middle (+32. Good! Neutral in TPM...)
# Huber - Middle (+10. Just ok. +40 in TPM which is ok)
# Perry - Libero (Good RAPM - 40. -6 in TPM)
# Fornal - Outside (+30! +140 touch value; 2nd)
# Venero - Outside (+90! And MVP! +40 in TPM which is ok)

# Also want to make a note of how Toniutti and Rodriguez both have good RAPM.
# But Rodriguez has the best setting and Toniuitti has the worst setting by FAR. And like. Outlying far. It's stupid. 


player_analysis <- player_vault %>%
  select(player_id, role, team, RAPM_hitting_sadj_points_added_total, `RAPM_hitting_sadj|OFF_role_rel_points_added`, `RAPM_hitting_sadj|DEF_role_rel_points_added`)

vault_points <- player_vault %>%
  select(player_id, role, team, contains("points_added"))





player_awards <- player_vault %>%
  select(player_id, role, team, RAPM_raw_sadj_points_added_total, TPM_skills_role_points_added_total, TPM_skills_prevseq_role_points_added_total, TPM_skills_phitters_role_points_added_total, TPM_skills_prevact_stone_role_points_added_total, `TPM_skills_def_role|DEF_points_added`) %>%
  rename(RAPM_TOTAL = RAPM_raw_sadj_points_added_total, SKILLS_TOTAL = TPM_skills_role_points_added_total, PREVSEQ_TOTAL = TPM_skills_prevseq_role_points_added_total, PHITTERS_TOTAL = TPM_skills_phitters_role_points_added_total, PREVACT_S_TOTAL = TPM_skills_prevact_stone_role_points_added_total) %>%
  left_join(points_played_per_set, by = "player_id") %>%
  mutate(RAPM = RAPM_TOTAL/points_played*50, SKILLS = SKILLS_TOTAL/points_played*50, PREVSEQ = PREVSEQ_TOTAL/points_played*50, PHITTERS = PHITTERS_TOTAL/points_played*50, PREVACT_S = PREVACT_S_TOTAL/points_played*50)


player_awards_ranked <- player_awards %>%
  filter(points_played >= 500) %>%
  group_by(role) %>%
  mutate(
    # compute ranks (dense_rank same as before)
    R_Rank  = dense_rank(desc(RAPM)),
    S_Rank  = dense_rank(desc(SKILLS)),
    PS_Rank = dense_rank(desc(PREVSEQ)),
    PH_Rank = dense_rank(desc(PHITTERS)),
    PAS_Rank = dense_rank(desc(PREVACT_S)),
    
    # total players in each role
    n_role = n(),
    
    # convert to percentile (higher = better)
    R_centile  = 100 * (1 - (R_Rank - 1) / (n_role - 1)),
    S_centile  = 100 * (1 - (S_Rank - 1) / (n_role - 1)),
    PS_centile = 100 * (1 - (PS_Rank - 1) / (n_role - 1)),
    PH_centile = 100 * (1 - (PH_Rank - 1) / (n_role - 1)),
    PAS_centile = 100 * (1 - (PAS_Rank - 1) / (n_role - 1))
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


player_awards_ranked %>%
  count(role)

# AWARD WINNER

top_players <- c("Venero", "Rodrigues", "Kochanowski", "Huber", "Perry", "Fornal", "Butryn")

top_player_scores <- player_awards_ranked %>%
  filter(str_detect(player_id, paste(top_players, collapse = "|"))) %>%
  select(
    player_id, team, role,
    RAPM, SKILLS, PREVACT_S,  
    R_centile, S_centile, PAS_centile
  ) %>%
  arrange(R_centile, role) %>%
  mutate(
    team = str_sub(team, 1, 1),           
    across(where(is.numeric), ~ round(.x, 2)) 
  )


top_player_scores %>%
  kable(format = "latex", booktabs = TRUE,
        caption = "RAPM, TPM, and TPM prevseq scores for award winners", escape = TRUE) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/Best_Players_AWARDS.tex")

# BEST IN RAPM

top_2_rapm_scores <- player_awards_ranked %>%
  filter(R_Rank == 1 | R_Rank ==2) %>%
  select(
    player_id, team, role,
    RAPM, SKILLS, PREVACT_S,  
    R_Rank, S_centile, PAS_centile
  ) %>%
  arrange(R_Rank, role) %>%
  mutate(
    team = str_sub(team, 1, 1),           
    across(where(is.numeric), ~ round(.x, 2)) 
  )

top_2_rapm_scores %>%
  kable(format = "latex", booktabs = TRUE,
        caption = "RAPM, TPM, and TPM prevseq scores for RAPM leaders", escape = TRUE) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/Best_Players_RAPM.tex")


# Comparing skills for phitters ------------------------------------------

phitters_comparison <- player_vault %>%
  select(player_id, role, `TPM_skills_role|Reception_points_added`,`TPM_skills_phitters_role|Reception_points_added`)

cor(player_vault$`TPM_skills_role|Reception_points_added`, player_vault$`TPM_skills_phitters_role|Reception_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Set_points_added`, player_vault$`TPM_skills_phitters_role|Set_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Dig_points_added`, player_vault$`TPM_skills_phitters_role|Dig_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Block_points_added`, player_vault$`TPM_skills_phitters_role|Block_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Serve_points_added`, player_vault$`TPM_skills_phitters_role|Serve_points_added`, use = "complete.obs")
cor(player_vault$`TPM_skills_role|Attack_points_added`, player_vault$`TPM_skills_phitters_role|HIT_points_added`, use = "complete.obs")



# Correlation of skills from the BASE -------------------------------------------------------------

metric_cols_combined <- names(player_vault_combined)[
  str_detect(names(player_vault_combined), "TPM_skills") &
    str_detect(names(player_vault_combined), "points_added_total$") &
    !str_detect(names(player_vault_combined), "fab") &
    !str_detect(names(player_vault_combined), "_skill(?!s)") &
    !str_detect(names(player_vault_combined), "def")
]

model_order_combined_init <- str_remove(metric_cols_combined, "_points_added_total$")

base_prefix <- "TPM_skills_role"

model_order_combined <- model_order_combined_init[(!str_detect(model_order_combined_init, base_prefix))]

correlation_from_base <- map_dfr(model_order_combined, function(model_name) {
  map_dfr(skill_order, function(skill) {
    base_col   <- paste0(base_prefix, "|", skill, "_points_added")
    target_col <- paste0(model_name, "|", skill, "_points_added")
    
    if (all(c(base_col, target_col) %in% names(player_vault_combined))) {
      tibble(
        Model = model_name,
        Skill = skill,
        Correlation = cor(
          player_vault_combined[[base_col]],
          player_vault_combined[[target_col]],
          use = "pairwise.complete.obs"
        )
      )
    } else {
      tibble(Model = model_name, Skill = skill, Correlation = NA_real_)
    }
  })
})

correlation_from_base_wide <- correlation_from_base %>%
  pivot_wider(names_from = Skill, values_from = Correlation) %>%
  mutate(
    Model = str_replace(Model, "^TPM_skills_", ""),
  )

correlation_from_base_long <- correlation_from_base %>%
  mutate(Model = factor(Model, levels = rev(unique(Model)))) %>%
  mutate(Model = str_remove(Model, "TPM_skills_"),
         Model = str_remove(Model, "_role"))

corr_min_base <- min(correlation_from_base_long$Correlation, na.rm = TRUE)
corr_max_base <- max(correlation_from_base_long$Correlation, na.rm = TRUE)

FROM_BASE_correlations <- ggplot(correlation_from_base_long, aes(x = Skill, y = Model, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient(low = "blue", high = "red",
                      limits = c(corr_min_base, corr_max_base)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    title = "Correlation of BASE Skill-Touch Plus-Minus to augmented models",
    x = "Skill",
    y = "Model",
    fill = "Correlation"
  )


plot(FROM_BASE_correlations)

ggsave(
  filename = "Exports/FROM_BASE_correlations.png",
  plot = FROM_BASE_correlations,    
  width = 10, height = 6, dpi = 300         
)



# Comparing role distribution ---------------------------------------------



compare_metric <- "TPM_raw_def_role_points_added_total"

role_distribution <- player_vault %>%
  group_by(role) %>%
  summarise(
    n_players = n(),
    mean = mean(.data[[compare_metric]], na.rm = TRUE),
    median = median(.data[[compare_metric]], na.rm = TRUE),
    min = min(.data[[compare_metric]], na.rm = TRUE),
    max = max(.data[[compare_metric]], na.rm = TRUE),
  )


role_to_plot <- "setter"

player_vault %>%
  filter(role == role_to_plot) %>%
  ggplot(aes(x = reorder(player_id, .data[[compare_metric]]), 
             y = .data[[compare_metric]])) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Player",
    y = "Number of Possessions",
    title = paste("Player points added for Role:", role_to_plot)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )



# Points Generated Per Set ------------------------------------------------

points_played_per_set <- RAPM_raw %>%
  select(player_id, number_RAPM_raw) %>%
  rename(points_played = number_RAPM_raw)

elite_player_points_per_set <- vault_readable %>%
  left_join(points_played_per_set, by = "player_id") %>%
  filter(points_played > 250) %>%
  mutate(across(contains("points_added"), ~ (.x / points_played) * 50, .names = "{.col}_per_set")) %>%
  select(player_id, team, role, points_played, ends_with("_per_set"))

leaderboards <- elite_player_points_per_set %>%
  select(player_id, team, role, ends_with("_per_set")) %>%
  pivot_longer(ends_with("_per_set"), names_to = "metric", values_to = "points_per_50") %>%
  group_by(metric) %>%
  summarise(
    Top_3 = list(slice_max(pick(player_id, team, role, points_per_50), points_per_50, n = 3, with_ties = FALSE)),
    Bottom_3 = list(slice_min(pick(player_id, team, role, points_per_50), points_per_50, n = 3, with_ties = FALSE)),
    .groups = "drop"
  )


# Histogram of points added -----------------------------------------------

ggplot(elite_player_points_per_set, aes(x = TPM_skills_stone_role_points_added_total_per_set)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density of Total Points Added per Player",
    x = "Total Points Added",
    y = "Density"
  )



elite_player_points_per_set_long <- elite_player_points_per_set %>%
  select(player_id, contains("points_added_total_per_set")) %>%
  pivot_longer(
    cols = contains("points_added_total_per_set"),
    names_to = "Metric",
    values_to = "PointsPerSet"
  )

ggplot(elite_player_points_per_set_long, aes(x = PointsPerSet)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  facet_wrap(~ Metric, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distributions of Points Added per Set Across Models",
    x = "Points Added per Set",
    y = "Number of Players"
  )

# Statistics for describing the issues with touch-based modelling -------------------

# One Touch Dig Statistics 
# 3041 total. 2397 errors = 78.8%.

aggregate_plays %>%
  count(skill, evaluation) %>%
  filter(skill == "Dig") %>%
  summarise(dig_count = sum(n))

touch_based_segments_skills %>%
  filter(str_detect(par_1, "Dig"), is.na(par_2)) %>%
  count(value)


aggregate_plays %>%
  count(skill, evaluation) %>%
  filter(skill == "Attack")




# Middle Attack Statistics

aggregate_plays %>%
  filter(skill == "Attack") %>%
  group_by(role) %>%
  count(evaluation) %>%
  summarise(
    total_attacks = sum(n, na.rm = TRUE),
    kills = sum(n[evaluation == "winning attack"], na.rm = TRUE),
    errors = sum(n[evaluation %in% c("error", "blocked")], na.rm = TRUE),
    hitting_percentage = (kills - errors) / total_attacks
  )

# Reception Statistic

touch_based_segments_skills %>%
  filter(
    if_any(starts_with("par_"), ~ str_detect(., "Reception")) &
      if_any(starts_with("par_"), ~ str_detect(., "Attack"))
  ) %>%
  count(value) %>%
  summarise(
    total = sum(n),
    points = sum(n[value == 1]),
    errors = sum(n[value == -1]),
    point_percentage = (points - errors) / total
  )


# Showing off teams for multicollinearity ---------------------------------

team_playtime <- player_awards %>%
  filter(team == "BOGDANKA LUK Lublin") %>%
  select(player_id, role, team, points_played, RAPM)


team_points_played <- aggregate_plays %>%
  group_by(match_id, point_id) %>%
  slice_tail() %>%
  ungroup() %>%
  select(home_team, visiting_team) %>%
  pivot_longer(cols = c(home_team, visiting_team), values_to = "team") %>%
  count(team, name = "points_played")

# Showing off some role and skill averages ------------------------------------------

touch_skill_averages_by_role <- role_avgs %>%
  select(role, contains("TPM_skills_pos"), contains("TPM_skills_neg"), -contains("total"))

showing_skill_avgs <- skill_avgs %>%
  filter(str_detect(statname, "TPM_skills_pos|TPM_skills_neg|TPM_skills_prevact_pos|TPM_skills_prevact_neg|TPM_skills_prevseq_pos|TPM_skills_prevseq_neg"))

intercepts <- tibble(
  model = c("BASE", "Prevact", "Prevseq"),
  pos_intercept = c(
    attr(TPM_skills, "pos_attributes")$intercept,
    attr(TPM_skills_prevact, "pos_attributes")$intercept,
    attr(TPM_skills_prevseq, "pos_attributes")$intercept
  ),
  neg_intercept = c(
    attr(TPM_skills, "neg_attributes")$intercept,
    attr(TPM_skills_prevact, "neg_attributes")$intercept,
    attr(TPM_skills_prevseq, "neg_attributes")$intercept
  )
)

intercept_row <- tibble(
  Skill = "Intercept",
  BASE_pos = intercepts$pos_intercept[1],
  BASE_neg = intercepts$neg_intercept[1],
  Prevact_pos = intercepts$pos_intercept[2],
  Prevact_neg = intercepts$neg_intercept[2],
  Prevseq_pos = intercepts$pos_intercept[3],
  Prevseq_neg = intercepts$neg_intercept[3]
)

skill_avgs_table_prelim <- showing_skill_avgs %>%
  separate(statname, into = c("model", "type_skill"), sep = "_", extra = "merge") %>%
  separate(type_skill, into = c("type", "skill"), sep = "\\|") %>%
  pivot_wider(names_from = type, values_from = avg) %>%
  select(-model) %>%
  rename(Skill = skill, BASE_pos = skills_pos, BASE_neg = skills_neg, 
         Prevact_pos = skills_prevact_pos, Prevact_neg = skills_prevact_neg,
         Prevseq_pos = skills_prevseq_pos, Prevseq_neg = skills_prevseq_neg) 

skill_avgs_table <- bind_rows(skill_avgs_table_prelim, intercept_row) %>%
  mutate(across(where(is.numeric), round, 3))

skill_avgs_table %>%
  kable(format = "latex", booktabs = TRUE, caption = "Average Coefficients for Skills", escape = TRUE) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/skill_avgs_table.tex")


# Finding averages for middles and liberos.

role_avgs %>%
  select(role, `TPM_raw_def_pos|DEF_avg`)



# ROLE and SKILL Correlations ---------------------------------------------

paired_skrole_models <- tibble(
  role_model  = model_order_combined[str_detect(model_order_combined, "_role")],
  skill_model = str_replace(model_order_combined[str_detect(model_order_combined, "_role")], "_role", "_skill")
) %>%
  filter(skill_model %in% model_order_combined)

grep("TPM_skills_fab_role", names(player_vault_combined), value = TRUE)
grep("TPM_skills_fab_skill", names(player_vault_combined), value = TRUE)


# Correlate
cor_skrole_results <- map_df(paired_skrole_models$role_model, function(role_model) {
  
  skill_model <- paired_skrole_models$skill_model[paired_skrole_models$role_model == role_model]
  
  # Find skill columns for both models
  role_cols  <- metric_cols_combined[str_starts(metric_cols_combined, paste0(role_model, "\\|"))]
  skill_cols <- metric_cols_combined[str_starts(metric_cols_combined, paste0(skill_model, "\\|"))]
  
  role_skills <- str_extract(role_cols, "(?<=\\|).*")
  skill_skills  <- str_extract(skill_cols,  "(?<=\\|).*")
  
  common_skills <- intersect(role_skills, skill_skills)
  
  tibble(
    Model = role_model,
    Skill = common_skills,
    Correlation = map_dbl(common_skills, ~ {
      role_col  <- paste0(role_model, "|", .x)
      skill_col <- paste0(skill_model, "|", .x)
      cor(player_vault_combined[[role_col]], player_vault_combined[[skill_col]], use = "pairwise.complete.obs")
    })
  )
})


cor_skrole_results_wide <- cor_skrole_results %>%
  pivot_wider(
    names_from = Skill,
    values_from = Correlation
  ) %>%
  arrange(factor(Model, levels = model_order_combined)) %>%
  mutate(
    Model = str_remove(Model, "_role"),
    Mean_Correlation = rowMeans(select(., -Model), na.rm = TRUE)
  ) %>%
  mutate(Model = str_replace(Model, "^TPM_skills_?", ""))

cor_skrole_results_long <- cor_skrole_results_wide %>%
  mutate(
    Model = factor(Model, levels = rev(Model))  # reverse for top-to-bottom
  ) %>%
  pivot_longer(
    cols = -c(Model, Mean_Correlation),
    names_to = "Skill",
    values_to = "Correlation"
  ) 

corr_min <- min(cor_skrole_results_long$Correlation, na.rm = TRUE)
corr_max <- max(cor_skrole_results_long$Correlation, na.rm = TRUE)

BASE_VS_DEF_correlation <- ggplot(cor_skrole_results_long, aes(x = Skill, y = Model, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(corr_min, corr_max)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    title = "Correlation of Models with Role relative and Skill relative scales",
    x = "Skill",
    y = "Model",
    fill = "Correlation"
  )

plot(BASE_VS_DEF_correlation)

ggsave(
  filename = "Exports/BASE_VS_DEF_correlation.png",
  plot = BASE_VS_DEF_correlation,    
  width = 10, height = 6, dpi = 300         
)


# Role by role ROLE vs SKILL Correlations ---------------------------------

cor_skrole_by_role <- player_vault_combined %>%
  select(role, contains("points_added")) %>%
  filter(!is.na(role)) %>%
  group_split(role) %>%
  set_names(unique(player_vault_combined$role)) %>%
  map_dfr(function(role_df) {
    role_name <- unique(role_df$role)
    
    # restrict correlation to players of this role
    role_cor_results <- map_df(paired_skrole_models$role_model, function(role_model) {
      skill_model <- paired_skrole_models$skill_model[paired_skrole_models$role_model == role_model]
      
      role_cols  <- names(role_df)[startsWith(names(role_df), paste0(role_model, "|"))]
      skill_cols <- names(role_df)[startsWith(names(role_df), paste0(skill_model, "|"))]
      
      role_skills  <- sub(".*\\|", "", role_cols)
      skill_skills <- sub(".*\\|", "", skill_cols)
      common_skills <- intersect(role_skills, skill_skills)
      
      if (length(common_skills) == 0) return(NULL)
      
      tibble(
        Skill = common_skills,
        Correlation = map_dbl(common_skills, ~ {
          role_col  <- paste0(role_model, "|", .x)
          skill_col <- paste0(skill_model, "|", .x)
          if (role_col %in% names(role_df) && skill_col %in% names(role_df)) {
            cor(role_df[[role_col]], role_df[[skill_col]], use = "pairwise.complete.obs")
          } else NA_real_
        })
      )
    })
    
    # summarize mean correlation by skill for this role
    role_cor_results %>%
      group_by(Skill) %>%
      summarise(Average_Correlation = mean(Correlation, na.rm = TRUE)) %>%
      mutate(role = role_name)
  })

# --- make wide for display ---
AVG_ROLE_correlation_ROLE_VS_SKILL <- cor_skrole_by_role %>%
  pivot_wider(
    names_from = Skill,
    values_from = Average_Correlation
  ) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  arrange(role)%>%
  rename_with(~ str_remove(.x, "_points_added$")) %>%
  select(role,Serve, Reception, Set, Attack, Block, Dig, Freeball, HIT, DEF)

AVG_ROLE_correlation_ROLE_VS_SKILL %>%
  kable(format = "latex", booktabs = TRUE, caption = "Average Correlations by Role - Role vs Skill Models", escape = TRUE) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("Exports/AVG_ROLE_correlation_ROLE_VS_SKILL.tex")



