# anaylsis file
# - Load cleaned data
# - Set up and train models

# link dependencies
source("source/derived/imports.R")
source("source/derived/helper_functions.R")

set_dplyr_defaults()
configure_fixest()
 
df_candidates <- read_csv("output/derived/clean/candidates_clean.csv", locale = locale(encoding = "UTF-8"))

# Prepare data for regression discontinuity --> House races only
threshold <- 0.1

# Step 1: create primary election margin dataset 
df_primaries <- df_candidates %>%
  # Keep only house primary races
  filter(race == "house" & unopposed == FALSE & race_type %in% c("repprim", "demprim")) %>%
  group_by(year, state_district, race_type) %>%
  # Calculate margin of victory in primary and cultural comparisons
  mutate(
    # calculate winner/runnerup status
    primary_winner = vote_share == max(vote_share),
    primary_runnerup = vote_share == sort(vote_share, decreasing=TRUE)[2],
    # calculate raw margin
    primary_margin = case_when(
      primary_winner ~ vote_share - sort(vote_share, decreasing=TRUE)[2],
      primary_runnerup ~ vote_share - max(vote_share),
      TRUE ~ NA_real_
    )
  ) %>%
  # Keep only winner and runner-up
  filter(primary_winner | primary_runnerup) %>%
  ungroup() %>% 
  relocate(c("primary_winner", "primary_runnerup", "primary_margin"), .after = win)

# Step 2: create general election dataset
df_generals <- df_candidates %>%
  filter(race_type == "gen", race == "house") %>%
  select(year, state_district, party, vote_share, name, 
         t_econ_minus_culture, t_econ, t_culture, n_airings)

# Step 3: Create RDD dataset by matching primary candidates to general election outcomes
df_rdd <- df_primaries %>%
  # Keep primary winners and match to their general election outcome
  left_join(
    df_generals,
    by = c("year", "state_district", "party", "name"),
    suffix = c("_primary", "_general")
  ) %>% 
  # only races that have ads
  filter(has_ads == 1) %>% 
  # For each primary race, identify the more cultural candidate
  group_by(year, state_district, race_type) %>%
  mutate(
    # Get single values for winner and runnerup cultural scores
    winner_econ_score = first(t_econ_minus_culture_primary[primary_winner]),
    runnerup_econ_score = first(t_econ_minus_culture_primary[primary_runnerup]),
    
    # Compare economic scores
    is_winner_more_economic = winner_econ_score > runnerup_econ_score,
    
    # Create RD running variable
    primary_margin_adjusted = if_else(is_winner_more_economic, 
                                      primary_margin, 
                                      -primary_margin)
  ) %>%
  filter(!is.na(primary_margin_adjusted)) %>%
  ungroup() %>%
  # relocate columns
  relocate("vote_share_general", .after = "vote_share_primary") %>% 
  relocate(c("primary_margin_adjusted", "is_winner_more_economic"), .after = "primary_margin") %>%
  relocate(c("t_econ_minus_culture_general", "t_econ_general", "t_culture_general"), 
           .after = "t_culture_primary") %>% 
  relocate("n_airings_general", .after = "n_airings_primary") %>% 
  filter(abs(primary_margin) < threshold)


# Basic RD plot
p1 <- ggplot(df_rdd, aes(x = primary_margin_adjusted, y = vote_share_general)) +
  geom_point(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_smooth(data = subset(df_rdd, primary_margin_adjusted < 0),
              method = "lm", formula = y ~ poly(x, 1), se = TRUE) +
  geom_smooth(data = subset(df_rdd, primary_margin_adjusted >= 0),
              method = "lm", formula = y ~ poly(x, 1), se = TRUE) +
  labs(title = "Effect of More Economic Candidate Winning Primary on Vote Share",
       x = "Adjusted Primary Margin (positive if more economic candidate wins)",
       y = "General Election Vote Share")

ggsave("output/tables/figure_rdd.png", plot = p1, width = 10, height = 7, dpi = 300)
