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
ggplot(df_rdd, aes(x = primary_margin_adjusted, y = vote_share_general)) +
  geom_point(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_smooth(data = subset(df_rdd, primary_margin_adjusted < 0),
              method = "lm", formula = y ~ poly(x, 1), se = TRUE) +
  geom_smooth(data = subset(df_rdd, primary_margin_adjusted >= 0),
              method = "lm", formula = y ~ poly(x, 1), se = TRUE) +
  labs(title = "Effect of More Economic Candidate Winning Primary on Vote Share",
       x = "Adjusted Primary Margin (positive if more economic candidate wins)",
       y = "General Election Vote Share")

# Investigate the impact of conservatism econ minus culture
# ------
# ------
# ------

df_candidates_house <- df_candidates %>% filter(race == "house" & has_ads == 1)
df_candidates_senate <- df_candidates %>% filter(race == "senate" & has_ads == 1 & has_vs == 1)
df_candidates_governor <- df_candidates %>% filter(race == "governor" & has_ads == 1 & has_vs == 1)
df_candidates_president <- df_candidates %>% filter(race == "president" & has_ads == 1)

# DIME graph
p1 <- ggplot(df_candidates_house, aes(x = dwdime, y = t_econ_minus_culture)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  labs(
    title = "Regression of Economic Share of Ads vs. DIME (House Candidates)",
    x = "DIME Score",
    y = "Net share of economic ads"
  ) + 
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  theme(
    panel.grid = element_blank(),  # removes all gridlines
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
# DW-NOMINATE Graph
p2 <- ggplot(df_candidates_house, aes(x = dwnom, y = t_econ_minus_culture)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  labs(
    title = "Regression of Economic Share of Ads vs. DW-NOMINATE (House Candidates)",
    x = "DW-NOMINATE Score",
    y = "Net share of economic ads"
  ) + 
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  theme(
    panel.grid = element_blank(),  # removes all gridlines
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# CF-score graph
p3 <- ggplot(df_candidates_house, aes(x = cfscore, y = t_econ_minus_culture)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  labs(
    title = "Regression of Economic Share of Ads vs. CF-score (House Candidates)",
    x = "CF-score",
    y = "Net share of economic ads"
  ) + 
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  theme(
    panel.grid = element_blank(),  # removes all gridlines
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Votesmart graph
p4 <- ggplot(df_candidates_house, aes(x = vs_econ_conservatism, y = t_econ_minus_culture)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  labs(
    title = "Regression of Economic Share of Ads vs. Votesmart Rating (House Candidates)",
    x = "Votesmart Economic Conservatism",
    y = "Net share of economic ads"
  ) + 
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  theme(
    panel.grid = element_blank(),  # removes all gridlines
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# create correlation matrix
corr <- df_candidates_house %>% select(t_econ_minus_culture, dwnom, dwdime, cfscore, vs_econ_conservatism) %>% cor(use = "pairwise.complete.obs")

ggsave("output/derived/analyze/econ_on_dime.png", plot = p1, width = 10, height = 7, dpi = 300)
ggsave("output/derived/analyze/econ_on_dwnom.png", plot = p2, width = 10, height = 7, dpi = 300)
ggsave("output/derived/analyze/econ_on_cfscore.png", plot = p3, width = 10, height = 7, dpi = 300)
ggsave("output/derived/analyze/econ_on_votesmart.png", plot = p4, width = 10, height = 7, dpi = 300)
png("output/derived/analyze/heatmap.png", width = 10, height = 7, units = 'in', res = 300)
corrplot(corr, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45, 
         diag = TRUE)
dev.off()

model_cfscore <- feols(t_econ_minus_culture ~ cfscore, data = df_candidates_house)
model_cfscore_2 <- feols(t_econ_minus_culture ~ cfscore + I(cfscore^2), data = df_candidates_house)
model_cfscore_3 <- feols(t_econ_minus_culture ~ cfscore + I(cfscore^2) + I(cfscore^3), data = df_candidates_house)

model_vs <- feols(t_econ_minus_culture ~ vs_econ_conservatism, data = df_candidates_house)
model_vs_2 <- feols(t_econ_minus_culture ~ vs_econ_conservatism + I(vs_econ_conservatism^2), data = df_candidates_house)
model_vs_3 <- feols(t_econ_minus_culture ~ vs_econ_conservatism + I(vs_econ_conservatism^2) + I(vs_econ_conservatism^3), data = df_candidates)

model_dwdime <- feols(t_econ_minus_culture ~ dwdime, data = df_candidates_house)
model_dwdime_2 <- feols(t_econ_minus_culture ~ dwdime + I(dwdime^2), data = df_candidates_house)
model_dwdime_3 <- feols(t_econ_minus_culture ~ dwdime + I(dwdime^2) + I(dwdime^3), data = df_candidates_house)

model_dwnom <- feols(t_econ_minus_culture ~ dwnom, data = df_candidates_house)
model_dwnom_2 <- feols(t_econ_minus_culture ~ dwnom + I(dwnom^2), data = df_candidates_house)
model_dwnom_3 <- feols(t_econ_minus_culture ~ dwnom + I(dwnom^2) + I(dwnom^3), data = df_candidates_house)


# list of models
models <- list(model_dwnom, model_dwnom_2, model_dwdime, model_dwdime_2, model_cfscore, model_cfscore_2, model_vs, model_vs_2)

# create table
etable(models,
       title = "Impact of Candidate Ideology Scores on Net Share of Economic Advertising (House)",
       digits = 3,
       signif.code = c("*" = .1, "**" = .05, "***" = .01),
       style.tex = style.tex("aer"),
       # Add summary statistics
       fitstat = ~n + r2 + ar2,
       tex = TRUE
       )
linearHypothesis(model_cfscore_3, c("I(cfscore^2) = 0", "I(cfscore^3) = 0"))
linearHypothesis(model_vs_3, c("I(vs_econ_conservatism^2) = 0", "I(vs_econ_conservatism^3) = 0"))
linearHypothesis(model_dwdime_3, c("I(dwdime^2) = 0", "I(dwdime^3) = 0"))
linearHypothesis(model_dwnom_3, c("I(dwnom^2) = 0", "I(dwnom^3) = 0"))


# Investigate the impact of economic conditions on t_econ_minus_culture
# ------
# ------
# ------

# relationship between share of advertisements and economic condidtions
ggplot(df_candidates_house, aes(x = unemp_local, y = t_econ_minus_culture)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    title = "Regression of Economic Share of Ads vs. National Unemployment",
    x = "National Unemployment",
    y = "Net share of economic ads"
  ) + 
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  theme(
    panel.grid = element_blank(),  # removes all gridlines
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

model_unemp_1 <- feols(t_econ_minus_culture ~ cpi_nat + n_airings | state_district, data = df_candidates_house)
model_unemp_2 <- feols(t_econ_minus_culture ~ unemp_local + n_airings, data = df_candidates_house)
model_unemp_3 <- feols(t_econ_minus_culture ~ unemp_local + n_airings | state_district, data = df_candidates_house)

etable(model_unemp_1)

model <- feols(vote_share ~ t_econ_minus_culture*cpi_nat, data = df_candidates_senate)
etable(model)
