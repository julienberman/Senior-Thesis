# anaysis file
# - Load cleaned data
# - Set up and train models

# link dependencies
source("source/derived/imports.R")
source("source/derived/helper_functions.R")

set_dplyr_defaults()
configure_fixest()

df_candidates <- read_csv("output/derived/clean/candidates_clean.csv", locale = locale(encoding = "UTF-8"))

# create dataframe of general elections for house races
df_candidates_house <- df_candidates %>% 
  filter(race_type == "gen" & has_ads == "1" & unopposed == FALSE & race == "house") %>% 
  relocate(c("t_econ", "t_culture", "t_econ_minus_culture"), .after = win)

# base model
model1 <- feols(vote_share ~ t_econ_minus_culture, 
                data = df_candidates_house,
                cluster = ~state_district)

# district fixed effects and year fixed effects
model2 <- feols(vote_share ~ t_econ_minus_culture | state_district + year, 
                data = df_candidates_house,
                cluster = ~state_district)

# district x year fixed effects and district x party fixed effects
model3 <- feols(vote_share ~ t_econ_minus_culture | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with cpr favorability interaction
model4 <- feols(vote_share ~ t_econ_minus_culture*cpr_favored | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with cpr favorability interaction and incumbency interaction
model5 <- feols(vote_share ~ t_econ_minus_culture*incumbent | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

model6 <- feols(vote_share ~ t_econ_minus_culture + cpr_solid_r + cpr_lean_d + cpr_likely_d + cpr_solid_d + cpr_lean_r + cpr_likely_r + cpr_toss_up | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

model7 <- feols(vote_share ~ t_econ_minus_culture | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

table(df_candidates_house$cpr_lean_d)

  
# Then combine into list
models <- list(
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3,
  "Model 4" = model4,
  "Model 5" = model5
)

summary(model1)
summary(model7)

# Create two-way fixed effects table
etable(models,     # First argument is the models list
       title = "Impact of Economic Advertising on Vote Share in House Races (2000 - 2020)",
       digits = 3,
       signif.code = c("*" = .1, "**" = .05, "***" = .01),
       # Add fixed effects indicators
       # fixef.group = list(
       #   "state_district" = "District FE",
       #   "year" = "Year FE",
       #   "state_district^year" = "District × Year FE",
       #   "state_district^party" = "District × Party FE"
       # ),
       # Style settings
       style.tex = style.tex("aer"),
       dict = c(
         "t_econ_minus_culture" = "Net Economic Advertising",
         "cpr_favored" = "Favored by the CPR",
         "incumbentTRUE" = "Incumbent",
         "t_econ_minus_culture:cpr_favored" = "Net Economic Advertising × Favored",
         "t_econ_minus_culture:incumbentTRUE" = "Net Economic Advertising × Incumbent",
         "state_district" = "District FE",
         "year" = "Year FE",
         "state_district^year" = "District × Year FE",
         "state_district^party" = "District × Party FE",
         "nobs" = "Observations",
          "r2" = "R²",
          "ar2" = "Adjusted R²"
         ),
       # Add summary statistics
       fitstat = ~n + r2 + ar2,
       notes = paste0(
         "\\footnotesize Notes: Standard errors clustered by congressional district in parentheses. ",
         "* p< 0.1, ** p < 0.05, *** p < 0.01. ",
         "The dependent variable is the candidate's vote share. ",
         "Net Economic Advertising measures the difference between the share of economic and cultural content in candidate's advertising. ",
         "Favored by the CPR indicates districts rated as favoring the candidate's party by the Cook Political Report eight months prior to the election. ",
         "Incumbent indicates candidates previously in office."
       ),
       tex = TRUE
)

# Prepare data for regression discontinuity --> House races only
threshold <- 0.1  # 10% bandwidth

# Step 1: create primary election margin dataset 
df_primaries <- df_candidates %>%
  # Keep only house primary races
  filter(race == "house" & unopposed == FALSE & race_type %in% c("repprim", "demprim")) %>%
  group_by(year, state_district, race_type) %>%
  # Calculate margin of victory in primary
  mutate(
    primary_winner = vote_share == max(vote_share),
    primary_runnerup = vote_share == sort(vote_share, decreasing=TRUE)[2],
    primary_margin = case_when(
      primary_winner ~ vote_share - sort(vote_share, decreasing=TRUE)[2],
      primary_runnerup ~ vote_share - max(vote_share),
      TRUE ~ NA_real_
    )
  ) %>%
  # Keep only winner and runner-up
  filter(primary_winner | primary_runnerup) %>%
  ungroup() %>% 
  relocate(c("primary_winner", "primary_runnerup", "primary_margin", "t_econ", "t_culture", "t_econ_minus_culture"), .after = win)

# Step 2: create general election dataset
df_generals <- df_candidates %>%
  filter(race_type == "gen", race == "house") %>%
  select(year, state_district, party, vote_share, name)

# Step 3: Create RDD dataset by matching primary candidates to general election outcomes
df_rdd <- df_primaries %>%
  # Keep primary winners and match to their general election outcome
  left_join(
    df_generals,
    by = c("year", "state_district", "party", "name"),
    suffix = c("_primary", "_general")
  ) %>% 
  # relocate
  relocate("vote_share_general", .after = "vote_share_primary") %>% 
  # only close primaries
  filter(abs(primary_margin) <= threshold) %>% 
  # only races that have ads
  filter(has_ads == 1) %>% 
  # only the candidates that won the primary
  filter(primary_winner == TRUE)

# Add diagnostic checks
diagnostic_checks <- function(data) {
  # Check for unmatched primary winners
  unmatched_winners <- data %>%
    filter(primary_winner == TRUE & is.na(vote_share_general)) %>%
    nrow()
  
  # Check for duplicate matches
  duplicate_matches <- data %>%
    group_by(year, state_district, party, name) %>%
    filter(n() > 1) %>%
    nrow()
  
  # Print results
  cat(sprintf("Unmatched primary winners: %d\n", unmatched_winners))
  cat(sprintf("Duplicate matches: %d\n", duplicate_matches))
  
  return(data)
}

df_rdd <- diagnostic_checks(df_rdd)

write_csv(df_rdd, "output/derived/clean/candidates_rdd.csv")

# train rdd model
model_rdd <- feols(vote_share_general ~ primary_margin, 
                data = df_rdd,
                cluster = ~state_district)

summary(model_rdd)
