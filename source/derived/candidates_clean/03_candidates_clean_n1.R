# Candidates cleaning file n1
# - Load raw data
# - rename and recode variables

# link dependencies
source("source/derived/candidates_clean/01_candidates_imports.R")
source("source/derived/candidates_clean/02_candidates_helper_functions.R")

# load datasets
df_candidates <- read_csv("source/raw_data/candidates.csv", locale = locale(encoding = "UTF-8"))
df_candidates_aug <- read_csv("source/raw_data/candidates_aug.csv", locale = locale(encoding = "UTF-8"))
df_cpr <- read_csv("source/raw_data/cpr_ratings.csv", locale = locale(encoding = "UTF-8"))
df_cpr_aug <- read_csv("source/raw_data/cpr_ratings_aug.csv", local = locale(encoding = "UTF-8"))
df_states <- read_csv("source/raw_data/state_codes.csv", locale = locale(encoding = "UTF-8"))
df_econ <- read_csv("source/raw_data/econ_indicators.csv", locale = locale(encoding = "UTF-8"))

columns_candidates = c("race_id", "year", "race", "state2", "district2", "statedistrict", "census_division",	"census_region", "primary",
            "race_type", "name", "party", "status", "incumbent", "unopposed", "votes", "totalvotes", "voteshare", "win",
            "population", "share_male", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus",
            "log_income_per_capita", "unemprate",	"lfp", "universalism", "fb_connectedness",	"fb_bias", 
            "volunteering_rate",	"civic_organizations","population_2000", "share_male_2000", "share_white_2000", 
            "share_black_2000", "share_hisp_2000", "share_other_2000", "share_u18_2000", "share_65plus_2000",
            "log_income_per_capita_2000", "num.givers","num.givers.total", "total.receipts","total.disbursements", 
            "total.indiv.contribs", "total.unitemized", "total.pac.contribs", "total.party.contribs", 
            "total.contribs.from.candidate", "votesmart_id", "economic_conservatism_average", "social_conservatism_average",
            "channel_mean_cnn_leo", "channel_mean_fnc_leo", "channel_mean_msnbc_leo",	"channel_median_cnn_leo",	
            "channel_median_fnc_leo", "channel_median_msnbc_leo",	"channel_cnn_milena",	
            "channel_fnc_milena", "channel_msnbc_milena",	"rtg_cnn_milena",	"rtg_fnc_milena",	"rtg_msnbc_milena",
            "shr_cnn_milena",	"shr_fnc_milena",	"shr_msnbc_milena",	"channel_fnc_pc",	
            "channel_cnn_pc",	"channel_msnbc_pc",	"rtg_fnc_pc")

df_candidates <- df_candidates %>% 
  # get relevant columns
  select(all_of(columns_candidates)) %>% 
  # merge state codes 
  left_join(df_states, by = c("state2" = "state_num")) %>% 
  relocate(state, state_abbr, .after = race) %>% 
  # rename columns
  rename(
    state_code = state2,
    district_code = district2,
    state_district = statedistrict,
    total_votes = totalvotes,
    vote_share = voteshare,
    num_givers = num.givers,
    num_givers_total = num.givers.total,
    total_receipts = total.receipts,
    total_disbursements = total.disbursements,
    total_indiv_contribs = total.indiv.contribs, 
    total_unitemized = total.unitemized,
    total_pac_contribs = total.pac.contribs,
    total_party_contribs = total.party.contribs,
    total_contribs_from_candidate = total.contribs.from.candidate
  ) %>% 
  # recode
  mutate(
    state = tolower(state),
    state_abbr = tolower(state_abbr),
    race = tolower(race),
    party = tolower(party),
    status = tolower(status),
    incumbent = case_when(
        !is.na(status) & tolower(status) == "incumbent"  ~ TRUE,
        !is.na(status) & tolower(status) == "challenger" ~ FALSE,
        is.na(status) & !is.na(incumbent)             ~ incumbent,
        TRUE                                          ~ NA  # When both are NA
      ),
    status = case_when(
      incumbent == TRUE ~ "incumbent",
      incumbent == FALSE  ~ "challenger",
      TRUE                         ~ status  # Retain original status if NA
    )
  ) %>% 
  # drop duplicate rows
  distinct() %>% 
  # merge advertisement info from candidates_aug
  left_join(df_candidates_aug %>% select(race_id, name, starts_with("t_")), by = c("race_id", "name")) %>%
  mutate(has_ads = ifelse(!is.na(t_econ), 1, 0)) %>% 
  relocate(has_ads, .before = t_econ) %>% 
  # sort
  arrange(year, race, state, district_code)

# clean CPR dataset
columns_cpr = c("Cycle", "Office", "dist_num", "State", "Rating", "EarlyRating")

df_cpr <- df_cpr %>% 
  # merge in early ratings
  left_join(df_cpr_aug, by = c("Cycle", "Office", "District")) %>% 
  relocate(EarlyRating, .after = "Rating") %>%
  # fill missing early ratings with the corresponding late rating
  mutate(EarlyRating = coalesce(EarlyRating, Rating)) %>% 
  # get relevant columns
  select(all_of(columns_cpr)) %>% 
  # rename columns
  rename(
    year = Cycle,
    race = Office,
    district_code = dist_num,
    state = State,
    cpr_rating_late = Rating,
    cpr_rating_early = EarlyRating
  ) %>% 
  # recode
  mutate(
    state = tolower(state),
    race = tolower(race),
    cpr_rating_late = tolower(cpr_rating_late),
    cpr_rating_early = tolower(cpr_rating_early),
    race_type = "gen",
    district_code = ifelse(district_code == "AL", 1, district_code),
    district_code = ifelse(race == "senate" | race == "governor", 0, district_code),
    district_code = as.numeric(district_code)
  ) %>% 
  dummy_cols(select_columns = "cpr_rating_early") %>% 
  rename(
    cpr_solid_d = `cpr_rating_early_solid d`,
    cpr_likely_d = `cpr_rating_early_likely d`,
    cpr_lean_d = `cpr_rating_early_lean d`,
    cpr_toss_up = `cpr_rating_early_toss up`,
    cpr_lean_r = `cpr_rating_early_lean r`,
    cpr_likely_r = `cpr_rating_early_likely r`,
    cpr_solid_r = `cpr_rating_early_solid r`,
  ) %>% 
  select(-ends_with("i"), -ends_with("?")) %>%
  # drop presidential races and pick relevant years
  filter(!race == "president" & year >= 2000) %>% 
  # drop duplicates. Deal with asterisked states
  distinct(year, race, state, district_code, race_type, .keep_all = TRUE)

# clean economic indicators
df_econ <- df_econ %>% 
  # filter to relevant yearsc
  filter(year >= 2000 & year <= 2022) %>% 
  # filter to quarter 2
  filter(month %in% c(4, 5, 6)) %>% 
  # aggregate
  group_by(year) %>% 
  summarize(
    jobs_nat = mean(jobs, na.rm = TRUE),
    pce_nat = mean(pce, na.rm = TRUE),
    rdpi_nat = mean(rdpi, na.rm = TRUE),
    cpi_nat = mean(cpi, na.rm = TRUE),
    ics_nat = mean(ics, na.rm = TRUE),
    sp500_nat = mean(sp500, na.rm = TRUE),
    unemp_nat = mean(unemp, na.rm = TRUE)
  )

# merge CPR and econ indicators into candidates
df_candidates <- df_candidates %>% 
  # merge econ indicators
  left_join(df_econ, by = "year") %>% 
  # merge CPR
  left_join(df_cpr, by = c("year", "race", "state", "district_code", "race_type")) %>% 
  # create a variable for whether candidate is favored
  mutate(
    cpr_favored = ifelse( 
      (cpr_rating_early %in% c("lean d", "likely d", "solid d") & party == "democrat") | 
        (cpr_rating_early %in% c("lean r", "likely r", "solid r") & party == "republican"), 
      1, 0)
  ) %>% 
  relocate(starts_with("cpr_"), .after = win)

View(df_candidates)


df_candidates_gen <- df_candidates %>% 
  filter(race_type == "gen") %>% 
  filter(has_ads == "1") %>% 
  # drop dummy
  select(-cpr_solid_r) %>% 
  relocate(c("t_econ", "t_culture", "t_econ_minus_culture"), .after = win) %>% 
  # drop zeros
  filter(t_econ > 0) %>% 
  # drop unopposed
  filter(unopposed == FALSE)

formula_1 = as.formula("vote_share ~ t_econ")
formula_2 = as.formula("vote_share ~ t_econ + share_black + share_hisp + share_other + share_u18 + share_65plus + log_income_per_capita")
formula_3 = as.formula("vote_share ~ t_econ + share_black + share_hisp + share_other + share_u18 + share_65plus + log_income_per_capita + factor(year) + factor(state)")
formula_4 = as.formula("vote_share ~ t_econ + cpr_likely_r + cpr_lean_r + cpr_toss_up + cpr_lean_d + cpr_likely_d + cpr_solid_d")
formula_5 = as.formula("vote_share ~ t_econ * cpr_favored + t_econ * incumbent + share_black + share_hisp + share_other + share_u18 + share_65plus + log_income_per_capita + factor(year) + factor(state)")
formula_6 = as.formula("vote_share ~ t_econ * cpr_favored + t_econ * incumbent + unemprate*incumbent + lfp*incumbent")
formula_7 = as.formula("vote_share ~ t_econ * incumbent")

formula_8 = as.formula("vote_share ~ t_econ_minus_culture")
formula_9 = as.formula("vote_share ~ t_econ_minus_culture + share_black + share_hisp + share_other + share_u18 + share_65plus + log_income_per_capita")
formula_10 = as.formula("vote_share ~ t_econ_minus_culture + share_black + share_hisp + share_other + share_u18 + share_65plus + log_income_per_capita + factor(year) + factor(state)")
formula_11 = as.formula("vote_share ~ t_econ_minus_culture + cpr_likely_r + cpr_lean_r + cpr_toss_up + cpr_lean_d + cpr_likely_d + cpr_solid_d")
formula_12 = as.formula("vote_share ~ t_econ_minus_culture * cpr_favored + t_econ_minus_culture * incumbent + share_black + share_hisp + share_other + share_u18 + share_65plus + log_income_per_capita + factor(year) + factor(state)")
formula_13 = as.formula("vote_share ~ t_econ_minus_culture * cpr_favored + t_econ_minus_culture * incumbent + unemprate*incumbent + lfp*incumbent")


econ_formulas <- list(formula_1, formula_2, formula_3, formula_4, formula_5, formula_6, formula_7)
econ_minus_culture_formulas <-list(formula_8, formula_9, formula_10, formula_11, formula_12, formula_13)

econ_models <- train_models(econ_formulas, df_candidates_gen)
econ_minus_culture_models <- train_models(econ_minus_culture_formulas, df_candidates_gen)

model <- lm(formula_12, data = df_candidates_gen)

summary(model)

generate_table(
  models = econ_models,
  title = "Results of Regressions of Vote Share on Econ",
  output_file = "table_1.tex"
)

generate_table(
  models = econ_minus_culture_models,
  title = "Results of Regressions of Vote Share on Econ Minus Culture",
  output_file = "table_2.tex"
)


test_model = lm("win ~ cpr_favored", data = df_candidates_gen)
summary(test_model)

df_candidates_gen %>% 
  filter(t_econ >= 0.5 & t_econ <= 0.75) %>% 
  ggplot(mapping = aes(x = t_econ, y = vote_share)) +
  geom_point() + 
  geom_smooth()

df_candidates_gen %>% 
  filter(unemprate < 0.2) %>% 
  ggplot(mapping = aes(x = unemprate, y = t_econ)) +
  geom_point() + 
  geom_smooth()


View(df_candidates_gen)









# missing a ton of 2022 races
df_cpr %>% 
  anti_join(df_candidates, by = c("year", "race", "state", "district_code", "race_type")) %>%
  filter(year == 2022) %>% 
  View()

  
# non 2022 missing races
df_cpr %>% 
  anti_join(df_candidates, by = c("year", "race", "state", "district_code", "race_type")) %>%
  filter(!year == 2022) %>% 
  arrange(desc(year)) %>% 
  View()



# Quantify missingness

# df %>% 
#   arrange(year, race, statedistrict, race_type) %>% 
#   select(columns) %>% 
#   filter(is.na(votes) & unopposed == FALSE) %>% 
#   View()
# 
# df %>% 
#   arrange(year, race, statedistrict, race_type) %>% 
#   select(columns) %>% 
#   filter(race == "Senate" & race_type == "gen" & str_detect(name, "brown")) %>% 
#   View()
# 
# df %>% 
#   arrange(year, race, statedistrict, race_type) %>% 
#   select(columns) %>% 
#   filter(is.na(share_white)) %>% 
#   View()
# 
# colnames(df)
