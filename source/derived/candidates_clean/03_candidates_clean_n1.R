# Candidates cleaning file n1
# - Load raw data
# - rename and recode variables

# link dependencies
source("source/derived/candidates_clean/01_candidates_imports.R")

# load datasets
df_candidates <- read_csv("source/raw_data/candidates.csv", locale = locale(encoding = "UTF-8"))
df_candidates_aug <- read_csv("source/raw_data/candidates_aug.csv", locale = locale(encoding = "UTF-8"))
df_cpr <- read_csv("source/raw_data/cpr_ratings.csv", locale = locale(encoding = "UTF-8"))
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
columns_cpr = c("Cycle", "Office", "dist_num", "State", "Rating")

df_cpr <- df_cpr %>% 
  # get relevant columns
  select(all_of(columns_cpr)) %>% 
  # rename columns
  rename(
    year = Cycle,
    race = Office,
    district_code = dist_num,
    state = State,
    cpr_rating = Rating,
  ) %>% 
  # recode
  mutate(
    state = tolower(state),
    race = tolower(race),
    cpr_rating = tolower(cpr_rating),
    race_type = "gen",
    district_code = ifelse(district_code == "AL", 1, district_code),
    district_code = ifelse(race == "senate" | race == "governor", 0, district_code),
    district_code = as.numeric(district_code)
  ) %>% 
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
  relocate(cpr_rating, .after = win)


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
