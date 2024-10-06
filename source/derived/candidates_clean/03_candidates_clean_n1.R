
'''
Candidates cleaning file n1
- Load raw data
- rename and recode variables

'''

# link dependencies
source("source/derived/candidates_clean/01_candidates_imports.R")

# load datasets
df_candidates <- read_csv("source/raw_data/candidates.csv", locale = locale(encoding = "UTF-8"))
df_states <- read_csv("source/raw_data/state_codes.csv", locale = locale(encoding = "UTF-8"))
df_econ <- read_csv("source/raw_data/econ_indicators.csv", locale = locale(encoding = "UTF-8"))


columns = c("race_id", "year", "race", "state2", "district2", "statedistrict", "census_division",	"census_region", "primary",
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
  select(all_of(columns)) %>% 
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
  # sort
  arrange(year, race, state, district_code)

# Quantify missingness


'''
df %>% 
  arrange(year, race, statedistrict, race_type) %>% 
  select(columns) %>% 
  filter(is.na(votes) & unopposed == FALSE) %>% 
  View()

df %>% 
  arrange(year, race, statedistrict, race_type) %>% 
  select(columns) %>% 
  filter(race == "Senate" & race_type == "gen" & str_detect(name, "brown")) %>% 
  View()

df %>% 
  arrange(year, race, statedistrict, race_type) %>% 
  select(columns) %>% 
  filter(is.na(share_white)) %>% 
  View()

colnames(df)
'''