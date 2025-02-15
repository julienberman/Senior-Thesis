# Candidates cleaning file
# - Load raw data
# - rename and recode variables

# link dependencies
source("source/derived/imports.R")
source("source/derived/helper_functions.R")

# load datasets
df_candidates <- read_csv("source/raw_data/candidates.csv", locale = locale(encoding = "UTF-8"))
df_candidates_aug <- read_csv("source/raw_data/candidates_aug.csv", locale = locale(encoding = "UTF-8"))
df_presidents <- read_csv("source/raw_data/presidents.csv", locale = locale(encoding = "UTF-8"))
df_cpr <- read_csv("source/raw_data/cpr_ratings.csv", locale = locale(encoding = "UTF-8"))
df_cpr_aug <- read_csv("source/raw_data/cpr_ratings_aug.csv", locale = locale(encoding = "UTF-8"))
df_states <- read_csv("source/raw_data/state_codes.csv", locale = locale(encoding = "UTF-8"))
df_econ <- read_csv("source/raw_data/econ_indicators.csv", locale = locale(encoding = "UTF-8"))
df_dime <- read_csv("source/raw_data/dime_trunc.csv", local = locale(encoding = "UTF-8"))

# set output directory
output_dir <- "output/derived/clean"

columns_candidates = c("race_id", "id", "bonica.cid", "year", "race", "state2", "district2", "statedistrict", "census_division",	"census_region", "primary",
            "race_type", "name", "party", "status", "incumbent", "unopposed", "votes", "totalvotes", "voteshare", "win",
            "population", "share_male", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus",
            "log_income_per_capita", "unemprate",	"lfp", "universalism", "fb_connectedness",	"fb_bias", 
            "volunteering_rate",	"civic_organizations","population_2000", "share_male_2000", "share_white_2000", 
            "share_black_2000", "share_hisp_2000", "share_other_2000", "share_u18_2000", "share_65plus_2000",
            "log_income_per_capita_2000", "num.givers","num.givers.total", "total.receipts","total.disbursements", 
            "total.indiv.contribs", "total.unitemized", "total.pac.contribs", "total.party.contribs", 
            "total.contribs.from.candidate", "votesmart_id", "economic_conservatism_average", "social_conservatism_average",
            "recipient.cfscore", "dwdime", "dwnom1", "channel_mean_cnn_leo", "channel_mean_fnc_leo", "channel_mean_msnbc_leo", "channel_median_cnn_leo",	
            "channel_median_fnc_leo", "channel_median_msnbc_leo",	"channel_cnn_milena",	
            "channel_fnc_milena", "channel_msnbc_milena",	"rtg_cnn_milena",	"rtg_fnc_milena",	"rtg_msnbc_milena",
            "shr_cnn_milena",	"shr_fnc_milena",	"shr_msnbc_milena",	"channel_fnc_pc",	
            "channel_cnn_pc",	"channel_msnbc_pc",	"rtg_fnc_pc")

df_candidates <- df_candidates %>% 
  # stack presidential races on top of house, senate, and gubernatorial races
  bind_rows(df_presidents) %>% 
  select(any_of(names(df_candidates))) %>%
  # get relevant columns
  select(all_of(columns_candidates)) %>% 
  # merge state codes 
  left_join(df_states, by = c("state2" = "state_num")) %>% 
  relocate(state, state_abbr, .after = race) %>% 
  # merge gender & id information from the dime dataset
  left_join(df_dime %>% select(bonica.rid, lname, ffname, gender, icpsr, id_fec), by = c("id" = "bonica.rid")) %>% 
  # rename columns
  rename(
    id_bonica = id,
    id_bonica_contrib = bonica.cid,
    id_icpsr = icpsr,
    surname = lname,
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
    total_contribs_from_candidate = total.contribs.from.candidate,
    dwnom = dwnom1,
    id_votesmart = votesmart_id,
    vs_econ_conservatism = economic_conservatism_average,
    vs_social_conservatism = social_conservatism_average,
    cfscore = recipient.cfscore,
    unemp_local = unemprate,
    lfpr_local = lfp
  ) %>% 
  # recode
  mutate(
    state = tolower(state),
    state_abbr = tolower(state_abbr),
    race = tolower(race),
    party = tolower(party),
    gender = tolower(gender),
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
    ),
    unemp_local = unemp_local * 100
  ) %>% 
  # drop duplicate rows
  distinct() %>% 
  # merge advertisement info from candidates_aug
  left_join(df_candidates_aug %>% 
              bind_rows(df_presidents) %>% 
              select(any_of(names(df_candidates_aug))) %>% 
              select(race_id, name, n_airings, n_videos, starts_with("t_")), 
            by = c("race_id", "name")) %>%
  mutate(
    has_ads = ifelse(!is.na(t_econ), 1, 0),
    has_vs = ifelse(!is.na(vs_econ_conservatism), 1, 0),
    is_dem = ifelse(party == "democrat", 1, 0)
    ) %>% 
  # predict race using last name
  predict_race(surname.only = TRUE) %>% 
  mutate(
    pred_race = case_when(
      pred.whi == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "white",
      pred.bla == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "black",
      pred.his == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "hispanic",
      pred.asi == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "asian",
      pred.oth == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth, na.rm = TRUE) ~ "other",
      TRUE ~ NA_character_
    ),
    pred_race = ifelse(is.na(surname), NA, pred_race),
    is_minority = ifelse(pred_race == "white", 0, 1),
    is_female = ifelse(gender == "m", 0, 1),
    z_econ_minus_culture = as.numeric(scale(t_econ_minus_culture))
  ) %>% 
  select(-c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")) %>% 
  # relocate
  relocate(c("has_ads", "has_vs", "t_econ_minus_culture", "z_econ_minus_culture", "t_econ", "t_culture", "vs_econ_conservatism", "vs_social_conservatism", "cfscore", "dwdime", "dwnom", "n_airings", "n_videos"), .after = win) %>% 
  relocate(is_dem, .after = party) %>%
  relocate(c("id_bonica", "id_bonica_contrib", "id_icpsr", "id_fec", "id_votesmart"), .after = race_id) %>% 
  relocate(c("surname", "ffname", "gender", "is_female", "pred_race", "is_minority"), .after = name) %>% 
  # sort
  arrange(factor(race, levels = c("president", "house", "senate", "governor")), year, state, district_code)


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
  # filter to quarter 2
  filter(month %in% c(4, 5, 6)) %>% 
  # aggregate
  group_by(year) %>% 
  summarize(
    rdpi = mean(rdpi, na.rm = TRUE),
    cpi = mean(cpi, na.rm = TRUE),
    unemp_nat = mean(unemp, na.rm = TRUE),
    rgdppc_growth = first(rgdppc),
    ics = mean(ics, na.rm = TRUE),
    sp500 = mean(sp500, na.rm = TRUE),
    jobs = mean(jobs, na.rm = TRUE),
    pce = mean(pce, na.rm = TRUE)
  ) %>% 
  mutate(
    inflation = ((cpi - lag(cpi)) / lag(cpi)) * 100,
    rdpi_growth = ((rdpi - lag(rdpi)) / lag(rdpi)) * 100,
  ) %>% 
  # filter to relevant years
  filter(year >= 2000 & year <= 2022)

# merge CPR and econ indicators into candidates
df_candidates <- df_candidates %>% 
  # merge econ indicators
  left_join(df_econ %>% select(year, unemp_nat, inflation, rdpi_growth, rgdppc_growth, ics), by = "year") %>% 
  # relocate econ indicators
  relocate(
    c("unemp_nat", "inflation", "rdpi_growth", "rgdppc_growth", "ics"), .after = lfpr_local
  ) %>% 
  # merge CPR
  left_join(df_cpr, by = c("year", "race", "state", "district_code", "race_type")) %>% 
  # create a variable for whether candidate is favored
  mutate(
    cpr_favored = ifelse( 
      (cpr_rating_early %in% c("lean d", "likely d", "solid d") & party == "democrat") | 
        (cpr_rating_early %in% c("lean r", "likely r", "solid r") & party == "republican"), 
      1, 0)
  ) %>% 
  relocate(starts_with("cpr_"), .before = population) %>% 
  filter(year >= 2002)

# save clean data
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(df_candidates, file.path(output_dir, "candidates_clean.csv"))
