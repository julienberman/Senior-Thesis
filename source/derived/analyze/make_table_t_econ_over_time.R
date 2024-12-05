source("source/derived/imports.R")
source("source/derived/helper_functions.R")
set_dplyr_defaults()
configure_fixest()

df_candidates <- read_csv("output/derived/clean/candidates_clean.csv", locale = locale(encoding = "UTF-8"))

df_candidates_house <- df_candidates %>% 
  filter(race_type == "gen" & has_ads == "1" & unopposed == FALSE & race == "house") %>% 
  filter(year <= 2010)


model <- feols(vote_share ~ t_econ_minus_culture + i(year, t_econ_minus_culture, ref = 2002) + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus,
                data = df_candidates_house,
                cluster = ~state_district)

etable(model,
  title = "Impact of Economic Advertising on Vote Share Over Time",
  digits = 3,
  digits.stats = 3,
  signif.code = c("*" = .1, "**" = .05, "***" = .01),
  fontsize = "small",
  page.width = "fit",
  style.tex = style.tex("aer"),
  dict = c(
    "vote_share" = "Vote Share",
    "t_econ_minus_culture" = "Net Economic Advertising",
    "cpr_favored" = "Favored by the CPR",
    "year" = "Year",
    "state_district" = "Clustered by District"
  ),
  # Drop both sets of individual variables
  drop = c("population", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus"),
  # Add both groups in the fixed effects section
  group = list("_Candidate Demographic Controls" = c("is_female", "is_minority"),
               "_District Demographic Controls" = c("population", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus")),
  # Add summary statistics
  fitstat = ~n + r2 + ar2,
  notes = paste0(
    "\\footnotesize Notes: Standard errors clustered by congressional district in parentheses. ",
    "* p < 0.1, ** p < 0.05, *** p < 0.01. ",
    "Candidate demographic controls include gender and race. ",
    "District demographic controls include population, racial composition, and age bracket."
  ),
  tex = TRUE,
  file = "output/tables/table_vote_share_over_time.tex",
  replace = TRUE
)
