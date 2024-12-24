# make regresion table: vote_share vs t_econ

# link dependencies
source("source/derived/imports.R")
source("source/derived/helper_functions.R")
set_dplyr_defaults()
configure_fixest()

df_candidates <- read_csv("output/derived/clean/candidates_clean.csv", locale = locale(encoding = "UTF-8"))

# create dataframe of general elections for house races
df_candidates_house <- df_candidates %>% 
  filter(race_type == "gen" & has_ads == "1" & unopposed == FALSE & race == "house")

# base model
model1 <- feols(vote_share ~ t_econ_minus_culture, 
                data = df_candidates_house,
                cluster = ~state_district)

# district x year fixed effects and district x party fixed effects
model2 <- feols(vote_share ~ t_econ_minus_culture | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with district demographics and candidate demographics
model3 <- feols(vote_share ~ t_econ_minus_culture + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with cpr favorability & incumbent
model4 <- feols(vote_share ~ t_econ_minus_culture + cpr_favored + incumbent + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with cpr favorability interaction
model5 <- feols(vote_share ~ t_econ_minus_culture*cpr_favored + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with incumbency interaction
model6 <- feols(vote_share ~ t_econ_minus_culture*incumbent + cpr_favored + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with economic performance interaction
model7 <- feols(vote_share ~ t_econ_minus_culture*unemp_local + cpr_favored + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# Then combine into list
models <- list(
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3,
  "Model 4" = model4,
  "Model 5" = model5,
  "Model 6" = model6,
  "Model 7" = model7
  
)

# create table
etable(models,     
       title = "Impact of Economic Advertising on Vote Share in House Races (2002 - 2022)",
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
         "incumbentTRUE" = "Incumbent",
         "t_econ_minus_culture:cpr_favored" = "Net Economic Advertising × Favored",
         "t_econ_minus_culture:incumbentTRUE" = "Net Economic Advertising × Incumbent",
         "t_econ_minus_culture:unemp_local" = "Net Economic Advertising × Local Unemployment",
         "unemp_local" = "Local Unemployment Rate",
         "party^year" = "Year-Party",
         "state_district^party" = "District-Party",
         "fixed effects" = "Fixed Effects",
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
         "The dependent variable is the candidate's vote share. ",
         "Net Economic Advertising measures the difference between the share of economic and cultural content in candidate's advertising. ",
         "Favored by the CPR indicates districts rated as favoring the candidate's party by the Cook Political Report eight months prior to the election. ",
         "Incumbent indicates candidates previously in office. ",
         "Candidate demographic controls include gender and race. ",
         "District demographic controls include population, racial composition, and age bracket."
       ),
       tex = TRUE,
       file = "output/tables/table_vote_share.tex",
       replace = TRUE
)


# create same table but with standardized z_econ_minus_culture variable
# base model
model1_std <- feols(vote_share ~ z_econ_minus_culture, 
                data = df_candidates_house,
                cluster = ~state_district)

# district x year fixed effects and district x party fixed effects
model2_std <- feols(vote_share ~ z_econ_minus_culture | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with district demographics and candidate demographics
model3_std <- feols(vote_share ~ z_econ_minus_culture + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with cpr favorability & incumbent
model4_std <- feols(vote_share ~ z_econ_minus_culture + cpr_favored + incumbent + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with cpr favorability interaction
model5_std <- feols(vote_share ~ z_econ_minus_culture*cpr_favored + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with incumbency interaction
model6_std <- feols(vote_share ~ z_econ_minus_culture*incumbent + cpr_favored + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# ... with economic performance interaction
model7_std <- feols(vote_share ~ z_econ_minus_culture*unemp_local + cpr_favored + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | party^year + state_district^party, 
                data = df_candidates_house,
                cluster = ~state_district)

# Then combine into list
models_std <- list(
  "Model 1" = model1_std,
  "Model 2" = model2_std,
  "Model 3" = model3_std,
  "Model 4" = model4_std,
  "Model 5" = model5_std,
  "Model 6" = model6_std,
  "Model 7" = model7_std
  
)

# create table
etable(models_std,     
       title = "Impact of Economic Advertising on Vote Share in House Races (2002 - 2022)",
       digits = 3,
       digits.stats = 3,
       signif.code = c("*" = .1, "**" = .05, "***" = .01),
       fontsize = "small",
       page.width = "fit",
       style.tex = style.tex("aer"),
       dict = c(
         "vote_share" = "Vote Share",
         "z_econ_minus_culture" = "Net Economic Advertising (Standardized)",
         "cpr_favored" = "Favored by the CPR",
         "incumbentTRUE" = "Incumbent",
         "z_econ_minus_culture:cpr_favored" = "Net Economic Advertising × Favored",
         "z_econ_minus_culture:incumbentTRUE" = "Net Economic Advertising × Incumbent",
         "z_econ_minus_culture:unemp_local" = "Net Economic Advertising × Local Unemployment",
         "unemp_local" = "Local Unemployment Rate",
         "party^year" = "Year-Party",
         "state_district^party" = "District-Party",
         "fixed effects" = "Fixed Effects",
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
         "The dependent variable is the candidate's vote share. ",
         "Net Economic Advertising measures the difference between the share of economic and cultural content in candidate's advertising. ",
         "Favored by the CPR indicates districts rated as favoring the candidate's party by the Cook Political Report eight months prior to the election. ",
         "Incumbent indicates candidates previously in office. ",
         "Candidate demographic controls include gender and race. ",
         "District demographic controls include population, racial composition, and age bracket."
       ),
       tex = TRUE,
       file = "output/tables/table_vote_share_standardized.tex",
       replace = TRUE
)

binscatter <- binsreg(y = df_candidates_house$vote_share, x = df_candidates_house$t_econ_minus_culture, nbins = 40, bycolors = "black")

p1 <- binscatter$bins_plot + 
  geom_smooth(data = df_candidates_house, aes(x = t_econ_minus_culture, y = vote_share), method = "lm", se = TRUE, alpha = 0.2,) + 
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "Net Economic Advertising", y = "Vote Share") + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0.4, 0.6))

ggsave("output/tables/figure_vote_share.png", p1, width = 8, height = 6)

