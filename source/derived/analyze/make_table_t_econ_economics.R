# make regresion table: t_econ vs factors

# link dependencies
source("source/derived/imports.R")
source("source/derived/helper_functions.R")
set_dplyr_defaults()
configure_fixest()

df_candidates <- read_csv("output/derived/clean/candidates_clean.csv", locale = locale(encoding = "UTF-8"))

df_candidates_house <- df_candidates %>% 
  filter(race_type == "gen" & has_ads == "1" & has_vs == 1 & race == "house")

# Economics model

model_economics_nat <- feols(t_econ_minus_culture ~ sw(unemp_local, unemp_nat, inflation, rdpi_growth, rgdppc_growth, ics) + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | state_district,
                             data = df_candidates_house,
                             cluster = ~state_district)

model_economics_local <- feols(t_econ_minus_culture ~ unemp_local + unemp_nat + rgdppc_growth + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | state_district,
                               data = df_candidates_house,
                               cluster = ~state_district)

# create economics table
etable(model_economics_nat, model_economics_local,    
       title = "Impact of Economic Conditions on Type of Political Messaging in House Races (2000 - 2020)",
       digits = 3,
       digits.stats = 3,
       signif.code = c("*" = .1, "**" = .05, "***" = .01),
       fontsize = "small",
       page.width = "fit",
       style.tex = style.tex("aer"),
       dict = c(
         "t_econ_minus_culture" = "Net Economic Advertising",
         "unemp_local" = "Regional Unemployment",
         "unemp_nat" = "National Unemployment",
         "inflation" = "Inflation",
         "rdpi_growth" = "Growth in Disposable Income",
         "rgdppc_growth" = "Growth in GDP Per Capita",
         "ics" = "Consumer Sentiment Index",
         "state_district" = "District",
         "party^year" = "Year-Party",
         "state_district^party" = "District-Party"
       ),
       # Drop both sets of individual variables
       drop = c("population", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus"),
       # Add both groups in the fixed effects section
       group = list("_Demographic Controls" = c("population", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus")),
       # Add summary statistics
       fitstat = ~n + r2 + ar2,
       notes = paste0(
         "\\footnotesize Notes: Standard errors clustered by congressional district in parentheses. ",
         "* p < 0.1, ** p < 0.05, *** p < 0.01. ",
         "The dependent variable is the candidate's vote share. ",
         "Net Economic Advertising measures the difference between the share of economic and cultural content in candidate's advertising. ",
         "Favored by the CPR indicates districts rated as favoring the candidate's party by the Cook Political Report eight months prior to the election. ",
         "Incumbent indicates candidates previously in office. ",
         "District demographic controls include population, racial composition, and age composition."
       ),
       tex = TRUE,
       file = "output/tables/table_t_econ_economics.tex",
       replace = TRUE
)

# create economic local figure
p <- ggplot(df_candidates_house, aes(x = unemp_local, y = t_econ_minus_culture)) +
  # Add binned points with standard error bars
  stat_summary_bin(fun.data = "mean_se", 
                   bins = 20, 
                   geom = "pointrange", 
                   alpha = 0.8) +
  # Add regression line
  geom_smooth(method = "lm", 
              se = TRUE, alpha = 0.2,
              color = "blue") +
  # Labels
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Regional Unemployment",
       y = "Net Economic Advertising") +
  # Theme
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold")
  ) +
  geom_vline(xintercept = 0, color = "black") + 
  geom_hline(yintercept = 0, color = "black")
  

ggsave("output/tables/figure_t_econ_economics.png", p, width = 8, height = 6)
