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

model_economics_local <- feols(z_econ_minus_culture ~ unemp_local + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | state_district,
                               data = df_candidates_house,
                               cluster = ~state_district)

model_economics_nat <- feols(z_econ_minus_culture ~ sw(unemp_nat, inflation, rdpi_growth, rgdppc_growth, ics) + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | state_district,
                             data = df_candidates_house,
                             cluster = ~state_district)

model_economics_final <- feols(z_econ_minus_culture ~ unemp_local + unemp_nat + rgdppc_growth + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus | state_district,
                               data = df_candidates_house,
                               cluster = ~state_district)

# create economics table
etable(model_economics_local, model_economics_nat, model_economics_final,    
       title = "Impact of Economic Conditions on Type of Political Messaging in House Races (2000 - 2020)",
       digits = 3,
       digits.stats = 3,
       signif.code = c("*" = .1, "**" = .05, "***" = .01),
       fontsize = "small",
       page.width = "fit",
       style.tex = style.tex("aer"),
       dict = c(
         "z_econ_minus_culture" = "Net Economic Advertising",
         "unemp_local" = "Regional Unemployment",
         "unemp_nat" = "National Unemployment",
         "inflation" = "Inflation",
         "rdpi_growth" = "Growth in Disposable Income",
         "rgdppc_growth" = "Growth in GDP Per Capita",
         "ics" = "Consumer Sentiment Index",
         "state_district" = "District",
         "year" = "Year",
         "party^year" = "Year-Party",
         "state_district^party" = "District-Party"
       ),
       # Drop both sets of individual variables
       drop = c("population", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus"),
       # Add both groups in the fixed effects section
       group = list("_Candidate Demographic Controls" = c("is_female", "is_minority"),
                    "_Demographic Controls" = c("population", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus")),
       # Add summary statistics
       fitstat = ~n + r2 + ar2,
       notes = paste0(
         "\\footnotesize Notes:",
         "* p < 0.1, ** p < 0.05, *** p < 0.01. "
       ),
       tex = TRUE,
       file = "output/tables/table_t_econ_economics.tex",
       replace = TRUE
)

binscatter <- binsreg(x = df_candidates_house$unemp_local, y = df_candidates_house$z_econ_minus_culture, nbins = 20, bycolors = "black")

p1 <- binscatter$bins_plot + 
  geom_smooth(data = df_candidates_house, aes(x = unemp_local, y = z_econ_minus_culture), method = "lm", se = TRUE, alpha = 0.2) + 
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(limits = c(0, 15), labels = function(x) paste0(x, "%")) +
  labs(x = "Regional Unemployment", y = "Net Economic Advertising") + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("output/tables/figure_t_econ_economics.png", p1, width = 8, height = 6)
