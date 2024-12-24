# make regresion table: t_econ vs factors

# link dependencies
source("source/derived/imports.R")
source("source/derived/helper_functions.R")
set_dplyr_defaults()
configure_fixest()

df_candidates <- read_csv("output/derived/clean/candidates_clean.csv", locale = locale(encoding = "UTF-8"))

df_candidates_house <- df_candidates %>% 
  filter(race_type == "gen" & has_ads == "1" & has_vs == 1 & race == "house")

# relationship between economic messaging and political ideology

model_ideology1 <- feols(z_econ_minus_culture ~ cfscore*is_dem + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus,
                         data = df_candidates_house,
                         vcov = "HC1")

model_ideology2 <- feols(z_econ_minus_culture ~ dwnom*is_dem + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus,
                         data = df_candidates_house,
                         vcov = "HC1")

model_ideology3 <- feols(z_econ_minus_culture ~ dwdime*is_dem + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus,
                         data = df_candidates_house,
                         vcov = "HC1")

model_ideology4 <- feols(z_econ_minus_culture ~ vs_econ_conservatism*is_dem + is_female + is_minority + population + share_white + share_black + share_hisp + share_other + share_u18 + share_65plus,
                         data = df_candidates_house,
                         vcov = "HC1")

# create ideology table
etable(model_ideology1, model_ideology2, model_ideology3, model_ideology4,    
       title = "Impact of Candidate Ideology on Type of Political Messaging in House Races (2000 - 2020)",
       digits = 3,
       digits.stats = 3,
       signif.code = c("*" = .1, "**" = .05, "***" = .01),
       fontsize = "small",
       page.width = "fit",
       style.tex = style.tex("aer"),
       dict = c(
         "z_econ_minus_culture" = "Net Economic Advertising",
         "cfscore" = "Campaign Finance Score",
         "is_dem" = "Democrat",
         "dwnom" = "DW-NOMINATE Score",
         "dwdime" = "DW-DIME Score",
         "vs_econ_conservatism" = "VoteSmart Economic Conservatism Rating"
       ),
       order = c("^Democrat$"),
       # Drop both sets of individual variables
       drop = c("population", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus",
                "Constant|Int"),
       # Add both groups in the fixed effects section
       group = list("_Candidate Demographic Controls" = c("is_female", "is_minority"),
                    "_Demographic Controls" = c("population", "share_white", "share_black", "share_hisp", "share_other", "share_u18", "share_65plus")),
       # Add summary statistics
       fitstat = ~n + r2 + ar2,
       notes = paste0(
         "\\footnotesize Notes: Heteroskedasticity robust standard errors in parentheses. ",
         "* p < 0.1, ** p < 0.05, *** p < 0.01. ",
         " The dependent variable is the candidate's vote net economic advertising. It measures the difference between the share of economic and cultural content in candidate's advertising.",
         " 'Democrat' is an indicator that equals one if the candidate is a Democrat.",
         " The other independent variables are different metrics measuring a candidate's ideology.",
         " The 'Campaign Finance Score' uses the political ideology of campaign donors. The 'DW-NOMINATE Score' uses roll-call votes.",
         " The 'DW-DIME Score' uses a combindation of both. And the VoteSmart ratings are the ideology scores given to candidates by various interest groups like the National Rifle Association and the American Civil Liberties Union.",
         " For all ideology scores, more postive scores indicate a more conservative candidate, whereas more negative scores indicate a more progressive candidate.",
         " District demographic controls include population, racial composition, and age composition."
       ),
       tex = TRUE,
       file = "output/tables/table_t_econ_ideology.tex",
       replace = TRUE
)

# Q: If I add state-level fixed effects, does the entire thing go away?


# create cfscore figure
binscatter_1 <- binsreg(x = df_candidates_house$cfscore, 
                      y = df_candidates_house$z_econ_minus_culture, 
                      nbins = 40, 
                      by = df_candidates_house$is_dem,
                      bycolors = c("red", "blue"),
                      bysymbols = c(19, 19))

p1 <- binscatter_1$bins_plot + 
  geom_smooth(data = df_candidates_house, aes(x = cfscore, y = z_econ_minus_culture, color = factor(is_dem)), method = "lm", se = TRUE, alpha = 0.2,) + 
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "Campaign Finance Score", y = "Net Economic Advertising") + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold")
  ) + 
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Republican", "Democrat"),
                     name = NULL)

ggsave("output/tables/figure_t_econ_ideology_cfscore.png", p1, width = 8, height = 6)


# create votesmart figure
binscatter_2 <- binsreg(x = df_candidates_house$vs_econ_conservatism, 
                        y = df_candidates_house$z_econ_minus_culture, 
                        nbins = 40, 
                        by = df_candidates_house$is_dem,
                        bycolors = c("red", "blue"),
                        bysymbols = c(19, 19))

p2 <- binscatter_2$bins_plot + 
  geom_smooth(data = df_candidates_house, aes(x = vs_econ_conservatism, y = z_econ_minus_culture, color = factor(is_dem)), method = "lm", se = TRUE, alpha = 0.2) + 
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "VoteSmart Economic Conservatism Rating", y = "Net Economic Advertising") + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold")
  ) + 
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Republican", "Democrat"),
                     name = NULL)

ggsave("output/tables/figure_t_econ_ideology_votesmart.png", p2, width = 8, height = 6)

