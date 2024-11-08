library(tidyverse)
library(haven)
library(fixest)
library(modelsummary)
library(broom)
library(gt)
library(insight)
library(lubridate)
library(jsonlite)

news_ad_colors = c('#f89d0c','#0a8cda')
econ_culture_other_colors = c('#075A3A', '#431C53',  '#A9A9A9', '#5C4033')
dem_rep_colors = c('#0000ff', '#E81B23')

summarize = dplyr::summarise
select = dplyr::select

options(modelsummary_format_numeric_latex = "plain")
tablenotes = jsonlite::read_json('code/tablenotes.json')

setFixest_fml(
  ..indiv_demo = ~as.factor(racegr) + educ + poly(age, 2),
  ..county_demo_2000 = ~log_income_per_capita_2000+share_white_2000+share_black_2000+log(population_2000),
  ..county_demo = ~share_white+share_black + log(population),
  ..campaign_conditions = ~incumbent+n_videos,
  ..econ = ~unemprate+lfp+log_income_per_capita,
  ..voting = ~voteshare_lag,
  ..voting_indiv = ~as.factor(party),
  ..ad_controls = ~log(n_airings),
  ..base_2000 = ~log(population_2000),
  ..demo_2000 = ~share_white_2000+share_black_2000+share_hisp_2000+share_other_2000+log_income_per_capita_2000,
  ..fourthwall_individual = ~news_prop+party+educ+log(1+income)+married+log(1+net_worth)+has_children+male_hh,
  reset=T
)




gt_save = function(x, label, make_small=T) {
  latex <- x %>%
    tab_options(table.font.size = '80%') %>%
    as_latex() %>%
    as.character() %>%
    str_replace("\\\\midrule\\\\addlinespace\\[2\\.5pt\\]\n\\\\multicolumn\\{[0-9]\\}\\{l\\}\\{\\\\vspace\\*\\{-5mm\\}\\} \\\\\\\\ \n\\\\midrule", "\\\\midrule") %>% 
    str_replace_all('\\\\begin\\{minipage\\}\\{\\\\linewidth\\}\n', '\\\\begin{minipage}{0.95\\\\linewidth}\n \\\\vspace{0.25cm} \\\\textit{Notes:} ') %>% 
    str_replace_all('\\\\caption\\*\\{\\n\\{', str_interp('\\\\caption\\{\\{\\\\label{tab:${label}}')) 
  
  if (make_small) {
    latex = latex %>% 
    paste0('{\n\\scriptsize', ., '\n}') %>% 
    str_remove_all('\\\\large')
  }
  
  latex %>% 
    write_lines(file = str_interp('output/tables/${label}.tex'))

}

get_mean_of_models <- function(models, ...) {
  means = map_dbl(models, function(x) get_data(x)[[find_response(x)]] %>% mean(na.rm=T) %>% round(3) )
  out = c('DV mean', means)
  return(out)
}

winsorize = function(x, q) {
  low = quantile(x, q)
  high = quantile(x, 1-q)
  x[x<low] = low
  x[x>high] = high
  return(x)
}

get_worstcase_p_value <- function(coef1, coef2, se1, se2) {
  delta <- coef1 - coef2
  cov_worst_case <- se1 * se2
  se_delta_worst_case <- sqrt(se1^2 + se2^2 - 2 * cov_worst_case)
  z_value <- delta / se_delta_worst_case
  p_value <- 2 * (1 - pnorm(abs(z_value)))
  return(p_value)
}


topics = jsonlite::read_json('code/gpt-prompts/topics.json')
extract_topics <- function(lst, prefix = "") {
  topics <- c()
  for (name in names(lst)) {
    full_name <- if (prefix == "") name else paste(prefix, name, sep = "$")
    topics = c(topics, name)
    if (length(lst[[name]]) > 0) {
      topics <- c(topics, extract_topics(lst[[name]], full_name))
    }
  }
  return(topics)
}


topic_dict = read_csv('data/raw/topics-to-coarse-dict.csv') %>% 
  mutate(term = janitor::make_clean_names(topic),
         header = topic %in% names(topics)) %>% 
  mutate(header_name = if_else(header, topic, NA_character_)) %>% 
  fill(header_name, .direction = "down") %>% 
  mutate(exclude_from_comparison = header_name %in% c('Politicians/candidates and political campaigns', 'Entertainment/popular culture/sports', 'Natural or manmade disasters/extreme weather events in the US', 'International events', 'Science and technology', 'General anti-elite/anti-politician/anti-media rhetoric', 'Commercial (non-political) advertising'))

additional = tribble(~term, ~topic, ~header, ~econ, ~cultural, ~other, ~header_name, ~exclude_from_comparison,
                    'econ', 'Economics', F, 1, 0, 0, 'Economics', F,
                    'culture', 'Culture', F, 0, 1, 0, 'Culture', F,
                    'other', 'Other', F, 0, 0, 1, 'Other content', F)

topic_dict = bind_rows(additional, topic_dict) %>% 
  mutate(type = case_when(
    econ==1 & cultural == 0 & other == 0 ~ 'Economic',
    econ==0 & cultural == 1 & other == 0 ~ 'Cultural',
    econ==0 & cultural == 0 & other == 1 ~ 'Other political',
    econ==0 & cultural == 0 & other == 0 ~ 'Non-political',
    T ~ 'Mixed'),
    type = factor(type, levels=c('Economic','Cultural','Other political','Mixed','Non-political')))

get_parents <- function(term, topics) {
  find_parents <- function(term, topics, path = NULL) {
    for (key in names(topics)) {
      new_path <- c(path, key)
      if (key == term) {
        return(new_path)
      }
      if (length(topics[[key]]) > 0) {
        result <- find_parents(term, topics[[key]], new_path)
        if (!is.null(result)) {
          return(result)
        }
      }
    }
    return(NULL)
  }
  path <- find_parents(term, topics)
  return(path)
}
rows = list()
for (topic in topic_dict$topic) {
  parents = get_parents(topic, topics)
  new_rows = expand.grid(topic, parents)
  rows[[length(rows)+1]] = new_rows
}
parentdict = bind_rows(rows)
names(parentdict) = c('term','parent')
parentdict = parentdict %>% 
  rename(child_topic = term, parent_topic = parent) %>% 
  left_join(topic_dict %>% select(topic, term) %>% rename(child_term = term), by=c('child_topic'='topic')) %>% 
  left_join(topic_dict %>% select(topic, term) %>% rename(parent_term = term), by=c('parent_topic'='topic')) %>% 
  mutate(across(c(child_term, parent_term), function(x) paste0('t_', x)))


renaming = c(
  'Economic policy and economic conditions' = 'Economic policy/conditions',
  'Entitlements/healthcare/welfare/social safety net' = 'Healthcare and social safety net',
  'Social issues and women and minority issues' = 'Race/gender issues',
  'Immigration to the US/immigrants in the US' = 'Immigration',
  'Politicians/candidates and political campaigns' = 'Candidates and campaigns',
  'Government functioning' = 'Gov functioning/conflict'
)
topic_dict = topic_dict %>% mutate(topic = ifelse(topic %in% names(renaming), renaming[topic], topic))

sum_columns = function(data, parentdict) {
  sum_list = list()
  for (parentterm in unique(parentdict$parent_term)) {
    children = parentdict %>% filter(parent_term == parentterm)
    cols_to_sum = unique(children$child_term)
    sums = data %>% select(any_of(cols_to_sum)) %>% rowSums()
    sum_list[[parentterm]] = sums
  }
  result_df <- as.data.frame(sum_list)
  colnames(result_df) <- names(sum_list)
  data = data %>% select(-any_of(colnames(result_df))) %>% bind_cols(result_df)
  return (data)
}

normalize = function(data) {  
  
  data$normalization = data %>% 
    select(starts_with('t_')) %>% 
    rowSums()
  
  data = data %>% 
    mutate(across(starts_with('t_'), function(x) x/normalization)) %>% 
    select(-normalization)
  
  data = data %>% 
    mutate(t_econ = rowSums(data %>% select(matches(econpattern))),
           t_culture = rowSums(data %>% select(matches(culturepattern))),
           t_other = rowSums(data %>% select(matches(otherpattern)))) %>% 
    select(-length) %>% 
    mutate(normalization = t_econ + t_culture + t_other,
           across(c(t_econ, t_culture, t_other), ~./normalization)) %>%
    select(-normalization) %>%
    mutate(t_econ_minus_culture = t_econ - t_culture)
  
  data = data %>% 
    sum_columns(parentdict)
  
  return(data)
}

econpattern <- paste(paste0("^t_", topic_dict$term[topic_dict$econ==1]), collapse = "|")
culturepattern <- paste(paste0("^t_", topic_dict$term[topic_dict$cultural==1]), collapse = "|")
otherpattern <- paste(paste0("^t_", topic_dict$term[topic_dict$other==1]), collapse = "|")
