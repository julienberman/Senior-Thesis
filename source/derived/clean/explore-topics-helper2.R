library(janitor)

setFixest_estimation(
  lean = T,
  mem.clean = F,
  fixef.iter = 10000,
  fixef.tol = 1e-8,
  collin.tol = 1e-9,
  verbose = 2
)
setFixest_nthreads(0, save = FALSE)


normalize = function(data) {
  
  data$normalization = data %>%
    select(starts_with('t_')) %>%
    rowSums()
  
  data = data %>%
    mutate(
      across(starts_with('t_'), function(x) x / normalization)
    ) %>%
    select(-normalization)
  
  data = data %>%
    mutate(
      t_econ = rowSums(data %>% select(matches(econpattern))),
      t_culture = rowSums(data %>% select(matches(culturepattern))),
      t_other = rowSums(data %>% select(matches(otherpattern)))
    ) %>%
    select(-length) %>%
    mutate(
      normalization = t_econ + t_culture + t_other,
      across(c(t_econ, t_culture, t_other), ~ . / normalization)
    ) %>%
    select(-normalization) %>%
    mutate(
      t_econ_minus_culture = t_econ - t_culture
    )
  
  data = data %>%
    sum_columns(parentdict)
  
  return(data)
}

get_amounts = function(data, grouping_variables = c()) {
  
  total_amounts = data %>%
    select(c(weight, starts_with('t_'), all_of(grouping_variables))) %>%
    pivot_longer(
      starts_with('t_'),
      names_to = 'term'
    ) %>%
    group_by(across(c('term', all_of(grouping_variables)))) %>%
    summarize(
      prevalence = sum(value * weight, na.rm = TRUE),
      weight = sum(weight, na.rm=T)
    ) %>%
    ungroup() %>%
    mutate(
      share = prevalence / weight
    )
  
  return(total_amounts)
}

process_news = function(data, weights = 'equal', timeslot, minimum_shows = 0, fullshow_switch = F) {
  data = data %>%
    janitor::clean_names() %>%
    filter(network %in% c('CNN', 'FNC', 'MSNBC')) %>%
    mutate(
      showname = show,
      dt = with_tz(dt, tzone = "America/New_York"),
      week = floor_date(dt, unit = 'week'),
      hour = hour(dt),
      day_of_year = day(dt),
      date = floor_date(dt, unit = 'day'),
      day = wday(dt, label = T),
      weekend = day %in% c('Sat', 'Sun'),
      monthday = mday(dt),
      primetime = hour >= 17 & hour <= 22 & !weekend,
      month = floor_date(dt, unit = 'month'),
      year = year(dt)
    ) %>%
    group_by(show, date) %>%
    arrange(dt) %>%
    mutate(
      primetime_cum = cumsum(primetime),
      primetime_average = mean(primetime)
    ) %>%
    ungroup() %>%
    mutate(
      show = interaction(show, hour)
    ) %>%
    mutate(
      n_households = winsorize(n_households, 0.001),
      network = factor(network, levels = c('CNN', 'FNC', 'MSNBC'))
    ) %>%
    mutate(
      competitor_n_households_inc_broadcast = total_news_viewership - n_households
    ) %>%
    group_by(dt) %>%
    mutate(
      competitor_n_households_exc_broadcast = sum(n_households) - n_households
    ) %>%
    ungroup() %>%
    group_by(network) %>%
    arrange(dt) %>%
    mutate(
      n_households_previous_timeslot = lag(n_households)
    ) %>%
    ungroup() %>%
    group_by(show) %>%
    arrange(dt) %>%
    mutate(
      n_households_previous_day = lag(n_households)
    ) %>%
    ungroup() %>%
    mutate(
      time = dt
    )
  
  if (timeslot == 'weekdays') {
    data = data %>% filter(!weekend)
  } else if (timeslot == 'primetime') {
    data = data %>% filter(primetime)
  }
  
  if (fullshow_switch) {
    data = data %>% filter(full_show == T)
  }
  
  data = data %>%
    group_by(show) %>%
    filter(n() >= minimum_shows) %>%
    ungroup()
  
  if (weights == 'network') {
    data = data %>% group_by(network)
  } else if (weights == 'year') {
    data = data %>% group_by(year)
  } else if (weights == 'network year') {
    data = data %>% group_by(network, year)
  }
  
  data = data %>%
    mutate(
      weight = 1 / n()
    ) %>%
    ungroup()
  
  return(data)
}

options(future.globals.maxSize = 2524^5)  # 100 GB

setFixest_fml(
  ..ad_controls = ~log(total_airings),
  ..base_2000 = ~log(population_2000),
  ..demo_2000 = ~share_white_2000 + share_black_2000 + share_hisp_2000 + share_other_2000 + log_income_per_capita_2000,
  ..voting_2000 = ~I(dem_votes_forward_presidential_2000 / total_votes_forward_presidential_2000),
  ..demo = ~share_white + share_black + share_hisp + share_other + log(population),
  ..voting = ~dem_voteshare_all_presidential_backward + dem_voteshare_all_governor_backward + dem_voteshare_all_senate_backward + dem_voteshare_all_house_backward,
  ..econ = ~unemprate + lfp + log_income_per_capita,
  ..fourthwall_individual = ~news_prop + party + educ + log(1 + income) + married + log(1 + net_worth) + has_children + male_hh
)

print("Reading ads")
ads = read_csv('data/working/classified-ads.csv') %>%
  janitor::clean_names() %>%
  mutate(
    weight = total_airings
  ) %>%
  group_by(year) %>%
  mutate(
    weight = weight / n()
  ) %>%
  ungroup() %>%
  mutate(
    length = 1
  )

ads_candidate_level = ads %>%
  group_by(sponsor_or_positive_id, year, primary) %>%
  summarize(
    across(
      starts_with(c('t_')), ~weighted.mean(., w = weight, na.rm = T)
    ),
    airings = sum(total_airings)
  ) %>%
  rename(
    id = sponsor_or_positive_id
  )

candidates = read_csv('data/working/candidates-augmented-with-demographics.csv') %>%
  mutate(
    yearraceparty = interaction(year, race, party, primary),
    yearracepartyincumbent = interaction(yearraceparty, incumbent),
    yearrace = interaction(year, race, primary),
    raceparty = interaction(race, party, primary),
    yearstate = interaction(year, state2, primary),
    general = primary == 0,
    seat = interaction(state2, district2, race, primary, party),
    n_videos = replace_na(n_videos, 0)
  ) %>%
  group_by(race_id) %>%
  arrange(desc(n_videos)) %>%
  mutate(
    min_videos = n_videos[2],
    win = voteshare == max(voteshare)
  ) %>%
  arrange(desc(voteshare)) %>%
  filter(
    general == F | row_number() <= 2
  ) %>%
  mutate(
    totalvotes = sum(votes)
  ) %>%
  ungroup() %>%
  filter(
    totalvotes < population
  ) %>%
  mutate(
    competitorvotes = totalvotes - votes,
    competitorvotes = ifelse(competitorvotes <= population / 25, NA, competitorvotes),
    votepop = votes / population,
    competitorvotepop = competitorvotes / population,
    competitive = dem_voteshare_all_house_backward < 0.55 & dem_voteshare_all_house_backward > 0.45
  ) %>%
  ungroup() %>%
  group_by(state2, district2, race) %>%
  arrange(year) %>%
  mutate(
    across(
      c(totalvotes, competitorvotes, votes, votepop, competitorvotepop), ~lag(.), .names = 'lag_{.col}'
    )
  ) %>%
  ungroup() %>%
  filter(
    party %in% c('Democrat', 'Republican')
  ) %>%
  inner_join(ads_candidate_level, by = c('id', 'year', 'primary')) %>%
  mutate(
    weight = airings,
    length = 1
  ) %>%
  normalize()

create_datasets = function(start_year, end_year, weights, timeslot, minimum_shows, debug = F, which_datasets = c('fourthwall', 'fourthwall_aggregate', 'fourthwall_disaggregate'), interval = 5) {
  
  adintel = read_csv('data/working/classified-news-showlevel.csv') %>%
    mutate(
      network = ifelse(network %in% c('ABC', 'CBS', 'NBC'), 'Broadcast', network)
    ) %>%
    filter(
      network %in% c('CNN', 'FNC', 'MSNBC')
    ) %>%
    mutate(
      length = 60
    )
  
  print('Reading aggregated fourthwall data')
  fourthwall_aggregate = arrow::read_feather(str_interp('data/working/fw-segment-${interval}.feather')) %>%
    mutate(
      length = as.numeric(interval)*60
    )
  
  print('Reading disaggregated fourthwall data')
  fourthwall_disaggregate = arrow::read_feather(str_interp('data/working/fw-segmentgroup-${interval}.feather')) %>%
    mutate(
      length = as.numeric(interval)*60
    ) %>% 
    select(-starts_with('t_')) 
  
  if (Sys.info()[['sysname']] == 'Linux' & T) {
    
    print('Reading Fourthwall')
    
    fourthwall = arrow::read_feather(str_interp('data/working/fw-ind-${interval}.feather')) %>%
      mutate(
        length = as.numeric(interval)*60
      )
    print('Read Fourthwall')
    viewership = read_csv('data/working/fourthwall-viewer-level.csv')
  }
  
  print(str_interp('Processing AdIntel'))
  if (debug) {
    adintel = adintel %>% sample_frac(0.1)
  }
  
  adintel_processed <- adintel %>%
    mutate(
      year = year(dt),
      length = 60
    ) %>%
    filter(
      year >= start_year, year <= end_year
    ) %>%
    process_news(weights = weights, timeslot = timeslot, minimum_shows = minimum_shows) %>%
    normalize() %>%
    drop_na(n_households, t_culture, t_econ, t_other, weight, show) %>%
    group_by(dt) %>%
    mutate(
      across(
        c('t_econ', 't_culture', 't_other', 't_econ_minus_culture'),
        ~ (sum(.) - .) / (n() - 1),
        .names = '{.col}_competitor'
      )
    ) %>%
    ungroup() %>%
    mutate(
      show_id = as.factor(show_id)
    )
  saveRDS(adintel_processed, file=str_interp('data/working/adintel_processed-${interval}.rds'))
  adintel_processed = adintel_processed %>% select(show_id, hour, day, primetime, n_households, weight, month, show, n_households_previous_timeslot, n_households_previous_day)
  rm(adintel)
  gc()
  
  if ('fourthwall_aggregate' %in% which_datasets) {
    fourthwall_aggregate_processed <- fourthwall_aggregate %>%
      group_by(segment_id) %>% 
      filter(n()==1) %>%
      ungroup() %>% 
      janitor::clean_names() %>%
      mutate(
        show_id = factor(str_extract(segment_id, "^[^-]+-[^-]+-[^-]+"), levels = adintel_processed$show_id),
        segment_id = as.factor(segment_id)
      ) %>%
      inner_join(
        adintel_processed %>% select(show_id, hour, day, n_households, weight, month, show, n_households_previous_timeslot, n_households_previous_day),
        by = 'show_id'
      ) %>%
      mutate(
        timeslot = as.numeric(difftime(granular_start_time, floor_date(granular_start_time, unit = "hour"), units = "mins")),
        time = granular_start_time
      ) %>%
      normalize() %>%
      drop_na(t_culture, t_econ, t_other, weight) %>%
      group_by(time) %>%
      mutate(
        total_fourthwall_viewership = sum(total_duration_watched),
        across(
          c('t_econ', 't_culture', 't_other', 't_econ_minus_culture'),
          ~ (sum(.) - .) / (n() - 1),
          .names = '{.col}_competitor'
        )
      ) %>%
      ungroup() %>%
      mutate(
        competitor_total_duration_watched = total_fourthwall_viewership - total_duration_watched
      )
    
    rm(fourthwall_aggregate)
    
    saveRDS(fourthwall_aggregate_processed, file=str_interp('data/working/fourthwall_aggregate_processed-${interval}.rds'))
    fourthwall_aggregate_processed = fourthwall_aggregate_processed %>% 
      select(segment_id, time, starts_with('t_'), timeslot, hour)
    
    gc()
  } else {
    fourthwall_aggregate_processed = NA
  }
  
  if (Sys.info()[['sysname']] == 'Linux' & ('fourthwall' %in% which_datasets)) {
    
    print(str_interp('Processing FourthWall'))
    fourthwall_processed = fourthwall %>%
      group_by(hh_id, network) %>%
      filter(n() >= 50) %>%
      ungroup()
    # ids = levels(fourthwall$hh_id) %>% sample(5000)
    # fourthwall = fourthwall %>% filter(hh_id %in% ids)
    
    
    print('Sampled 2')
    fourthwall_processed = fourthwall_processed %>%
      janitor::clean_names() %>%
      mutate(
        show_id = factor(str_extract(segment_id, "^[^-]+-[^-]+-[^-]+"), levels = adintel_processed$show_id),
        segment_id = factor(segment_id, levels = unique(fourthwall_aggregate_processed$segment_id))
      )
    print('Created show and segment ids')
    fourthwall_processed = fourthwall_processed %>%
      inner_join(
        adintel_processed %>% select(show_id, day, n_households, month, weight, show, n_households_previous_timeslot, n_households_previous_day),
        by = 'show_id'
      )
    print('Joined with adintel')
    
    # mutate(
    #   aligned = (party == "Republican" & network %in% c('FNC')) | (party == 'Democratic' & network == 'MSNBC'),
    #   misaligned = (party == 'Democratic' & network == 'FNC') | (party == 'Republican' & network == 'MSNBC'),
    #   party = ifelse(party %in% c('No Party', 'Independent'), 'None/Independent', party)
    # )
    fourthwall_processed = fourthwall_processed %>%
      inner_join(
        fourthwall_aggregate_processed %>% select(t_econ_minus_culture, t_econ_minus_culture_competitor, t_culture, t_econ, t_other, timeslot, time, hour, segment_id),
        by = c('segment_id')
      ) %>%
      drop_na(prop_watched, t_culture, t_econ, t_other, weight, show)
    
    saveRDS(fourthwall_processed, file=str_interp('data/working/fourthwall_processed-${interval}.rds'))
    rm(fourthwall, fourthwall_processed)
    gc()
    print('Created fourthwall_processed')
    
  } else {
    fourthwall_processed = NA
  }
  
  
  if ('fourthwall_disaggregate' %in% which_datasets) {
    print('Processing FourthWall disaggregate')
    fourthwall_disaggregate_processed <- fourthwall_disaggregate %>%
      janitor::clean_names() %>%
      mutate(
        show_id = str_extract(segment_id, "^[^-]+-[^-]+-[^-]+")
      ) %>%
      inner_join(
        adintel_processed %>% select(show_id, hour, day, month, n_households, weight, show, n_households_previous_timeslot, n_households_previous_day),
        by = 'show_id'
      ) %>%
      inner_join(
        fourthwall_aggregate_processed %>% select(starts_with('t_'), segment_id, timeslot, time),
        by = c('segment_id')
      ) %>%
      drop_na(mean_prop_watched, t_culture, t_econ, t_other, weight, show) %>% 
      group_by(time, measure) %>% 
      mutate(total_duration_watched_all = sum(total_duration_watched, na.rm=T)) %>%
      ungroup() %>%
      mutate(total_duration_watched_competitor = total_duration_watched_all - total_duration_watched) 
  } else {
    fourthwall_disaggregate_processed = NA
  }
  print('Created fourthwall_disaggregate_processed')
  saveRDS(fourthwall_disaggregate_processed, file=str_interp('data/working/fourthwall_disaggregate_processed-${interval}.rds'))
  
}

coerce_single_feols = function(model) {
  model = tidy(model)
  names(model) = c('coefficient', 'Estimate', 'Std. Error', 't value', 'Pr(>|t|)')
  return(model)
}

adintel = readRDS(str_interp('data/working/adintel_processed-60.rds'))

ads_amount = get_amounts(candidates) %>% mutate(domain = 'Ads') 
tv_amount = get_amounts(adintel) %>% mutate(domain = 'News') 
amounts = ads_amount %>% 
  bind_rows(tv_amount) %>% 
  mutate(term = str_remove_all(term, '^t_')) %>% 
  inner_join(topic_dict, by='term') %>% 
  filter(!exclude_from_comparison) %>% 
  group_by(domain) %>% 
  mutate(domain = factor(domain, levels=c('News', 'Ads'))) %>% 
  ungroup() %>% 
  group_by(topic) %>% 
  mutate(total_share = sum(share, na.rm=T)) %>% 
  ungroup() 
amounts_wide = amounts %>% select(term, topic, share, domain) %>% 
  pivot_wider(values_from=share, names_from = domain) %>% 
  mutate(share = Ads+News) %>% 
  arrange(desc(share)) %>% 
  mutate(news_ads_diff = News-Ads, news_ads_logodds = log(News/Ads),
         topic = factor(topic, levels=rev(topic)))

amounts$topic = factor(amounts$topic, levels=rev(amounts_wide$topic))