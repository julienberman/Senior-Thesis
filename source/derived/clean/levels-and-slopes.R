source('code/analysis/load.R')
source('code/analysis/explore-topics-helper2.R')
new = F
for (interval in c(15)) {
if (new) {
    create_datasets(start_year = 2011, end_year = 2020, weights = 'equal', timeslot = 'all', minimum_shows = 0, debug=F, interval=interval)  
}
}

interval = 15
sharemin = 0.0001
additional = c('t_econ_minus_culture','t_econ','t_culture','t_other')
regressors_short = c(paste0('t_', unique(amounts$term[amounts$header])), additional)
regressors = unique(c(paste0('t_', amounts$term[amounts$share>=sharemin]), additional, regressors_short))
estimate_news_figure = function(regressors, short_regressors) {
    regs = regressors %>% paste(collapse = ", ")
    regs_interacted = paste0(regressors, ':persuadability')  %>% paste0(collapse = ", ")

    short_regs = short_regressors %>% paste(collapse = ", ")
    short_regs_interacted = paste0(short_regressors, ':persuadability')  %>% paste0(collapse = ", ")

    adintel = readRDS(str_interp('data/working/adintel_processed-${interval}.rds')) 
    f1 = as.formula(paste("c(log(n_households), log(competitor_n_households_exc_broadcast), log(competitor_n_households_inc_broadcast), n_households, competitor_n_households_exc_broadcast, competitor_n_households_inc_broadcast) ~ sw(", regs, ") + t_econ_minus_culture_competitor + log(n_households_previous_timeslot) + log(n_households_previous_day) | interaction(show, month) + interaction(show, day)"))
    adintel_models = feols(f1, data=adintel, weights=~weight, cluster=~dt, fsplit=~network)

    fourthwall_aggregate = readRDS(str_interp('data/working/fourthwall_aggregate_processed-${interval}.rds'))
    f2 = as.formula(paste("c(mean_watched_to_end, mean_duration_watched, log(total_duration_watched), I(mean_turn_off+mean_switch_to_non_news), mean_switch_to_news) ~ sw(", short_regs, ") | interaction(show, month)+interaction(show, day)"))
    fw_agg_models = feols(f2, data=fourthwall_aggregate, cluster=~interaction(date, hour), weights=~weight)
    rm(fourthwall_aggregate); gc()

    if (F) {
      fourthwall = readRDS(str_interp('data/working/fourthwall_processed-${interval}.rds')) %>% filter(measure=='persuadability_conditional_nom') %>% rename(persuadability = persuadability_conditional_nom)
      fourthwall = fourthwall  %>% 
        mutate(insample = TRUE) %>%
        group_by(hh_id, show) %>% 
        filter(n()>=100) %>% 
        ungroup()
      debug = T
      if (debug) {
        ids = fourthwall$hh_id %>% unique() %>% sample(8000)
        fourthwall = fourthwall %>% 
          mutate(
            rand = runif(n()),
            insample = hh_id %in% ids & rand < 0.25
      )
      }
      fourthwall = fourthwall %>% filter(insample)
      #f3 = as.formula(paste("c(watched_to_end*100, duration_watched) ~ sw(", regs, ") +log(n_households_previous_timeslot) + log(n_households_previous_day) | interaction(network, hour, month, timeslot)+interaction(network, hour, day, timeslot)+interaction(show, year, hh_id)"))

      #f3 = as.formula(paste("c(watched_to_end*100, duration_watched) ~ sw(", regs, ") +log(n_households_previous_timeslot) + log(n_households_previous_day) | interaction(show, month, timeslot)+interaction(show, day, timeslot)+interaction(show, hh_id, timeslot)"))
      #fw_ind_models_agg = feols(f3, data=fourthwall %>% filter(insample), cluster=~hh_id, weights=~weight)
      f4 = as.formula(paste("c(prop_watched, duration_watched) ~ sw(", regs_interacted, ") | segment_id + interaction(hh_id, show, month) + interaction(hh_id, show, day)"))
      fw_ind_models_disagg = feols(f4, data=fourthwall , cluster=~hh_id+interaction(date, hour), weights=~weight)
      rm(fourthwall); gc()
    }
    
    fourthwall_disaggregate = readRDS(str_interp('data/working/fourthwall_disaggregate_processed-${interval}.rds')) %>% filter(measure=='persuadability_nom') %>% rename(persuadability = persuadability_measure)
    f5 = as.formula(paste("c(mean_prop_watched, log(total_duration_watched)) ~ sw(", short_regs, ") | interaction(persuadability, show, month) + interaction(persuadability, show, day)"))
    fw_agg_models = feols(f5, data=fourthwall_disaggregate, cluster=~show, weights=~weight)

    f6 = as.formula(paste("c(mean_prop_watched, log(total_duration_watched)) ~ sw(", short_regs_interacted, ") | segment_id + interaction(persuadability, show, month) + interaction(persuadability, show, day)"))
    fw_disagg_models = feols(f6, data=fourthwall_disaggregate, cluster=~show, weights=~weight)
    rm(fourthwall_disaggregate); gc()
    
    coefs = bind_rows(
        coeftable(adintel_models) %>% mutate(dataset='adintel'),
        coeftable(fw_agg_models) %>% mutate(dataset='fw_agg'),
        #coeftable(fw_ind_models_agg) %>% mutate(dataset='fw_ind_agg'),
        #coeftable(fw_ind_models_disagg) %>% mutate(dataset='fw_ind_disagg'),
        coeftable(fw_disagg_models) %>% mutate(dataset='fw_disagg')
    ) %>% filter(str_starts(coefficient, 't_'), !str_detect(coefficient, 'competitor'))
    return(coefs)
}


estimate_politician_figure = function(regressors) {
  regs = regressors %>% paste(collapse = ", ")

  f3 = as.formula(paste("c(win, voteshare, log(competitorvotes), competitorvotepop, log(votes), votepop) ~ sw(", regs, ") +race*(..demo+..voting_2000+..econ+..voting+..demo_2000+..ad_controls+incumbent*(voteshare_all_backward+voteshare_twoparty_backward))|yearraceparty+seat"))
  models =  feols(f3, data=candidates %>% filter(!unopposed, general), weights=~population, cluster=~race_id)
  models_party =  feols(f3, data=candidates, weights=~population, cluster=~race_id, split=~party)
  coefs = coeftable(models) %>% filter(str_starts(coefficient, 't_'))
  coefs = coeftable(models) %>% filter(str_starts(coefficient, 't_'))
  return(coefs)
}

create_tv_tables = function() {
    debug = T
    adintel = readRDS(str_interp('data/working/adintel_processed-${interval}.rds'))

    n_hh = feols(log(n_households)*100~t_econ_minus_culture+log(n_households_previous_timeslot) + log(n_households_previous_day)|interaction(show, month)+interaction(show, day)+sw0(interaction(network, date)), data=adintel, weights=~weight, cluster=~interaction(date))
    fw_disagg = readRDS(str_interp('data/working/fourthwall_disaggregate_processed-${interval}.rds')) %>% filter(measure=='persuadability_nom') %>% rename(persuadability = persuadability_measure)
    f5 = as.formula(c(log(total_duration_watched)*100, mean_prop_watched*100) ~ t_econ_minus_culture | interaction(persuadability, show, month) + interaction(persuadability, show, day) + sw0(interaction(network, date, persuadability), interaction(network, hour, date, persuadability)))
    fwmodels = feols(f5, data=fw_disagg, cluster=~date, weights=~weight)

    models = list(fwmodels[[1]], fwmodels[[3]], fwmodels[[2]], fwmodels[[4]], n_hh[[1]], n_hh[[2]])

    rows <- list('_^Show-month-TS FE' = c('Yes', '---', 'Yes', '---', 'Yes', 'Yes'),
                 '_^Show-DoW-TS FE' = c('Yes', '---', 'Yes', '---', 'Yes', 'Yes'),
                '_^Network-date FE' = c('No', '---', 'No', '---', 'No', 'Yes'),
                 '_^Episode FE' = c('No', 'Yes', 'No', 'Yes', 'No', 'No'))

  rows = rev(rows)
  
  dict = c('t_econ_minus_culture' = 'Relative econ %')
  
  
  table1 = etable(models, 
    dict = dict, 
    keep='%t_econ_minus_culture',
    drop='other',
    headers=list('Log(seconds)' = 2, '% watched' = 2, 'Log(HHs)' = 2),
    style.df = style.df(stats.title=''),
    extralines = rows,
    title = 'TV performance: Effect of economic versus cultural content on viewership',
    label = 'tab:tv-performance',
    depvar = F,
    notes=paste0('\\vspace{0.2cm} {\\small \\textit{Notes:} ', tablenotes$`tv-performance`, '}'),
    digits.stats=2,
    drop.section='fixef',
    style.tex = style.tex("aer"), signif.code = NA, fitstat = ~ r2 + n, tex = TRUE)

  table2 = c(
    table1[1],
    table1[4:6],
    '& \\multicolumn{4}{c}{FourthWall} & \\multicolumn{2}{c}{AdIntel} \\\\',
    '\\cmidrule(lr){2-5} \\cmidrule(lr){6-7}',
    table1[7],
    '\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}',
    '\\midrule',
    table1[10:11],
    '\\addlinespace',
    table1[13:16],
    '\\addlinespace',
    table1[18:21],
    table1[length(table1)]
  )
  write_lines(table2, file = str_interp('output/tables/tv-performance.tex'))
  return(table2)
  #comparison = feols(c(other_n_households_exc_broadcast, n_households)~t_econ_minus_culture+t_econ_minus_culture_other|csw(showmonth+interaction(show, hourday), interaction(network, date)), data=adintel, weights=~weight, cluster=~dt)
  
  m1 = feols(log(other_n_households_exc_broadcast)*100~t_econ_minus_culture+t_econ_minus_culture_other|interaction(show, month)+interaction(show, day)+interaction(network, date), data=adintel, weights=~weight, cluster=~dt)
  print(summary(m1))
  m2 = feols(log(other_total_duration_watched)*100~t_econ_minus_culture+t_econ_minus_culture_other|interaction(show, month, timeslot)+interaction(show, day, timeslot)+interaction(network, date, timeslot), data=fw_agg, weights=~weight, cluster=~time)
  m2 = feols(log(other_total_duration_watched)*100~t_econ_minus_culture+t_econ_minus_culture_other|interaction(show, month, timeslot)+interaction(show, day, timeslot)+interaction(network, date, timeslot), data=fw_agg, weights=~weight, cluster=~time)
  print(summary(m2))
  fw_disagg = readRDS(str_interp('data/working/fourthwall_disaggregate_processed-${interval}.rds')) %>% filter(measure=='persuadability_conditional_nom') %>% rename(persuadability = persuadability_conditional_nom)
  fw_disagg = readRDS(str_interp('data/working/fourthwall_disaggregate_processed-${interval}.rds')) %>% filter(measure=='persuadability_conditional_nom') %>% rename(persuadability = persuadability_conditional_nom)
  #fw_disagg_sample = fw_disagg %>% filter(year==2016) %>% sample_frac(0.05)
  #write_rds(fw_disagg_sample, str_interp('data/working/fourthwall_disaggregate_sample-${interval}.rds'))
#  m3 = feols(log(total_duration_watched)*100 ~ scale(network_prop_news):t_econ_minus_culture + log(total_news_hours):t_econ_minus_culture | segment_id+interaction(total_news_hours, show, day, timeslot, network_prop_news)+interaction(month, show, news_consumption, total_news_hours, network_prop_news), data=fw_disagg %>% sample_frac(0.05), cluster=~time, weights=~weight)
  
  #m3 = feols(log(total_duration_watched)*100 ~ scale(network_prop_news):scale(t_econ_minus_culture) + scale(total_news_hours):scale(t_econ_minus_culture) | segment_id+interaction(total_news_hours, show, day, timeslot)+interaction(month, show, timeslot, total_news_hours) + interaction(network_prop_news, show, day, timeslot)+interaction(month, show, timeslot, network_prop_news), data=fw_disagg, cluster=~time, weights=~weight)
  m3 = feols(c(I(mean_switch_to_non_news+mean_turn_off), mean_watched_to_end, mean_switch_to_news, mean_prop_watched, log(total_duration_watched)*100) ~ persuadability_nom:t_econ_minus_culture + persuadability_nom*started | segment_id+interaction(persuadability_nom, network, hour, month, timeslot) + interaction(persuadability_nom, network, hour, day, timeslot) , data=fw_disagg, cluster=~time, weights=~weight, fsplit=~network)
  m3 = feols(c(I(mean_switch_to_non_news+mean_turn_off), mean_watched_to_end, mean_switch_to_news, mean_prop_watched, log(total_duration_watched)*100) ~ persuadability:t_econ_minus_culture + persuadability | interaction(segment_id)+interaction(persuadability, network, hour, month, timeslot) + interaction(persuadability, network, hour, day, timeslot) , data=fw_disagg %>% filter(network=='FNC')  %>% mutate(persuadability = persuadability_conditional_nom), cluster=~time, weights=~weight)
  m3 = feols(c(I(mean_switch_to_non_news+mean_turn_off), mean_watched_to_end, mean_switch_to_news, mean_prop_watched, log(total_duration_watched)*100) ~ persuadability*t_econ_minus_culture | interaction(persuadability, network, hour, month, timeslot) + interaction(persuadability, network, hour, day, timeslot) , data=fw_disagg %>% mutate(persuadability = persuadability_conditional_nom), cluster=~time, weights=~weight, fsplit=~network)

  print(summary(m3)) 
  
  
  # this one works pretty well
  m4 = feols(c(prop_watched*100, I(switch_to_non_news+turn_off)*100, watched_to_end*100, switch_to_news*100) ~ t_econ_minus_culture  |  interaction(network, hour, day, timeslot, hh_id), data = fw, weights = ~weight, cluster = ~hh_id+show_id)
  
  #m4 = feols(c(prop_watched*100, turn_off*100, switch_to_other_news_network*100) ~ t_econ_minus_culture  | interaction(show, month, granular_start_second)+interaction(show, day, granular_start_second)+ sw(interaction(network, hour, granular_start_second, hh_id), interaction(network, hour, day, granular_start_second, hh_id)), data = fw, weights = ~weight, cluster = ~hh_id+show_id)
  

# this one works pretty well
  m4 = feols(c(prop_watched*100, I(switch_to_non_news+turn_off)*100, watched_to_end*100, switch_to_news*100) ~ t_econ_minus_culture  |  interaction(network, hour, day, timeslot, hh_id), data = fw, weights = ~weight, cluster = ~hh_id+show_id)

  #m4 = feols(c(prop_watched*100, turn_off*100, switch_to_other_news_network*100) ~ t_econ_minus_culture  | interaction(show, month, granular_start_second)+interaction(show, day, granular_start_second)+ sw(interaction(network, hour, granular_start_second, hh_id), interaction(network, hour, day, granular_start_second, hh_id)), data = fw, weights = ~weight, cluster = ~hh_id+show_id)
  print(summary(m4))

  #m5 = feols(c(prop_watched*100, turn_off, switch_to_other_news_network) ~ scale(network_prop_news)*t_econ_minus_culture + log(total_news_hours)*t_econ_minus_culture |  segment_id+sw(interaction(network, hour, granular_start_second, hh_id), interaction(network, hour, day, granular_start_second, hh_id)), data = fw, weights = ~weight, cluster = ~hh_id+show_id)
  m5 = feols(c(prop_watched*100, I(switch_to_non_news+turn_off), watched_to_end*100, switch_to_news) ~ persuadability*t_econ_minus_culture |  segment_id+sw(interaction(network, hour, timeslot, hh_id)), data = fw, weights = ~weight, cluster = ~hh_id+show_id)

  print(summary(m5))
  browser()
  
  models = list(m1, m2, m3, m5[[1]], m4[[5]], m4[[6]])
  
  dict = c(dict, 
           'log(other_n_households_exc_broadcast)' = 'Log viewership of other cable news channels',
           'log(other_total_duration_watched)' = 'Log total seconds viewed of other cable news channels',
           'scale(network_prop_news):t_econ_minus_culture' = 'Relative econ % $\\times$ OO',
           'scale(total_news_hours):t_econ_minus_culture' = 'Relative econ % $\\times$ NH',
           't_econ_minus_culture:total_news_hours' = 'Relative econ % $\\times$ NH')
  
  rows <- list(
    '_^Show-month FE' = c('Yes', 'Yes', '---', '---', 'Yes', 'Yes'),
    '_^Show-DoW FE' = c('Yes', 'Yes', '---', '-HH', '-HH', '-HH'),
    '_^Network-date FE' = c('Yes', 'Yes', '---', '---', 'Yes', 'Yes'),
    '_^Segment FE' = c('No', 'No', 'Yes', 'Yes', 'No', 'No'))
  rows = rev(rows)
  
  slopes = etable(models, 
                  dict = dict, 
                  keep='%t_econ_minus_culture',
                  drop='other',
                  headers=list('Viewers' = 2, 'Seconds' = 2, 'Seconds ' = 2),
                  style.df = style.df(stats.title=''),
                  extralines = rows,
                  title = 'TV substitution: Effect of economic versus cultural content',
                  label = 'tab:tv-substitution',
                  depvar = F,
                  notes= paste0('\\vspace{0.2cm} {\\small \\textit{Notes:} ', tablenotes$`tv-substitution`, '}'),
                  digits.stats=2,
                  drop.section='fixef',
                  style.tex = style.tex("aer"), signif.code = NA, fitstat = ~ r2 + n, tex = TRUE)
  
  slopes_table = c(
    slopes[1:6],
    '& \\multicolumn{2}{c}{Other news} & \\multicolumn{4}{c}{Own} \\\\',
    '\\cmidrule(lr){2-3} \\cmidrule(lr){6-8}',
    '& \\multicolumn{1}{c}{Viewers} & \\multicolumn{1}{c}{Seconds} & Seconds & \\% watched & Turned off & Switched news \\\\',
    '\\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4}  \\cmidrule(lr){5-5}  \\cmidrule(lr){6-6} ', 
    slopes[8:15],
    '\\addlinespace',
    slopes[17:20],
    '\\addlinespace',
    slopes[22:length(slopes)]
  )
  write_lines(slopes_table, file = str_interp('output/tables/tv-substitution.tex'))
  
}

create_politician_tables = function() {
  
  candidates = candidates %>% 
    mutate(across(c(competitorvotepop, votepop), ~ ifelse(. > 1, NA, .))) %>%
      group_by(race_id) %>% 
      mutate(across(c('t_econ','t_culture','t_other','t_econ_minus_culture'), ~ (sum(.) - .)/(n()-1), .names='{.col}_other' )) %>%
      ungroup()

  models = list(
    feols(win~t_econ_minus_culture+t_econ_minus_culture_other+race*(..demo+..voting_2000+..demo_2000+incumbent)|yearraceparty+seat, data=candidates %>% filter(!unopposed, general), cluster=~race_id), 
    feols(win~t_econ_minus_culture+t_econ_minus_culture_other+race*(..demo+..voting_2000+..demo_2000+..econ+..voting+..ad_controls+incumbent*(voteshare_all_backward+voteshare_twoparty_backward))|yearraceparty+seat, data=candidates %>% filter(!unopposed, general), cluster=~race_id, weights=~population),
    feols(win~t_econ_minus_culture+race*(..demo+..voting_2000+..demo_2000+..econ+..voting+..ad_controls+incumbent*(voteshare_all_backward+voteshare_twoparty_backward))|yearraceparty+seat+race_id, data=candidates %>% filter(!unopposed, general), cluster=~race_id, weights=~population),
    feols(win~t_econ_minus_culture+t_econ_minus_culture_other+race*(..demo+..voting_2000+..demo_2000+..econ+..voting+..ad_controls+incumbent*(voteshare_all_backward+voteshare_twoparty_backward))|yearraceparty+seat, data=candidates %>% filter(!unopposed, general, party=='Democrat'), cluster=~race_id, weights=~population),
    feols(win~t_econ_minus_culture+t_econ_minus_culture_other+race*(..demo+..voting_2000+..demo_2000+..econ+..voting+..ad_controls+incumbent*(voteshare_all_backward+voteshare_twoparty_backward))|yearraceparty+seat, data=candidates %>% filter(!unopposed, general, party=='Republican'), cluster=~race_id, weights=~population),
    feols(win~t_econ_minus_culture+t_econ_minus_culture_other+race*(..demo+..voting_2000+..demo_2000+..econ+..voting+..ad_controls+incumbent*(voteshare_all_backward+voteshare_twoparty_backward))|yearraceparty+seat, data=candidates %>% filter(!unopposed, general, race!='House'), cluster=~race_id, weights=~population)
  )
  
  dict = c( 
    'win' = 'Won election',
    't_econ_minus_culture' = 'Relative econ %'
  )
  
  rows <- list(
    '_^Year-office-party FE' = c('Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
    '_^Seat-party FE' = c('Yes', 'Yes', '---', 'Yes', 'Yes', 'Yes'),
    '_^Recent econ and voting controls' = c('No', 'Yes', '---', 'Yes', 'Yes', 'Yes'),
    '_^Race FE' = c('No', 'No', 'Yes', 'No', 'No', 'No'))
  rows = rev(rows)
  
  politician_table = etable(models, 
                            dict = dict, 
                            keep='%t_econ_minus_culture',
                            drop='other',
                            style.df = style.df(stats.title=''),
                            extralines = rows,
                            title = 'Politician performance: Effect of economic versus cultural content on electoral outcomes',
                            label = 'tab:politician-performance',
                            depvar = F,
                            notes=paste0('\\vspace{0.2cm} {\\small \\textit{Notes:} ', tablenotes$`politician-performance`, '}'),
                            digits.stats=2,
                            drop.section='fixef',
                            style.tex = style.tex("aer"), signif.code = NA, fitstat = ~ r2 + n, tex = TRUE)
  
  politician_lines = c(
    politician_table[1:6],
    '& \\multicolumn{6}{c}{Won election} \\\\',
    '\\cmidrule(lr){2-7}',
    politician_table[7:10],
    '\\addlinespace',
    'Sample restriction & --- & --- & --- & Dems & Reps & No House \\\\',
    '\\addlinespace',
    politician_table[12:15],
    '\\addlinespace',
    politician_table[16:length(politician_table)]
  )
  write_lines(politician_lines, file = str_interp('output/tables/politician-performance.tex'))
  return(politician_lines)
  models = feols(c(log(othervotes), othervotepop, log(votes), votepop)~t_econ_minus_culture+t_econ_minus_culture_other+race*(..demo+..voting_2000+..demo_2000+..econ+..voting+..ad_controls+incumbent*(voteshare_all_backward+voteshare_twoparty_backward))|yearraceparty + seat, data=candidates %>% filter(!unopposed, general), cluster=~race_id, weights=~population)
  models2 = feols(c(log(othervotes), othervotepop, log(votes), votepop)~t_econ_minus_culture+t_econ_minus_culture_other+race*(..demo+..voting_2000+..demo_2000+..econ+..voting+..ad_controls+incumbent*(voteshare_all_backward+voteshare_twoparty_backward))|yearraceparty + seat, data=candidates %>% filter(!unopposed, general, race!='House'), cluster=~race_id, weights=~population)
  othervotes_models = list(models[[1]], models2[[1]], models[[2]], models2[[2]])
  ownvotes_models = list(models[[3]], models2[[3]], models[[4]], models2[[4]])
  
  dict = c('t_econ_minus_culture' = 'Relative econ %')
  
  othervotes_lines = etable(othervotes_models, 
                            dict = dict, 
                            keep='%t_econ_minus_culture',
                            drop='other',
                            headers=list('Log votes' = 2, 'Vote proportion' = 2),
                            style.df = style.df(stats.title=''),
                            title = 'Politician substitution: Effect of economic versus cultural content on electoral outcomes',
                            label = 'tab:politician-substitution',
                            depvar = F,
                            notes=paste0('\\vspace{0.2cm} {\\small \\textit{Notes:} ', tablenotes$`politician-substitution`, '}'),
                            digits.stats=2,
                            drop.section='fixef',
                            style.tex = style.tex("aer"), signif.code = NA, fitstat = ~ r2 + n, tex = TRUE)
  
  ownvotes_lines = etable(ownvotes_models,
                          dict = dict, 
                          keep='%t_econ_minus_culture',
                          drop='other',
                          headers=list('Log own votes' = 2, 'Own vote prop' = 2),
                          style.df = style.df(stats.title=''),
                          depvar = F,
                          notes='test',
                          digits.stats=2,
                          drop.section='fixef',
                          style.tex = style.tex("aer"), signif.code = NA, fitstat = ~ r2 + n, tex = TRUE)
  
  together = c(
    othervotes_lines[1:7],
    '\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}',
    othervotes_lines[8:9],
    "\\textbf{Panel A}: & \\multicolumn{4}{c}{\\textit{Competitor's votes}} \\\\",
    '\\midrule',
    othervotes_lines[10:11],
    '\\addlinespace',
    othervotes_lines[13:14],
    '\\midrule',
    "\\textbf{Panel B}: & \\multicolumn{4}{c}{\\textit{Own votes}} \\\\",
    '\\midrule',
    ownvotes_lines[8:9],
    '\\addlinespace',
    ownvotes_lines[11:12],
    '\\addlinespace',
    '\\midrule',
    'Sample restriction & --- & No House & --- & No House \\\\',
    othervotes_lines[15:length(othervotes_lines)]
  )
  write_lines(together, file = str_interp('output/tables/politician-substitution.tex'))
}

create_diversion_table = function() {
  fw_disagg = readRDS(str_interp('data/working/fourthwall_disaggregate_processed-${interval}.rds')) %>% filter(measure=='persuadability_nom') %>% rename(persuadability = persuadability_measure)
  f5 = as.formula(c(log(total_duration_watched)*100, mean_prop_watched*100, mean_switch_to_news*100, mean_switch_to_non_news*100) ~ t_econ_minus_culture*scale(persuadability) | interaction(persuadability, show, month) + interaction(persuadability, show, day))
  fwmodels = feols(f5, data=fw_disagg, cluster=~interaction(date, hour), weights=~weight)

  candidates = candidates %>% 
      mutate(across(c(competitorvotepop, votepop), ~ ifelse(. > 1, NA, .))) %>%
        group_by(race_id) %>% 
        mutate(across(c('t_econ','t_culture','t_other','t_econ_minus_culture'), ~ (sum(.) - .)/(n()-1), .names='{.col}_competitor' )) %>%
        ungroup()

  polmodels = feols(c(votepop*100, competitorvotepop*100)~t_econ_minus_culture+race*(..demo+..voting_2000+..demo_2000+incumbent)|yearraceparty+seat, data=candidates %>% filter(!unopposed, general), cluster=~race_id)
  dict = c( 
    't_econ_minus_culture:scale(persuadability)' = 'Relative econ % $\\times \\Delta$',
    't_econ_minus_culture' = 'Relative econ %'
  )

  diversiontable = etable(list(fwmodels, polmodels), 
    dict = dict, 
    keep='%t_econ_minus_culture',
    style.df = style.df(stats.title=''),
    label = 'tab:persuasion-premium',
    depvar = F,
    digits.stats=2,
    drop.section='fixef',
    style.tex = style.tex("aer"), signif.code = NA, fitstat = ~ r2 + n, tex = TRUE)
  
  diversion_lines = c(
    diversiontable[1],
    diversiontable[4:6],
    '& \\multicolumn{2}{c}{Duration} & \\multicolumn{2}{c}{Switch channel} & \\multicolumn{2}{c}{Votes} \\\\',
    '\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}',
    ' & Log total & Mean & Comp. & Non-news & Own & Comp. \\\\',
    diversiontable[8:12],
    '\\addlinespace',
    diversiontable[15:length(diversiontable)]
  )
  write_lines(diversion_lines, file = str_interp('output/tables/persuasion-premium.tex'))
}


create_politician_tables()
create_tv_tables()
news_coefs = estimate_news_figure(regressors, regressors_short)

regressors_pol = unique(c(paste0('t_', amounts$term[amounts$share>=sharemin & amounts$domain=='Ads']), additional))
politician_coefs = estimate_politician_figure(regressors_pol)
regressors_pol = unique(c(paste0('t_', amounts$term[amounts$share>=sharemin & amounts$domain=='Ads']), additional))
politician_coefs = estimate_politician_figure(regressors_pol)

coefs = bind_rows(news_coefs %>% mutate(domain='news'), politician_coefs %>% mutate(domain='ads')) %>% 
coefs = bind_rows(news_coefs %>% mutate(domain='news'), politician_coefs %>% mutate(domain='ads')) %>% 
  rename(estimate = Estimate,
         se = `Std. Error`,
         p = `Pr(>|t|)`) %>% 
  filter(str_detect(coefficient, 't_')) %>% 
  mutate(coefficient = str_remove_all(coefficient, '^t_')) %>%
  select(-rhs, -`t value`, -sample.var)
  mutate(coefficient = str_remove_all(coefficient, '^t_')) %>%
  select(-rhs, -`t value`, -sample.var)

write_csv(coefs, str_interp('data/working/coefs-${interval}.csv'))

for_shakked = function() {

  fourthwall_disaggregate = readRDS(str_interp('data/working/fourthwall_disaggregate_processed-${interval}.rds'))

  data = fourthwall_disaggregate %>% 
    mutate(measure = as.factor(measure)) %>%
    select(persuadability_measure, all_of(regressors_short), network, day, hour, measure, date, timeslot, show, total_duration_watched, mean_duration_watched)

  data %>% write_csv('data/working/fw-disagg-for-shakked.csv')
}