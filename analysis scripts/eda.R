# This script performs explorative data analysis: baseline characteristic
# and frequency of CT findings at the particular visits. Finally, distribution
# statistics of the CT severity score are calculated

  insert_head()
  
# container list ------
  
  eda <- list()
  
# baseline clinical characteristic -----
  
  insert_msg('Baseline clinics, entire cohort')
  
  ## analyses

  eda$baseline$analyses_cohort <- c('age', radio$long_expl_var) %>% 
    map(analyze_feature, 
        inp_tbl = rbind(mutate(radio$clear, dummy_var = factor('dummy1')), 
                        mutate(radio$clear, dummy_var = factor('dummy2'))), 
        split_var = 'dummy_var') %>% 
    set_names(c('age', radio$long_expl_var))

  ## summary table
  
  eda$baseline$summary_cohort <- eda$baseline$analyses_cohort %>% 
    map_dfr(get_feature_summary) %>% 
    mutate(label = translate_var(variable), 
           statistic = dummy1) %>% 
    select(variable, label, statistic)
  
# baseline characteristic in the severity groups -----
  
  insert_msg('Baseline clinics, severity groups')
  
  ## analyses
  
  eda$baseline$analyses_severity <- c('age', radio$long_expl_var) %>% 
    map(analyze_feature, 
        inp_tbl = radio$clear, 
        split_var = 'severity') %>% 
    set_names(c('age', radio$long_expl_var))
  
  ## summary tables, for age: ANOVA (homogeneous variances and normality, see the analysis object)
  
  eda$baseline$summary_severity <- eda$baseline$analyses_severity %>% 
    map_dfr(get_feature_summary) %>% 
    filter(variable != 'severity') %>% 
    mutate(p_value = ifelse(is.na(p_chi), p_non_param, p_chi)) %>% 
    add_stat_info %>% 
    select(variable, label, mild, moderate, severe, critical, test, p_value, significance, p_adj)
  
# CT features at the time points, entire cohort ------
  
  insert_msg('CT features, entire cohort')
  
  eda$ct$analyses_cohort <- radio$cat_outcome_var %>% 
    map(~analyze_feature(inp_tbl = radio$long[c('visit', .x)] %>% 
                           filter(complete.cases(.)),
                         variable = .x, 
                         split_var = 'visit')) %>% 
    set_names(radio$cat_outcome_var)
  
  eda$ct$summary_cohort <- eda$ct$analyses_cohort %>% 
    map_dfr(get_feature_summary) %>% 
    mutate(p_value = ifelse(is.na(fup3), NA, p_chi)) %>% 
    add_stat_info %>%
    select(variable, label, fup1, fup2, fup3, fup4, test, p_value, significance, p_adj)

# CT features at the time points, severity groups ------
  
  insert_msg('CT features, severity groups')
  
  eda$ct$analyses_visit <- radio$long %>% 
    dlply(.(severity)) %>% 
    map(function(sev_group) radio$cat_outcome_var %>% 
          map(~analyze_feature(inp_tbl = sev_group[c('visit', .x)] %>% 
                                 filter(complete.cases(.)),
                               variable = .x, 
                               split_var = 'visit')) %>% 
          set_names(radio$cat_outcome_var))
  
  eda$ct$summary_visit <- eda$ct$analyses_visit %>% 
    map(~map_dfr(.x, get_feature_summary)) %>% 
    map2_dfr(., names(.), ~mutate(.x, severity = .y)) %>% 
    mutate(p_value = ifelse(is.na(fup3), NA, p_chi)) %>% 
    add_stat_info %>%
    select(severity, variable, label, fup1, fup2, fup3, fup4, test, p_value, significance, p_adj)
  
# Differences in CT features between the severity groups ------
  
  insert_msg('CT features, severity groups')
  
  eda$ct$analyses_severity <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(function(visit) radio$cat_outcome_var %>% 
          map(~safely(analyze_feature)(inp_tbl = visit[c('severity', .x)] %>% 
                                 filter(complete.cases(.)),
                               variable = .x, 
                               split_var = 'severity')) %>% 
          set_names(radio$cat_outcome_var) %>% 
          map(~.x$result) %>% 
          compact)
  
  eda$ct$summary_severity <- eda$ct$analyses_severity %>% 
    map(~map_dfr(.x, get_feature_summary)) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y)) %>% 
    mutate(p_value = p_chi) %>% 
    add_stat_info %>%
    select(visit, variable, label, mild, moderate, severe, critical, test, p_value, significance, p_adj)
  
# Normality of the CT severity score at the time points -----
  
  insert_msg('CT severity score normality check')
  
  ## normality in the visit groups
  
  eda$ctss_normality$time_test_ident <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~shapiro_test(.x$ctss)) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  eda$ctss_normality$time_plot_ident <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~ggplot(.x, aes(sample = ctss)) + 
          geom_qq() + 
          geom_qq_line())
  
  ## normality in the time and time:severity groups
  
  eda$ctss_normality$time_sev_test_ident <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~shapiro_test(.x$ctss)) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  eda$ctss_normality$time_sev_plot_ident <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~ggplot(.x, aes(sample = ctss)) + 
          geom_qq() + 
          geom_qq_line())
  
  ## normality in the visit groups
  
  eda$ctss_normality$time_test_log <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~shapiro_test(log(.x$ctss + 1))) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  eda$ctss_normality$time_plot_log <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~ggplot(.x, aes(sample = log(ctss + 1))) + 
          geom_qq() + 
          geom_qq_line())
  
  ## normality of the log CTSS in the time and time:severity groups
  
  eda$ctss_normality$time_sev_test_log <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~shapiro_test(log(.x$ctss + 1))) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  eda$ctss_normality$time_sev_plot_log <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~ggplot(.x, aes(sample = log(ctss + 1))) + 
          geom_qq() + 
          geom_qq_line())

# Variance homogeneity of the CTSS at the time points and in the severity groups -----
  
  insert_msg('Variance homogeneity of the CTSS')
  
  eda$ctss_homogeneity <- list(ctss ~ visit, 
                               ctss ~ severity, 
                               log(ctss + 1) ~ visit, 
                               log(ctss + 1) ~ severity) %>% 
    map(levene_test, 
        data = radio$long) %>% 
    map2_dfr(., c('visit', 'severity', 'visit', 'severity'), ~mutate(.x, grouping = .y)) %>% 
    mutate(transformation = c('identity', 'identity', 'log', 'log'))

# CT score at the time points, entire cohort and the severity groups, non-parametric testing ------
  
  insert_msg('CT score at the visits')
  
  ## analyses
  
  eda$ctss$analyses <- radio$long %>% 
    dlply(.(severity)) %>% 
    c(list(cohort = radio$long), .) %>% 
    map(analyze_feature, 
        variable = 'ctss', 
        split_var = 'visit')
  
  ## summary table
    
  eda$ctss$summary <- eda$ctss$analyses %>% 
    map(get_feature_summary) %>% 
    map2_dfr(., names(.), ~mutate(.x, subset = .y)) %>% 
    mutate(p_value = p_non_param) %>% 
    add_stat_info %>% 
    mutate(test = 'Kruskal-Wallis') %>% 
    select(subset, fup1, fup2, fup3, fup4, test, significance, p_adj)
  
# END -----
  
  insert_tail()