# This script performs distribution checks of the numeric outcome variables

  insert_head()
  
# container list -----
  
  distr <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  distr$responses <- c('ctss', 'perc_opac', 'perc_hiopac', 'log_ctss', 'log_perc_opac', 'log_perc_hiopac')
  
# Normality of the CT severity score, percent opacity and percent high opacity at the time points: log and identity -----
  
  insert_msg('CT severity score normality check')
  
  ## normality in the visit groups
  
  distr$ctss_normality$time_test_visit <- radio$long %>% 
    mutate(log_ctss = log(ctss + 1), 
           log_perc_opac = log(perc_opac + 1), 
           log_perc_hiopac = log(perc_hiopac + 1)) %>% 
    explore(split_factor = 'visit', 
            variables = distr$responses, 
            what = 'normality') %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  distr$ctss_normality$time_plot_visit <- radio$long %>% 
    mutate(log_ctss = log(ctss + 1), 
           log_perc_opac = log(perc_opac + 1), 
           log_perc_hiopac = log(perc_hiopac + 1)) %>% 
    explore(split_factor = 'visit', 
            variables = distr$responses, 
            what = 'plots', 
            type = 'qq', 
            cust_theme = globals$common_theme) %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + labs(title = translate_var(.x$labels$title)))
  
  ## normality in the visit groups and COVID-19 severity
  
  distr$ctss_normality$time_test_visit_severity <- radio$long %>% 
    mutate(log_ctss = log(ctss + 1), 
           log_perc_opac = log(perc_opac + 1), 
           log_perc_hiopac = log(perc_hiopac + 1), 
           visit_severity = interaction(visit, severity)) %>% 
    explore(split_factor = 'visit_severity', 
            variables = distr$responses, 
            what = 'normality') %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  distr$ctss_normality$time_plot_visit_severity <- radio$long %>% 
    mutate(log_ctss = log(ctss + 1), 
           log_perc_opac = log(perc_opac + 1), 
           log_perc_hiopac = log(perc_hiopac + 1), 
           visit_severity = interaction(visit, severity)) %>% 
    explore(split_factor = 'visit_severity', 
            variables = distr$responses, 
            what = 'plots', 
            type = 'qq', 
            cust_theme = globals$common_theme) %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + labs(title = translate_var(.x$labels$title)))
  
# Variance homogeneity of the CTSS and automated perc opacity at the time points and in the severity groups -----
  
  insert_msg('Variance homogeneity of the CTSS')
  
  distr$homogeneity_visit <- radio$long %>% 
    mutate(log_ctss = log(ctss + 1), 
           log_perc_opac = log(perc_opac + 1), 
           log_perc_hiopac = log(perc_hiopac + 1)) %>% 
    compare_variables(variables = distr$responses, 
                      split_factor = 'visit', 
                      what = 'variance')
  
  distr$homogeneity_visit_severity <- radio$long %>% 
    mutate(log_ctss = log(ctss + 1), 
           log_perc_opac = log(perc_opac + 1), 
           log_perc_hiopac = log(perc_hiopac + 1), 
           visit_severity = interaction(visit, severity)) %>% 
    compare_variables(variables = distr$responses, 
                      split_factor = 'visit_severity', 
                      what = 'variance')
  
# END -----
  
  insert_tail()