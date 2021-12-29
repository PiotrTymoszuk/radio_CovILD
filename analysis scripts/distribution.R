# This script performs distribution checks of the numeric outcome variables

  insert_head()
  
# container list -----
  
  distr <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  distr$responses <- c('ctss', 'perc_opac')
  
# Normality of the CT severity score, percent opacity and percent high opacity at the time points -----
  
  insert_msg('CT severity score normality check')
  
  ## normality in the visit groups
  
  distr$ctss_normality$time_test_ident <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~shapiro_test(.x, ctss, perc_opac)) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))

  distr$ctss_normality$time_plot_ident$ctss <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~ggplot(.x, aes(sample = ctss)) + 
          geom_qq() + 
          geom_qq_line())
  
  distr$ctss_normality$time_plot_ident$perc_opac <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~ggplot(.x, aes(sample = perc_opac)) + 
          geom_qq() + 
          geom_qq_line())
  
  ## normality in the time and time:severity groups
  
  distr$ctss_normality$time_sev_test_ident <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~shapiro_test(.x, ctss, perc_opac)) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  distr$ctss_normality$time_sev_plot_ident$ctss <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~ggplot(.x, aes(sample = ctss)) + 
          geom_qq() + 
          geom_qq_line())
  
  distr$ctss_normality$time_sev_plot_ident$perc_opac <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~ggplot(.x, aes(sample = perc_opac)) + 
          geom_qq() + 
          geom_qq_line())
  
  ## normality in the visit groups
  
  distr$ctss_normality$time_test_log <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(mutate, 
        log_ctss = log(ctss + 1), 
        log_perc_opac = log(perc_opac + 1)) %>% 
    map(~shapiro_test(.x, log_ctss, log_perc_opac)) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  distr$ctss_normality$time_plot_log$ctss <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~ggplot(.x, aes(sample = log(ctss + 1))) + 
          geom_qq() + 
          geom_qq_line())
  
  distr$ctss_normality$time_plot_log$perc_opac <- radio$long %>% 
    dlply(.(visit)) %>% 
    map(~ggplot(.x, aes(sample = log(perc_opac + 1))) + 
          geom_qq() + 
          geom_qq_line())
  
  ## normality of the log CTSS in the time and time:severity groups
  
  distr$ctss_normality$time_sev_test_log <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(mutate, 
        log_ctss = log(ctss + 1), 
        log_perc_opac = log(perc_opac + 1)) %>% 
    map(~shapiro_test(.x, log_ctss, log_perc_opac)) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y))
  
  distr$ctss_normality$time_sev_plot_log$ctss <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~ggplot(.x, aes(sample = log(ctss + 1))) + 
          geom_qq() + 
          geom_qq_line())
  
  distr$ctss_normality$time_sev_plot_log$perc_opac <- radio$long %>% 
    dlply(.(visit, severity)) %>% 
    map(~ggplot(.x, aes(sample = log(perc_opac + 1))) + 
          geom_qq() + 
          geom_qq_line())
  
# Variance homogeneity of the CTSS and automated perc opacity at the time points and in the severity groups -----
  
  insert_msg('Variance homogeneity of the CTSS')
  
  ## CTSS
  
  distr$ctss_homogeneity <- list(ctss ~ visit, 
                               ctss ~ severity, 
                               log(ctss + 1) ~ visit, 
                               log(ctss + 1) ~ severity) %>% 
    map(levene_test, 
        data = radio$long) %>% 
    map2_dfr(., c('visit', 'severity', 'visit', 'severity'), ~mutate(.x, grouping = .y)) %>% 
    mutate(transformation = c('identity', 'identity', 'log', 'log'))
  
  ## percent opacity
  
  distr$perc_opac_homogeneity <- list(perc_opac ~ visit, 
                                    perc_opac ~ severity, 
                                    log(perc_opac + 1) ~ visit, 
                                    log(perc_opac + 1) ~ severity) %>% 
    map(levene_test, 
        data = radio$long) %>% 
    map2_dfr(., c('visit', 'severity', 'visit', 'severity'), ~mutate(.x, grouping = .y)) %>% 
    mutate(transformation = c('identity', 'identity', 'log', 'log'))
  
  ## percent high opacity
  
  distr$perc_hiopac_homogeneity <- list(perc_hiopac ~ visit, 
                                      perc_hiopac ~ severity, 
                                      log(perc_hiopac + 1) ~ visit, 
                                      log(perc_hiopac + 1) ~ severity) %>% 
    map(levene_test, 
        data = radio$long) %>% 
    map2_dfr(., c('visit', 'severity', 'visit', 'severity'), ~mutate(.x, grouping = .y)) %>% 
    mutate(transformation = c('identity', 'identity', 'log', 'log'))
  
# END -----
  
  insert_tail()