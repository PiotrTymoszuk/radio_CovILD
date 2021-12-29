# This script compares the performance of the manual CTSS and automated opacity detection at the consecutive time points

  insert_head()
  
# container list ----
  
  rater <- list()

# inter-rater comparison, any abnormalities by CTSS and opacity ----
  
  insert_msg('Any abnormalities')

  ## table objects
    
  rater$any_ct$tables <- radio$long %>% 
    rater_tbl(ctss_any, opacity) %>% 
    map(~table(.x[c('ctss_any', 'opacity')]))
  
  ## n numbers
  
  rater$any_ct$n_numbers <- map2(map(rater$any_ct$tables, sum), 
                                 map(rater$any_ct$tables, ~sum(.x[2, ])), 
                                 ~paste0('total: n = ', .x, ', events: n = ', .y))
  
  rater$any_ct$n_tag <- map2_chr(globals$visit_labels, 
                                 rater$any_ct$n_numbers, 
                                 paste, sep = '\n')
  
  ## Kappa calculation
  
  rater$any_ct$kappa <- rater$any_ct$tables %>% 
    map(Kappa_pipe) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y)) %>% 
    mutate(visit = factor(visit, levels = rev(visit)))
  
  ## ROC
  
  rater$any_ct$cutpoint <- radio$long %>% 
    rater_tbl(ctss_any, opacity) %>% 
    map(~map_dfc(.x, ~as.numeric(.x) - 1)) %>% 
    map(find_optimal_cutoff, 
        status_variable = 'ctss_any', 
        marker = 'opacity')
  
  rater$any_ct$roc_stats <- rater$any_ct$cutpoint %>% 
    map(get_optimal_cutpoint_coefs) %>% 
    map(unlist, recursive = FALSE) %>% 
    map(~.x[2:3]) %>%
    as_tibble %>% 
    t %>% 
    as.data.frame %>% 
    set_names(c('Se', 'Sp')) %>% 
    rownames_to_column('visit')
  
  rater$any_ct$roc_stats <- rater$any_ct$cutpoint %>% 
    map_dfr(get_auc) %>% 
    cbind(rater$any_ct$roc_stats, .) %>% 
    as_tibble %>% 
    mutate(plot_annotation = paste0('Se = ', signif(Se, 2), 
                                    ', Sp = ', signif(Sp, 2), 
                                    ', AUC = ', signif(AUC, 2), 
                                    ' [', signif(lowerCI, 2), 
                                    ' - ', signif(upperCI, 2), ']'), 
           y_annotation = c(0.22, 0.16, 0.1, 0.04), 
           x_annotation = 0.26, 
           ctss_any = 'yes', 
           opacity = 'yes')
  
  
  ## kappa and ROC plots
  
  rater$any_ct$kappa_plot <- plot_kappa(data = rater$any_ct$kappa, 
                                        plot_title = 'Any CT abnormalities', 
                                        plot_subtitle = 'CTSS versus automated opacity') + 
    scale_x_continuous(limits = c(0, 1))
  
  rater$any_ct$roc_plot <- plot_roc(data = radio$long, 
                                    d_var = 'ctss_any', 
                                    m_var = 'opacity', 
                                    roc_stats = rater$any_ct$roc_stats, 
                                    plot_title = 'Any CT abnormalities', 
                                    plot_subtitle = 'CTSS versus automated opacity', 
                                    plot_tag = paste(rater$any_ct$n_tag, collapse = '\n'))
  
# inter-rater comparison, consolidations and high opacity ----
  
  insert_msg('Consolidations')
  
  ## table objects
  
  rater$consolidation$tables <- radio$long %>% 
    rater_tbl(consol, hiopacity) %>% 
    map(~table(.x[c('consol', 'hiopacity')]))
  
  ## n numbers
  
  rater$consolidation$n_numbers <- map2(map(rater$consolidation$tables, sum), 
                                        map(rater$consolidation$tables, ~sum(.x[2, ])), 
                                        ~paste0('total: n = ', .x, ', events: n = ', .y))
  
  rater$consolidation$n_tag <- map2_chr(globals$visit_labels, 
                                        rater$consolidation$n_numbers, 
                                        paste, sep = '\n')
  
  ## Kappa calculation
  
  rater$consolidation$kappa <- rater$consolidation$tables %>% 
    map(Kappa_pipe) %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y)) %>% 
    mutate(visit = factor(visit, levels = rev(visit)))
  
  ## ROC
  
  rater$consolidation$cutpoint <- radio$long %>% 
    rater_tbl(consol, hiopacity) %>% 
    map(~map_dfc(.x, ~as.numeric(.x) - 1)) %>% 
    map(find_optimal_cutoff, 
        status_variable = 'consol', 
        marker = 'hiopacity')
  
  rater$consolidation$roc_stats <- rater$consolidation$cutpoint %>% 
    map(get_optimal_cutpoint_coefs) %>% 
    map(unlist, recursive = FALSE) %>% 
    map(~.x[2:3]) %>%
    as_tibble %>% 
    t %>% 
    as.data.frame %>% 
    set_names(c('Se', 'Sp')) %>% 
    rownames_to_column('visit')
  
  rater$consolidation$roc_stats <- rater$consolidation$cutpoint %>% 
    map_dfr(get_auc) %>% 
    cbind(rater$consolidation$roc_stats, .) %>% 
    as_tibble %>% 
    mutate(plot_annotation = paste0('Se = ', signif(Se, 2), 
                                    ', Sp = ', signif(Sp, 2), 
                                    ', AUC = ', signif(AUC, 2), 
                                    ' [', signif(lowerCI, 2), 
                                    ' - ', signif(upperCI, 2), ']'), 
           y_annotation = c(0.4, 0.3, 0.2, 0.1), 
           x_annotation = 0.45, 
           consol = 'yes', 
           hiopacity = 'yes')
  
  ## kappa and ROC plots
  
  rater$consolidation$kappa_plot <- plot_kappa(data = rater$consolidation$kappa, 
                                               plot_title = 'Consolidaitons', 
                                               plot_subtitle = 'Manual versus automated high opacity') + 
    scale_x_continuous(limits = c(0, 1))
  
  rater$consolidation$roc_plot <- plot_roc(data = radio$long, 
                                           d_var = 'consol', 
                                           m_var = 'hiopacity', 
                                           roc_stats = rater$consolidation$roc_stats, 
                                           plot_title = 'Consolidaitons', 
                                           plot_subtitle = 'Manual versus automated high opacity', 
                                           plot_tag = paste(rater$consolidation$n_tag, collapse = '\n'))
  
# correlations CTSS vs opacity -----
  
  insert_msg('Correlations CTSS versus opacity')
  
  ## analyses
  
  rater$ctss$analyses <- radio$long %>% 
    rater_tbl(ctss, perc_opac) %>% 
    map(cor_test, 
        ctss, 
        perc_opac, 
        method = 'spearman') %>% 
    map2_dfr(., names(.), ~mutate(.x, visit = .y)) %>% 
    mutate(n_complete = radio$long %>% 
             rater_tbl(ctss, perc_opac) %>% 
             map(nrow), 
           plot_lab = paste0('r = ', signif(cor, 2), ', ', format_p(p), ', n = ', n_complete))

  ## plots
  
  rater$ctss$plots <- list(data = radio$long %>% 
                             rater_tbl(ctss, perc_opac), 
                           plot_title = globals$visit_labels, 
                           fill_color = globals$visit_colors, 
                           plot_subtitle = rater$ctss$analyses$plot_lab) %>% 
    pmap(correlation_plot, 
         jitter_w = 0.1) %>% 
    map(~.x + scale_y_continuous(trans = 'pseudo_log'))
  
# END -----
  
  insert_tail()