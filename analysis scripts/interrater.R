# This script compares the performance of the manual CTSS and automated opacity detection at the consecutive time points

  insert_head()
  
# container list ----
  
  rater <- list()
  
# inter-rater comparison, any abnormalities, GGO, consolidations by CTSS and opacity ----
  
  insert_msg('Any abnormalities and opacity')
  
  rater[c('any_ct', 
          'ggo', 
          'consolidation')] <- list(d_var = c('ctss_any', 'ggo', 'consol'), 
                                    m_var = c('opacity', 'opacity', 'hiopacity'), 
                                    plot_title = c('Any CT abnormalities', 
                                                   'GGO', 
                                                   'Consolidation'), 
                                    plot_subtitle = c('Detection by automated opacity', 
                                                      'Detection by automated opacity', 
                                                      'Detection by automated high opacity')) %>% 
    pmap(interrater, 
         data = radio$long)
  
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