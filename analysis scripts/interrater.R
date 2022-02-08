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
  
  ## analysis table
  
  rater$ctss$analysis_tbl <- radio$long %>% 
    rater_tbl(ctss, perc_opac)
  
  ## analyses
  
  rater$ctss$analyses <- correlate_variables(!!!rater$ctss$analysis_tbl, 
                                             variables = c('ctss', 'perc_opac'), 
                                             type = 'spearman', 
                                             boot_method = 'bca', 
                                             pub_styled = TRUE, 
                                             simplify_p = TRUE) %>% 
    mutate(eff_size = stri_replace(eff_size, fixed = 'rho', replacement = '\u03C1'),
           eff_size = stri_replace_all(eff_size, fixed = '0.', replacement = '.'), 
           significance = stri_replace(significance, fixed = '0.', replacement = '.'), 
           plot_caption = paste0(eff_size, ', ', significance, ', n = ', n, sep = ', '), 
           plot_caption = stri_replace(plot_caption, regex = '\\[.*\\]', replacement = ''))


  ## plots
  
  rater$ctss$plots <- list(data = rater$ctss$analysis_tbl, 
                           plot_title = globals$visit_labels, 
                           point_color = globals$visit_colors, 
                           plot_subtitle = rater$ctss$analyses$plot_caption) %>% 
    pmap(plot_correlation, 
         variables = c('ctss', 'perc_opac'), 
         type = 'correlation', 
         cust_theme = globals$common_theme) %>% 
    map(~.x + scale_y_continuous(trans = 'pseudo_log'))

# END -----
  
  insert_tail()