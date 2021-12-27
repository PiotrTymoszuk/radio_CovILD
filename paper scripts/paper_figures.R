# This script generates paper figures -----

  insert_head()

# container list -----

  paper_figures <- list()
  
# Figure 1: Consort plot - placeholder, provided by the Author team ----
  
  insert_msg('Figure 1: CONSORT, provided by the authors')
  
# Figure 2: kinetics of any CT abnormalities -----
  
  insert_msg('Figure 2: kinetic of any CT abnormalities and CTSS')
  
  paper_figures$ct_kinetic$upper_panel <- kinetic$binary$plots[c('cohort.ctss_any', 
                                                                 'mild.ctss_any', 
                                                                 'moderate.ctss_any', 
                                                                 'severe.ctss_any', 
                                                                 'critical.ctss_any')] %>% 
    map2(., globals$subset_labels, 
         ~.x + 
           labs(title = .y) + 
           scale_y_continuous(limits = c(0, 115), 
                              breaks = seq(0, 100, by = 25)) + 
           scale_x_continuous(limits = c(2, 13), 
                              breaks = c(2, 3, 6, 12))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv')
  
  paper_figures$ct_kinetic$bottom_panel <- kinetic$binary$plots[c('cohort.ctss_mod_severe', 
                                                                   'mild.ctss_mod_severe', 
                                                                   'moderate.ctss_mod_severe', 
                                                                   'severe.ctss_mod_severe', 
                                                                   'critical.ctss_mod_severe')] %>% 
    map2(., globals$subset_labels, 
         ~.x + 
           labs(title = .y) + 
           scale_y_continuous(limits = c(0, 115), 
                              breaks = seq(0, 100, by = 25)) + 
           scale_x_continuous(limits = c(2, 13), 
                              breaks = c(2, 3, 6, 12))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv')
  
  paper_figures$ct_kinetic <- plot_grid(paper_figures$ct_kinetic$upper_panel, 
                                        paper_figures$ct_kinetic$bottom_panel, 
                                        nrow = 2, 
                                        labels = LETTERS, 
                                        label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_2_kinetic_ct', 
                     w = 180, 
                     h = 210)
  
# Figure 3: kinetic of the CTSS -----
  
  insert_msg('Figure 3: kinetic of the CTSS')
  
  paper_figures$ctss_kinetic <- kinetic$ctss$plots[c('cohort',
                                                     'mild', 
                                                     'moderate', 
                                                     'severe', 
                                                     'critical')] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    as_figure_object(figure_label = 'figure_3_kinetic_ctss',
                     w = 180, 
                     h = 210)
  
# Figure 5: PILI modeling -----
  
  insert_msg('Figure 4: PILI modeling')
  
  paper_figures$pili_modeling <- plot_grid(risk$univariate_plots$pili_fup4, 
                                           risk$multivariate_plots$pili_fup4, 
                                           nrow = 2, 
                                           align = 'hv', 
                                           rel_heights = c(0.6, 0.4), 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_4_pili_modeling', 
                     w = 180, 
                     h = 210)
  
# Figure 6: CTSS modeling ------
  
  insert_msg('Figure 5: CTSS modeling')
  
  paper_figures$ctss_modeling <- plot_grid(risk$univariate_plots$ctss_fup4, 
                                           risk$multivariate_plots$ctss_fup4, 
                                           nrow = 2, 
                                           align = 'hv', 
                                           rel_heights = c(0.6, 0.4), 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_5_ctss_modeling', 
                     w = 180, 
                     h = 210)
  
# Saving the figures on the disc -----
  
  insert_msg('Saving the figures')
  
  paper_figures %>% 
    walk(save_figure_object, 
         format = 'pdf', 
         dev = cairo_pdf, 
         target_folder = './paper/figures')
    
  
  
  