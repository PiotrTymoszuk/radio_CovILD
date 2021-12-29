# This script generates paper figures -----

  insert_head()

# container lists -----

  paper_figures <- list()
  suppl_figures <- list()
  
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
  
  paper_figures$ctss_kinetic <- kinetic$numeric$plots_ctss[c('cohort',
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
  
# Figure 4: Any CT abnormality modeling -----
  
  insert_msg('Figure 4: Any CT abnormality modeling')
  
  paper_figures$pili_modeling <- plot_grid(risk$univariate_plots$ctss_any_fup4, 
                                           risk$multivariate_plots$ctss_any_fup4, 
                                           nrow = 2, 
                                           align = 'hv', 
                                           rel_heights = c(0.63, 0.37), 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_4_pili_modeling', 
                     w = 180, 
                     h = 210)
  
# Figure 5: CTSS modeling ------
  
  insert_msg('Figure 5: CTSS modeling')
  
  paper_figures$ctss_modeling <- plot_grid(risk$univariate_plots$ctss_fup4, 
                                           risk$multivariate_plots$ctss_fup4, 
                                           nrow = 2, 
                                           align = 'hv', 
                                           rel_heights = c(0.64, 0.36), 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_5_ctss_modeling', 
                     w = 180, 
                     h = 210)
  
# Supplementary Figure S1: abnormalities detected by opacity -----
  
  insert_msg('Figures S1: opacity and detection of abnormalities')

  suppl_figures$ct_opacity <- kinetic$binary$plots[c('cohort.opacity', 
                                                     'mild.opacity', 
                                                     'moderate.opacity', 
                                                     'severe.opacity', 
                                                     'critical.opacity')] %>% 
    map2(., globals$subset_labels, 
         ~.x + 
           labs(title = .y) + 
           scale_y_continuous(limits = c(0, 115), 
                              breaks = seq(0, 100, by = 25)) + 
           scale_x_continuous(limits = c(2, 13), 
                              breaks = c(2, 3, 6, 12))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv') %>% 
    as_figure_object(figure_label = 'figure_s1_any_opacity', 
                     w = 180, 
                     h = 140)
  
# Supplementary Figure S2: opacity kinetics ----
  
  insert_msg('Figure S2: kinetics of the opacity')
  
  suppl_figures$opacity_kinetic <- kinetic$numeric$plots_opacity[c('cohort',
                                                                   'mild', 
                                                                   'moderate', 
                                                                   'severe', 
                                                                   'critical')] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    as_figure_object(figure_label = 'figure_s2_kinetic_opacity',
                     w = 180, 
                     h = 210)
  
# Supplementary Figure S3: risk modeling of any opacity ------
  
  insert_msg('Figure S3: risk modeling of any opacity')
  
  suppl_figures$opacity_risk <- plot_grid(risk$univariate_plots$opacity_fup4, 
                                           risk$multivariate_plots$opacity_fup4, 
                                           nrow = 2, 
                                           align = 'hv', 
                                           rel_heights = c(0.63, 0.37), 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s3_opacity_risk_modeling', 
                     w = 180, 
                     h = 220)
  
# Supplementary Figure S4: inter-rater performance, detection of abnormalities by CTSS and opacity ----
  
  insert_msg('Figure S4: detection of any abnormalities by CTSS and opacity')
  
  suppl_figures$any_abnormality_rater <- plot_grid(rater$any_ct$roc_plot + 
                                                     theme(plot.tag = element_blank(), 
                                                           legend.position = 'none'), 
                                                   rater$any_ct$kappa_plot + 
                                                     theme(legend.position = 'none') + 
                                                     scale_x_continuous(limits = c(0, 1.1), 
                                                                        breaks = seq(0, 1, by = 0.25)), 
                                                   ncol = 2, 
                                                   rel_widths = c(1.1, 0.9), 
                                                   labels = LETTERS, 
                                                   label_size = 10) %>% 
    plot_grid(plot_grid(get_legend(rater$any_ct$roc_plot), 
                        ggdraw() + 
                          draw_text(rater$any_ct$roc_plot$labels$tag, 
                                    size = 8, 
                                    x = 0.1, 
                                    hjust = 0), 
                        ncol = 2), 
              nrow = 2, 
              rel_heights = c(0.7, 0.3)) %>% 
    as_figure_object(figure_label = 'figure_s4_rater',
                     w = 180, 
                     h = 140)
  
# Figure S5: correlation of CTSS and opacity -----
  
  insert_msg('Figure S5: CTSS and opacity')
  
  suppl_figures$ctss_opacity <- rater$ctss$plots %>% 
    plot_grid(plotlist = ., 
              ncol = 2) %>% 
    as_figure_object(figure_label = 'figure_s5_ctss_opacity_correlations',
                     w = 180, 
                     h = 180)
    
# Saving the figures on the disc -----
  
  insert_msg('Saving the figures')
  
  paper_figures %>% 
    walk(save_figure_object, 
         format = 'pdf', 
         dev = cairo_pdf, 
         target_folder = './paper/figures')
  
  suppl_figures %>% 
    walk(save_figure_object, 
         format = 'pdf', 
         dev = cairo_pdf, 
         target_folder = './paper/supplementary figures')
    
  
# END ----
  
  insert_tail()