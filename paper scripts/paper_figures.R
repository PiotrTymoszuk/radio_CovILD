# This script generates paper figures -----

  insert_head()

# container lists -----

  paper_figures <- list()
  suppl_figures <- list()
  rev_figures <- list() ## response to the statistical reviewer
  
# Figure 1: Consort plot - placeholder, provided by the Author team ----
  
  insert_msg('Figure 1: CONSORT, provided by the authors')
  
  paper_figures$consort <- ggdraw() %>% 
    as_figure(label = 'figure_1_consort_placeholder', 
              w = 180, 
              h = 180)
  
# Figure 2: CT images, provided by the Author team -----
  
  insert_msg('Figure 2: CT images, provided by the authors')
  
  paper_figures$ct_images <- ggdraw() %>% 
    as_figure(label = 'figure_2_ct_placeholder', 
              w = 180, 
              h = 180)
  
# Figure 3: modeling of the CT abnormality risk -----
  
  insert_msg('Figure 3: CT abnormality risk')

  paper_figures$abnormality_risk <-  plot_grid(risk$univariate$plots$ctss_any_fup4, 
                                               plot_grid(risk$multivariate$plots$ctss_any_fup4, 
                                                         ggdraw(), 
                                                         nrow = 2, 
                                                         rel_heights = c(0.6, 0.4)), 
                                               ncol = 2, 
                                               rel_widths = c(0.52, 0.48), 
                                               labels = LETTERS, 
                                               label_size = 10) %>% 
    as_figure(label = 'figure_3_abnormality_risk', 
              h = 180, 
              w = 180)
  
# Figure 4: CT placeholder -----
  
  insert_msg('Figure 4: CT placeholder')
  
  paper_figures$ct_images2 <- ggdraw() %>% 
    as_figure(label = 'figure_4_ct_placeholder2', 
              w = 180, 
              h = 180)
  
# Figure 5: CTSS change in time ------
  
  insert_msg('Figure 5: CT change in time')
  
  paper_figures$ctss_change <- kinetic$plots_ctss %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              ncol = 2) %>% 
    as_figure(label = 'figure_5_ctss_change', 
              w = 180, 
              h = 210)
  
# Figure E1: CTSS severity at 1-year FUP -----
  
  insert_msg('Figure E1: CTSS severity class at 1 year')
  
  suppl_figures$ctss_severity <-  plot_grid(score$univariate$plots$ctss_class_fup4, 
                                            plot_grid(score$multivariate$plots$ctss_class_fup4, 
                                                      ggdraw(), 
                                                      nrow = 2, 
                                                      rel_heights = c(0.6, 0.4)), 
                                            ncol = 2, 
                                            rel_widths = c(0.52, 0.48), 
                                            labels = LETTERS, 
                                            label_size = 10) %>% 
    as_figure(label = 'figure_E1_CTSS_severity', 
              h = 180, 
              w = 180)
  
# Figure E2: CTSS and automated opacity determination -----
  
  insert_msg('Figure E2: CTSS and opacity correlation')
  
  suppl_figures$ctss_correlation <- rater$ctss$plots %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    as_figure(label = 'figure_E2_CTSS_correlations', 
              h = 180, 
              w = 180)
  
# Figure R1: Splining of the numeric variables -----
  
  insert_msg('Figure R1: splining')
  
  rev_figures$age_spline <- c(risk_gam$comparison$gam_models$ctss_any_fup4, 
                              risk_gam$comparison$cutoff_models$ctss_any_fup4) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(plot_grid(risk_gam$univariate$pred_plots$ctss_any_fup4.age.age + 
                          geom_vline(xintercept = 60, linetype = 'dashed'), 
                        ggdraw(), 
                        ncol = 2), 
              ., 
              nrow = 2, 
              rel_heights = c(0.9, 2.1), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_R1_age_spline',
              w = 180, 
              h = 210)
  
# Figure R2: Poisson and logistic ordinal regression results ------
  
  insert_msg('Figure R2: Poisson and logistic regression results')
  
  rev_figures$pois_ord_comp <- plot_grid(score$multivariate$plots$ctss_fup4, 
                                         score$multivariate$plots$ctss_class_fup4, 
                                         ncol = 2, 
                                         align = 'hv') %>% 
    plot_grid(plot_grid(score$cv$diagnostic_plots$ctss_class_fup4$train + 
                          labs(title = 'CTSS score class: confusion matrix', 
                               tag = paste0('\n', score$cv$diagnostic_plots$ctss_class_fup4$train$labels$tag)) + 
                          guides(fill = FALSE) + 
                          scale_x_discrete(labels = c('0', '1-5', '6-10', '11-25'), 
                                           name = 'True CTSS class') + 
                          scale_y_discrete(labels = c('0', '1-5', '6-10', '11-25'), 
                                           name = 'Predicted CTSS class'), 
                        ggdraw(), 
                        ncol = 2, 
                        rel_widths = c(0.6, 0.4)), 
              nrow = 2, 
              rel_heights = c(0.6, 0.4), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_R2_mod_techniques', 
              w = 180, 
              h = 180)

# Saving the figures on the disc -----
  
  insert_msg('Saving the figures')
  
  paper_figures %>% 
    walk(save_figure, 
         format = 'pdf', 
         device = cairo_pdf, 
         path = './paper/figures')
  
  paper_figures %>% 
    walk(save_figure, 
         format = 'eps', 
         device = cairo_ps, 
         path = './paper/figures')
  
  suppl_figures %>% 
    walk(save_figure, 
         format = 'pdf', 
         device = cairo_pdf, 
         path = './paper/supplementary figures')
  
  suppl_figures %>% 
    walk(save_figure, 
         format = 'eps', 
         device = cairo_ps, 
         path = './paper/supplementary figures')
  
  rev_figures %>% 
    walk(save_figure, 
         format = 'pdf', 
         device = cairo_pdf, 
         path = './paper/reviewer figures')
  
# END ----
  
  insert_tail()