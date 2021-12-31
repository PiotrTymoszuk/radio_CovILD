# This script 'models' kinetic of the CT score in the entire cohort and severity groups
# by Friedman test.
# Risk of incomplete resolution of any, moderate-to-severe, severe and particular type abnormalities
# is estimated by mixed-effect logistic modeling. Only the participants with the complete longitudinal CT scan sets are analyzed

  insert_head()
  
# container list -----
  
  kinetic <- list()
  
# analysis table ----
  
  kinetic$analysis_tbl <- radio$long %>% 
    filter(ID %in% radio$complete_long_ids)
  
  kinetic$analysis_tbl <- kinetic$analysis_tbl %>% 
    dlply(.(severity)) %>% 
    c(list(cohort = kinetic$analysis_tbl), .)
  
# CTSS, opacity and hig opacity percentage kinetics -----
  
  insert_msg('CTSS kinetic')
  
  ## test
  
  kinetic$numeric$analysis <- list(ctss = ctss ~ visit|ID, 
                                   perc_opac = perc_opac ~ visit|ID, 
                                   perc_hiopac = perc_hiopac ~ visit|ID) %>% 
    map(function(outcome) kinetic$analysis_tbl %>% 
          map(friedman_test, 
                  formula = outcome) %>% 
          map2_dfr(., names(.), ~mutate(.x, subset = .y)))

  ## summary
  
  kinetic$numeric$summary <- kinetic$numeric$analysis %>% 
    map(mutate, 
        p_value = p, 
        p_adj = p.adjust(p_value, 'BH'), 
        plot_caption = paste0('\u03C7 = ', signif(statistic, 2), 
                              ', df = 3, ', format_p(p_value, 2)))
  
  ## effect size determined by Kendall W, updating the plot_caption
  
  plan('multisession')
  
  kinetic$numeric$effsize <- list(ctss = ctss ~ visit|ID, 
                                  perc_opac = perc_opac ~ visit|ID, 
                                  perc_hiopac = perc_hiopac ~ visit|ID) %>% 
    map(function(outcome) kinetic$analysis_tbl %>% 
          future_map(friedman_effsize, 
                     formula = outcome, 
                     ci = TRUE, 
                     ci.type = 'norm', 
                     .options = furrr_options(seed = TRUE)) %>% 
          map2_dfr(., names(.), ~mutate(.x, subset = .y)))
  
  kinetic$numeric$summary <- map2(kinetic$numeric$summary, 
                                  kinetic$numeric$effsize, 
                                  ~mutate(.x, 
                                          w = paste0(signif(.y$effsize, 2), 
                                                     ' [', signif(.y$conf.low, 2), 
                                                     ' - ', signif(.y$conf.high, 2), ']'), 
                                          plot_caption = paste(plot_caption, w, sep = '\nW = ')))

  plan('sequential')
  
  ## post-hoc Wilcoxon tests
  
  kinetic$numeric$post_hoc <- list(ctss = ctss ~ visit, 
                                   perc_opac = perc_opac ~ visit, 
                                   perc_hiopac = perc_hiopac ~ visit) %>% 
    map(function(outcome) kinetic$analysis_tbl %>% 
          future_map(compare_means, 
                     formula = outcome,  
                     paired = TRUE, 
                     method = 'wilcox.test', 
                     p.adjust.method = 'holm'))
  
  ## post-hoc effect size
  
  kinetic$numeric$post_hoc_eff_size <- c('ctss', 'perc_opac', 'perc_hiopac') %>% 
    map(function(outcome) kinetic$analysis_tbl %>% 
          map(compare_effects, 
              response = outcome, 
              indep_var = 'visit', 
              group_var = 'ID', 
              invert = TRUE) %>% 
          map2_dfr(., names(.), function(data, sub) mutate(data, severity = sub))) %>% 
    set_names(c('ctss', 'perc_opac', 'perc_hiopac'))
  
  ## score/opacity plots
  
  kinetic$numeric$plots_ctss <- list(data = kinetic$analysis_tbl, 
                                     plot_title = globals$subset_labels, 
                                     plot_subtitle = kinetic$numeric$summary$ctss$plot_caption, 
                                     fill_color = globals$subset_colors) %>% 
    pmap(plot_ctss) %>% 
    map2(., kinetic$numeric$post_hoc$ctss, 
         add_ctss_post_p)
  
  kinetic$numeric$plots_opacity <- list(data = kinetic$analysis_tbl, 
                                        plot_title = globals$subset_labels, 
                                        plot_subtitle = kinetic$numeric$summary$perc_opac$plot_caption, 
                                        fill_color = globals$subset_colors) %>% 
    pmap(plot_ctss, 
         plot_var = 'perc_opac', 
         y_lab = 'Opacity, % lung') %>% 
    map2(., kinetic$numeric$post_hoc$perc_opac, 
         add_ctss_post_p, 
         line_y = 34, 
         voffset = 3, 
         text_offset = 0.7)
  
  kinetic$numeric$plots_hiopacity <- list(data = kinetic$analysis_tbl, 
                                          plot_title = globals$subset_labels, 
                                          plot_subtitle = kinetic$numeric$summary$perc_hiopac$plot_caption, 
                                          fill_color = globals$subset_colors) %>% 
    pmap(plot_ctss, 
         plot_var = 'perc_hiopac', 
         y_lab = 'High opacity, % lung') %>% 
    map2(., kinetic$numeric$post_hoc$perc_hiopac, 
         add_ctss_post_p, 
         line_y = 2.1, 
         voffset = 0.4, 
         text_offset = 0.05)
  
  ## effect size plots with the tau statistic
  
  kinetic$numeric$eff_plots <- list(eff_size_data = kinetic$numeric$post_hoc_eff_size, 
                                    plot_title = c('CTSS', 'Opacity', 'High opacity')) %>% 
    pmap(plot_effects, 
         eff_var = 'tau_b',
         ci = FALSE, 
         y_lab = expression('Recovery effect size, '*tau))

# Binary CT features -----
  
  insert_msg('Modeling binary CT features')
  
  kinetic$binary$variables <- c('ctss_any', 
                                'ctss_mod_severe', 
                                'ctss_severe', 
                                'ggo', 
                                'retic', 
                                'consol', 
                                'bronch', 
                                'opacity', 
                                'hiopacity')
  
  kinetic$binary$formulas <- paste(kinetic$binary$variables, '~ time_months + (1|ID)') %>% 
    map(as.formula) %>% 
    set_names(kinetic$binary$variables)
  
  kinetic$binary$null_formulas <- paste(kinetic$binary$variables, '~ (1|ID)') %>% 
    map(as.formula) %>% 
    set_names(kinetic$binary$variables)
  
  ## full and null models
  
  kinetic$binary$full_models <- kinetic$analysis_tbl %>% 
    map(function(subset) kinetic$binary$formulas %>% 
          map(safely(glmer), data = subset, family = 'binomial') %>% 
          map(~.x$result)) %>% 
    unlist(recursive = FALSE)
  
  kinetic$binary$null_models <- kinetic$analysis_tbl %>% 
    map(function(subset) kinetic$binary$null_formulas %>% 
          map(safely(glmer), data = subset, family = 'binomial') %>% 
          map(~.x$result)) %>% 
    unlist(recursive = FALSE)
  
  ## LRT
  
  kinetic$binary$lrt <- map2(kinetic$binary$full_models, 
                             kinetic$binary$null_models, 
                             safely(anova)) %>% 
    map(~.x$result)
  
  kinetic$binary$lrt_summary <- kinetic$binary$lrt %>% 
    map(unclass) %>% 
    map(~safely(tibble)(chisq = .x[['Chisq']][2], 
                        df = .x[['Df']][2], 
                        p_value = .x[['Pr(>Chisq)']][2])) %>% 
    map(~safely(mutate)(.x$result, plot_caption = paste0('LRT: \u03C7 = ', signif(chisq, 2), 
                                                          ', df = 1, ', format_p(p_value, 2)))) %>% 
    map(~.x$result)
  
  kinetic$binary$lrt_captions <- kinetic$binary$lrt_summary %>% 
    map(~.x$plot_caption)
  
  ## plotting
  
  kinetic$binary$plots <- list(dataset = kinetic$analysis_tbl, 
                               prefix = globals$subset_labels, 
                               fill_color = globals$subset_colors) %>% 
    pmap(function(dataset, prefix, fill_color) kinetic$binary$variables %>% 
           map(~plot_feature_kinetic(data = dataset, 
                                     plot_var = .x, 
                                     plot_title = paste(prefix, translate_var(.x), sep = ': '), 
                                     fill_color = fill_color)) %>% 
           set_names(kinetic$binary$variables)) %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + scale_y_continuous(limits = c(0, 100))) %>% 
    map2(., kinetic$binary$lrt_captions, ~.x + labs(subtitle = .y))
  
  ## post-hoc effect size

  kinetic$binary$post_hoc_eff_size <- kinetic$binary$variables %>% 
    map(function(outcome) kinetic$analysis_tbl %>% 
                 map(compare_effects, 
                     response = outcome, 
                     indep_var = 'visit', 
                     group_var = 'ID', 
                     invert = TRUE) %>% 
                 map2_dfr(., names(.), function(data, sub) mutate(data, severity = sub))) %>% 
    set_names(kinetic$binary$variables)
  
  ## effect plots with the tau
  
  kinetic$binary$eff_plots <- list(eff_size_data = kinetic$binary$post_hoc_eff_size, 
                                    plot_title = translate_var(kinetic$binary$variables)) %>% 
    pmap(plot_effects, 
         eff_var = 'tau_b', 
         ci = FALSE, 
         y_lab = expression('Recovery effect size, '*tau))

# END ----
  
  insert_tail()