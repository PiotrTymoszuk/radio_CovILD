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
  
# CTSS kinetics -----
  
  insert_msg('CTSS kinetic')
  
  ## test
  
  kinetic$ctss$analysis <- kinetic$analysis_tbl %>% 
    map(~friedman_test(ctss ~ visit|ID, data = .x))
  
  ## summary
  
  kinetic$ctss$summary <- kinetic$ctss$analysis %>% 
    map2_dfr(., names(.), ~mutate(.x, subset = .y, p_value = p)) %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'), 
           plot_caption = paste0('\u03C7 = ', signif(statistic, 2), 
                                 ', df = 3, p = ', signif(p_value, 2)))
  
  ## effect size determined by Kendall W, updating the plot_caption
  
  plan('multisession')
  
  kinetic$ctss$effsize <- kinetic$analysis_tbl %>% 
    future_map(~friedman_effsize(ctss ~ visit|ID, 
                                 data = .x, 
                                 ci = TRUE, 
                                 ci.type = 'bca'), 
               .options = furrr_options(seed = TRUE)) %>% 
    map2_dfr(., names(.), ~mutate(.x, subset = .y))
  
  kinetic$ctss$summary <- kinetic$ctss$summary %>% 
    mutate(plot_caption = list(x = plot_caption, 
                               y = kinetic$ctss$effsize$effsize, 
                               lo = kinetic$ctss$effsize$conf.low, 
                               hi = kinetic$ctss$effsize$conf.high) %>% 
             pmap_chr(function(x, y, lo, hi) paste0(x, ', W = ', signif(y, 2), ' [', signif(lo, 2), ' - ', signif(hi, 2), ']')))

  plan('sequential')
  
  ## post-hoc tests
  
  kinetic$ctss$post_hoc <- kinetic$analysis_tbl %>% 
    future_map(~compare_means(ctss ~ visit, data = .x, 
                              paired = TRUE, 
                              method = 'wilcox.test', 
                              p.adjust.method = 'holm'))
  
  ## plots
  
  kinetic$ctss$plots <- list(data = kinetic$analysis_tbl, 
                             plot_title = globals$subset_labels, 
                             plot_subtitle = kinetic$ctss$summary$plot_caption, 
                             fill_color = globals$subset_colors) %>% 
    pmap(plot_ctss) %>% 
    map2(., kinetic$ctss$post_hoc, add_ctss_post_p)
  
# Binary CT features -----
  
  insert_msg('Modeling binary CT features')
  
  kinetic$binary$variables <- c('ctss_any', 
                                'ctss_mod_severe', 
                                'ctss_severe', 
                                'ggo', 
                                'retic', 
                                'consol', 
                                'bronch')
  
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
                                                          ', df = 1, p = ', signif(p_value, 2)))) %>% 
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
  
# END ----
  
  insert_tail()