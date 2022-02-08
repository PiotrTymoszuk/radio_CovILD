# This script 'models' changes of the CT score and percent opacity in time in the entire cohort and severity groups
# by Friedman test.

  insert_head()
  
# container list -----
  
  kinetic <- list()
  
# analysis table ----
  
  insert_msg('Analysis table')
  
  kinetic$analysis_tbl <- radio$long %>% 
    filter(ID %in% radio$complete_long_ids)
  
  kinetic$analysis_tbl <- kinetic$analysis_tbl %>% 
    dlply(.(severity), as_tibble) %>% 
    c(list(cohort = kinetic$analysis_tbl), .)
  
  kinetic$analysis_tbl <- kinetic$analysis_tbl %>% 
    map(select, ID, severity, visit, time_months, ctss, perc_opac)
  
# Friedman test -----
  
  insert_msg('Friedman test')
  
  plan('multisession')
  
  kinetic$friedman_test <- kinetic$analysis_tbl %>% 
    future_map(compare_variables, 
               variables = c('ctss', 'perc_opac'), 
               split_factor = 'visit', 
               what = 'test', 
               types = 'friedman_test', 
               ci = TRUE, 
               pub_styled = TRUE, 
               simplify_p = TRUE, 
               boot_method = 'bca', 
               .options = furrr_options(seed = TRUE)) %>% 
    map2_dfr(., names(.), ~mutate(.x, subset = .y)) %>%
    mutate(signficance = stri_replace(significance, fixed = '0.', replacement = '.'), 
           eff_size = stri_replace_all(eff_size, fixed = '0.', replacement = '.'), 
           plot_caption = paste(signficance, eff_size, sep = ', '), 
           plot_caption = stri_replace(plot_caption, regex = '\\[.*\\]', replacement = ''))
  
  plan('sequential')

# Post-hoc testing: Wilcoxon test with the r statistic as effect size -----
  # No multiple testing adjustment, as requested by the statistical reviewer
  
  insert_msg('Post-hoc testing')
  
  kinetic$post_hoc_test <- kinetic$analysis_tbl %>% 
    map(post_eff_size) %>% 
    map(mutate, 
        significance = stri_replace_all(significance, regex = 'ns|\\s|\\(|\\)', replacement = ''))

# Plotting of the time change plots ------
  
  insert_msg('Time change plots')

  kinetic$plots_ctss <- list(data = kinetic$analysis_tbl, 
                             plot_title = globals$subset_labels, 
                             plot_subtitle = kinetic$friedman_test %>% 
                               filter(variable == 'ctss') %>% 
                               .$plot_caption, 
                             fill_color = globals$subset_colors) %>% 
    pmap(plot_ctss, 
         plot_var = 'ctss', 
         y_lab = 'CTSS') %>% 
    map2(., kinetic$post_hoc_test, 
         add_ctss_post_p, 
         variable = 'ctss') %>% 
    map(~.x + expand_limits(y = 25))
  
  kinetic$plots_opacity <- list(data = kinetic$analysis_tbl, 
                                plot_title = globals$subset_labels, 
                                plot_subtitle = kinetic$friedman_test %>% 
                                  filter(variable == 'perc_opac') %>% 
                                  .$plot_caption, 
                                fill_color = globals$subset_colors) %>% 
    pmap(plot_ctss, 
         plot_var = 'perc_opac', 
         y_lab = 'Opacity, % lung') %>% 
    map2(., kinetic$post_hoc_test, 
         add_ctss_post_p, 
         variable = 'perc_opac', 
         line_y = 34, 
         voffset = 3, 
         text_offset = 0.7)

# effect size (r value) plots -------
  
  insert_msg('Plotting the effect sizes')
  
  kinetic$eff_size_plots <- list(variable = c('ctss' = 'ctss', 
                                              'perc_opac' = 'perc_opac'), 
                                 plot_title = c('CTSS', 'Lung opactity')) %>% 
    pmap(plot_effects, 
         data = kinetic$post_hoc_test %>% 
           map2_dfr(., names(.), ~mutate(.x, subset = .y)), 
         plot_subtitle = 'Effect size') %>% 
    map(~.x + scale_y_continuous(limits = c(0, 1)))

# END ----
  
  insert_tail()