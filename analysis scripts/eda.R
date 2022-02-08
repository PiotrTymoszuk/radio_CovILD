# This script performs explorative data analysis: baseline characteristic
# and frequency of CT findings at the particular visits. Finally, distribution
# statistics of the CT severity score are calculated

  insert_head()

# container list and globals ------

  ed_analysis <- list()
  
  ed_analysis$variables <-  c('age', 'age_60', 'bmi_class', 'pky', 'pky_class', 'smoking')
  
  ed_analysis$test_types <- c('anova', 'chisq_test', 'chisq_test', 'kruskal_test', 'chisq_test', 'chisq_test')
  
# distribution of the age and pky variable in the severity groups ------
  
  insert_msg('Distributon and variance homogeneity for age')
  
  ed_analysis$age$normality <- explore(data = radio$clear, 
                                       variables = c('age', 'pky'), 
                                       split_factor = 'severity', 
                                       what = 'normality') %>% 
    map2_dfr(., names(.), ~mutate(.x, severity = .y))
  
  ed_analysis$age$homogeneity <- compare_variables(radio$clear, 
                                                   variables = c('age', 'pky'), 
                                                   split_factor = 'severity', 
                                                   what = 'variance')

# baseline clinical characteristic -----

  insert_msg('Baseline clinics, entire cohort')

  ed_analysis$baseline <- explore(data = radio$clear, 
                                  variables = c(ed_analysis$variables, 'severity'), 
                                  what = 'table', 
                                  pub_styled = TRUE, 
                                  simplify_p = TRUE)
  
# baseline characteristic in the severity groups -----
  
  insert_msg('Baseline clinics, severity groups')
  
  ## descriptive statistics
  
  ed_analysis$severity$desc <- explore(data = radio$clear, 
                                       variables = ed_analysis$variables, 
                                       split_factor = 'severity', 
                                       what = 'table', 
                                       pub_styled = TRUE, 
                                       simplify_p = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'mild', 'moderate', 'severe', 'critical'))
  
  ## testing for differences between the severity groups
  ## ANOVA for the age variable, see assumption testing above
  ## Chi-squared test for the categorical variables, Kruskal-Wallis test for the PKY variable
  
  ed_analysis$severity$test <- compare_variables(radio$clear, 
                                                 variables = ed_analysis$variables, 
                                                 split_factor = 'severity', 
                                                 what = 'test', 
                                                 types = ed_analysis$test_types, 
                                                 pub_styled = TRUE, 
                                                 simplify_p = TRUE, 
                                                 ci = FALSE)
  
  ## common table
  
  ed_analysis$severity <- left_join(ed_analysis$severity$desc, 
                                    ed_analysis$severity$test[c('variable', 'test', 'significance', 'eff_size')], 
                                    by = 'variable')

# CT features at the time points, entire cohort ------
  
  insert_msg('CT features, entire cohort')
  
  ## frequency of particular CT features
  
  ed_analysis$ct$desc <- explore(data = radio$long, 
                                 split_factor = 'visit', 
                                 variables = radio$cat_outcome_var, 
                                 what = 'table', 
                                 pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>%
    set_names(c('variable', paste0('fup', 1:4)))
  
  ## chi-squared tests
  
  ed_analysis$ct$test <- compare_variables(radio$long, 
                                           split_factor = 'visit', 
                                           variables = radio$cat_outcome_var, 
                                           types = 'chisq_test', 
                                           what = 'test', 
                                           pub_styled = TRUE, 
                                           ci = FALSE)
  
  ## common table
  
  ed_analysis$ct <- left_join(ed_analysis$ct$desc, 
                              ed_analysis$ct$test[c('variable', 'test', 'significance', 'eff_size')], 
                              by = 'variable')
  
# CT score, percent opacity and percent hi opacity at the time points ------

  insert_msg('CT score at the visits')
  
  ## descriptive statistic
  
  ed_analysis$ct_scoring$desc <- explore(data = radio$long, 
                                         split_factor = 'visit', 
                                         variables = c('ctss', 'perc_opac', 'perc_hiopac'), 
                                         what = 'table', 
                                         pub_styled = TRUE, 
                                         simplify_p = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', paste0('fup', 1:4)))
  
  ## testing: Kruskal-Wallis test
  
  ed_analysis$ct_scoring$test <- compare_variables(radio$long, 
                                                   variables = c('ctss', 'perc_opac', 'perc_hiopac'), 
                                                   split_factor = 'visit', 
                                                   what = 'test', 
                                                   types = 'kruskal_test', 
                                                   ci = FALSE, 
                                                   pub_styled = TRUE, 
                                                   simplify_p = TRUE)
  
  ## common table
  
  ed_analysis$ct_scoring <- left_join(ed_analysis$ct_scoring$desc, 
                                      ed_analysis$ct_scoring$test[c('variable', 'test', 'significance', 'eff_size')], 
                                      by = 'variable')
  
# END -----
  
  insert_tail()