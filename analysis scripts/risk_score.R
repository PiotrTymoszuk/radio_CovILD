# This script models risk of any, moderate-severe, severe, ggo, reticulation, consolidations and bronchial dilatation 
# (logistic regression) as well as of the abnormality grading at the 1-year follow-up. Explanatory variables
# are> sex, age, severity, bin, smoking parameters as well as initial CT abnormality severity (CTSS with the cutoffs 5 and 10)
# Multi-paramater models are constructed by backward elimination and validated by 20-fold CV

  insert_head()
  
# container list -----
  
  risk <- list()


# globals ------
  
  insert_msg('Globals')
  
  risk$explanatory_vars <- c(radio$long_expl_var)
  
  risk$outcomes <-  c('ggo_fup4', 
                      'retic_fup4', 
                      'pili_fup4', 
                      'ctss_any_fup4', 
                      'ctss_fup4')
  
  risk$outcome_forms <- risk$outcomes %>% 
    map(~paste(.x, paste(risk$explanatory_vars, collapse = '+'), sep = '~')) %>% 
    map(as.formula)
  
  risk$families <- c(rep('binomial', 4), 'poisson')
  
  risk$analysis_tbl <- radio$clear
  
  for(i in risk$outcomes) {
    
    if(is.factor(risk$analysis_tbl[[i]])) {
      
      risk$analysis_tbl <- risk$analysis_tbl %>% 
        mutate(!!sym(i) := as.numeric(.data[[i]]) - 1)
      
    }
    
  }
  
  risk$levene_forms <- risk$explanatory_vars %>% 
    paste('.resid ~', .) %>% 
    map(as.formula)
  
# serial univariate modeling -----

  insert_msg('Serial univariate modeling')  
  
  risk$univariate_models <- list(response = risk$outcomes, 
                                 family = risk$families) %>% 
    pmap(make_lm_model, 
        data = risk$analysis_tbl, 
        indep_variable = risk$explanatory_vars, 
        mod_fun = glm, 
        est_transf = exp, 
        error_resistant = FALSE) %>% 
    set_names(risk$outcomes)

  ## model diagnostic tables, normality and variance homogeneity testing
  
  risk$univariate_resids <- risk$univariate_models %>% 
    map(~map(.x, get_qc_tbl))
  
  risk$univariate_normality <- risk$univariate_resids %>% 
    map(~map(.x, ~safely(shapiro_test)(.x$.resid)) %>% 
          map(~.x$result) %>% 
          compact %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y)))
  
  risk$univariate_levene <- risk$univariate_resids %>% 
    map(~map2(.x, risk$levene_forms, ~safely(levene_test)(formula = .y, data = .x)) %>% 
          map(~.x$result) %>% 
          compact %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y)))
  
  ## diagnostic plots
  
  risk$univariate_diagnostic <- risk$univariate_models %>% 
    map(~map(.x, get_qc_plots))
  
  ## n numbers, and baseline levels
  
  risk$baseline_counts <- risk$explanatory_vars %>% 
    map_dfr(get_baseline, data = risk$analysis_tbl)

  ## inference summary 
  
  risk$univariate_summary <- risk$univariate_models %>% 
    map(get_model_summary) %>% 
    map(left_join, risk$baseline_counts, by = c('variable', 'level'))
  
  ## inference Forest plots
  
  risk$univariate_plots <- list(summary_tbl = risk$univariate_summary, 
                                plot_title = c('GGO, 1-year FUP', 
                                               'Reticulations, 1-year FUP', 
                                               'PILI, 1-year FUP', 
                                               'Any CT abnormality, 1-year FUP', 
                                               'CTSS, 1-year FUP'), 
                                plot_subtitle = c(rep('Logistic regression', 4),  
                                                      'Poisson regression'), 
                                hide_baseline_est = c(rep(TRUE, 4), FALSE), 
                                x_lab = c(rep('OR', 4), 'exp \u03B2')) %>% 
    pmap(plot_forest, 
         x_trans = 'log2')

# multi-parameter modeling, backward elimination -----
  
  insert_msg('Multi-paramater modeling, backward elimination')
  
  set.seed(1234)
  
  ## models
  
  plan('multisession')
  
  risk$multivariate_models <- list(form = risk$outcome_forms, 
                                   family = risk$families, 
                                   metric = c(rep('Kappa', 4), 
                                              'RMSE')) %>% 
    future_pmap(train, 
                data =  radio$clear, 
                method = 'glmStepAIC', 
                trControl = trainControl(method = 'cv', 
                                         number = 20, 
                                         savePredictions = 'final', 
                                         classProbs = TRUE), 
                .options = furrr_options(seed = TRUE)) %>% 
    set_names(risk$outcomes)
  
  plan('sequential')
  
  ## model diagnostic tables, normality and variance homogeneity testing
  
  risk$multivariate_resids <- risk$multivariate_models %>% 
    map(~.x$finalModel) %>% 
    map(get_qc_tbl)

  risk$multivariate_normality <- risk$multivariate_resids %>% 
    map(~.x$.resid) %>% 
    map(shapiro_test) %>% 
    map2_dfr(., names(.), ~mutate(.x, variable = .y))
  
  ## diagnostic plots
  
  risk$multivariate_diagnostic <- risk$multivariate_models %>% 
    map(~.x$finalModel) %>% 
    map(get_qc_plots)

  ## final models, inference
  
  risk$multivariate_summary <- risk$multivariate_models %>% 
    map(~get_estimates(.x$finalModel, 
                       transf_fun = exp)) %>% 
    map(mutate, 
        variable = stri_extract(parameter, regex = paste(risk$explanatory_vars, collapse = '|')), 
        variable = ifelse(parameter == '(Intercept)', 'baseline', variable), 
        level = stri_replace(parameter, regex = paste(risk$explanatory_vars, collapse = '|'), replacement = ''), 
        level = stri_replace_all(level, fixed = '`', replacement = ''), 
        level = ifelse(variable == 'baseline', 'baseline', level)) %>% 
    map(left_join, risk$baseline_counts, by = c('variable', 'level'))
  
  ## cross-validation statistic: ROC for the classification, 
  
  risk$multivariate_cv[risk$outcomes[1:4]] <- risk$multivariate_models[risk$outcomes[1:4]] %>% 
    map(~twoClassSummary(.x$pred, lev = c('no', 'yes'))) %>% 
    map2(., names(.), ~tibble(outcome = .y, 
                              auc = .x[1], 
                              sens = .x[2], 
                              spec = .x[3])) %>% 
    map(mutate, 
        plot_caption = paste0('Logistic regression, n = ', nrow(risk$analysis_tbl), 
                              ', CV: AUC = ', signif(auc, 2), 
                              ', Se = ', signif(sens, 2), 
                              ', Sp = ', signif(spec, 2)))
  
  risk$multivariate_cv$ctss_fup4 <- risk$multivariate_models$ctss_fup4$results[c('MAE', 'Rsquared')] %>% 
    mutate(outcome = 'ctss_any_fup4', 
           plot_caption = paste0('Poisson regression, n = ', nrow(risk$analysis_tbl), 
                                 ', CV: MAE = ', signif(MAE, 2), 
                                 ', Rsq = ', signif(Rsquared, 2)))
  
  ## forest plots
  
  risk$multivariate_plots <- list(summary_tbl = risk$multivariate_summary, 
                                  plot_title = c('GGO, 1-year FUP', 
                                                 'Reticulations, 1-year FUP', 
                                                 'PILI, 1-year FUP', 
                                                 'Any CT abnormality, 1-year FUP', 
                                                 'CTSS, 1-year FUP'), 
                                  plot_subtitle = map(risk$multivariate_cv, ~.x$plot_caption), 
                                  hide_baseline_est = c(rep(TRUE, 4), FALSE), 
                                  x_lab = c(rep('OR', 4), 'exp \u03B2')) %>% 
    pmap(plot_forest, 
         x_trans = 'log2')
  
# END ----
  
  insert_tail()