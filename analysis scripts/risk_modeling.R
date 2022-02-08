# This script models risk of any abnormality, any opacity, GGO and reticulation,  
# (logistic regression) at the 1-year follow-up. Explanatory variables
# are sex, age, severity, BMI, smoking, PKY. Multi-parameter models are constructed by backward elimination.

  insert_head()
  
# Container list -----
  
  risk <- list()
  
# Globals ------
  
  insert_msg('Globals')

  risk$outcomes <-  c('ggo_fup4', 
                      'retic_fup4', 
                      'ctss_any_fup4', 
                      'opacity_fup4')
  
  ## baseline levels
  
  risk$base_levels <- radio$long_expl_var %>% 
    map_chr(~levels(radio$clear[[.x]])[1]) %>% 
    set_names(radio$long_expl_var)

# Serial univariate modeling -----

  insert_msg('Serial univariate modeling')  
  
  ## modeling objects
  
  risk$univariate$models <- risk$outcomes %>% 
    map(function(resp) radio$long_expl_var %>% 
          map(~make_lm(data = radio$clear, 
                       response = resp, 
                       indep_variable = .x, 
                       mod_fun = glm, 
                       family = 'binomial')) %>% 
          set_names(radio$long_expl_var)) %>% 
    set_names(risk$outcomes)
  
  ## residuals: normality and variance homogeneity testing

  risk$univariate$resid_normality_eov <- risk$univariate$models %>% 
    map(~map(.x, summary, type = 'assumptions') %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))

  ## diagnostic plots: residuals
  
  risk$univariate$diagnostic_plots <- risk$univariate$models %>% 
    map(~map(.x, plot, cust_theme = globals$common_theme))
  
  ## fit statistics
  
  risk$univariate$fit_stats <- risk$univariate$models %>% 
    map(~map(.x, summary, type = 'fit') %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y)))
  
  ## inference summary, appending with the baseline levels
  
  risk$univariate$inference <- risk$univariate$models %>% 
    map(~map(.x, summary, type = 'inference', transf_fun = exp) %>% 
          map(~mutate(.x, 
                      plot_order = 1:nrow(.x), 
                      n = ifelse(is.na(n), n_complete[1] - sum(n, na.rm = TRUE), n))) %>% 
          map2(., names(.), ~mutate(.x, variable = .y)) %>% 
          map2_dfr(., risk$base_levels, ~mutate(.x, level = ifelse(level == 'baseline', .y, level))))
  
# Univariate Forest plots ----
  
  insert_msg('Univariate Forest plots')
  
  risk$univariate$plots <- list(summary_tbl = risk$univariate$inference, 
                                plot_title = c('GGO, 1-year follow-up', 
                                               'Reticulations, 1-year follow-up', 
                                               'CT abnormality, 1-year follow-up', 
                                               'Opacity, 1-year follow-up')) %>% 
    pmap(my_plot_forest, 
         x_trans = 'log2', 
         plot_subtitle = 'Uni-variable logistic regression', 
         hide_baseline_est = TRUE, 
         x_lab = 'OR')
  
# Development of multivariate models ----
  
  insert_msg('Multivariate modeling')
  
  ## optimization of the models by AIC-driven backward elimination
  
  risk$multivariate$models <- risk$outcomes %>% 
    map(~make_lm(data = radio$clear, 
                 response = .x, 
                 mod_fun = glm, 
                 indep_variable = radio$long_expl_var, 
                 family = 'binomial')) %>% 
    map(step) %>% 
    set_names(risk$outcomes)
  
  # residuals: normality and variance homogeneity testing
  
  risk$multivariate$resid_normality_eov <- risk$multivariate$models %>% 
    map(summary, type = 'assumptions') %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
  ## diagnostic plots: residuals
  
  risk$multivariate$diagnostic_plots <- risk$multivariate$models %>% 
    map(plot, cust_theme = globals$common_theme)
  
  ## fit statistics
  
  risk$multivariate$fit_stats <- risk$multivariate$models %>% 
    map(summary, type = 'fit')
  
  ## inference summary, appending with the baseline levels
  
  risk$multivariate$inference <- risk$multivariate$models %>% 
    map(summary, type = 'inference', transf_fun = exp) %>% 
    map(~mutate(.x, plot_order = 1:nrow(.x)))
  
# Multi-parameter model cross-validation -----
  
  insert_msg('Cross-validation of the multi-parameter models')

  set.seed(1234)
  
  registerDoParallel(cores = 7)
  
  risk$cv$caret_models <- risk$multivariate$models %>% 
    map(formula) %>% 
    map(~train(form = .x, 
               data = radio$clear, 
               method = 'glm', 
               family = 'binomial', 
               metric = 'Kappa', 
               trControl = trainControl(method = 'cv', 
                                        number = 20, 
                                        savePredictions = 'final', 
                                        classProbs = TRUE))) %>% 
    map(as_caretx)
  
  stopImplicitCluster()

  ## diagnostic plots of the residuals
  
  risk$cv$diagnostic_plots <- risk$cv$caret_models %>% 
    map(plot, 
        type = 'diagnostic', 
        cust_theme = globals$common_theme)
  
  ## fit statistics, ROC statistics to be shown in the plot captions
  
  risk$cv$fit_stats <- risk$cv$caret_models %>% 
    map(summary, ci = FALSE)

  risk$cv$plot_captions <- risk$cv$fit_stats %>% 
    map(~.x$cv) %>% 
    map(~paste0('CV: AUC = ', signif(.x[['estimate']][9], 2), 
                ', Se = ', signif(.x[['estimate']][10], 2), 
                ', Sp = ', signif(.x[['estimate']][11], 2))) %>% 
    map(stri_replace_all, fixed = '0.', replacement = '.') %>% 
    map2(., map(risk$cv$caret_models, nobs), ~paste0('n = ', .y, ', ', .x))

# Multi-parameter model Forest plots ------
  
  insert_msg('Multi-parameter Forest plot')
  
  risk$multivariate$plots <- list(summary_tbl = risk$multivariate$inference, 
                                  plot_title = c('GGO, 1-year follow-up', 
                                                 'Reticulations, 1-year follow-up', 
                                                 'CT abnormality, 1-year follow-up', 
                                                 'Opacity, 1-year follow-up'), 
                                  plot_subtitle = 'Multivariable logistic regression') %>% 
    pmap(my_plot_forest, 
         x_trans = 'log2', 
         hide_baseline_est = TRUE, 
         x_lab = 'OR', 
         hide_ns = TRUE)
  
# END ----
  
  insert_tail()