# This script models CTSS (Poisson regression) and CTSS class,  
# (logistic regression) at the 1-year follow-up. Explanatory variables
# are sex, age, severity, BMI, smoking, PKY. Multi-parameter models are constructed by backward elimination.

  insert_head()
  
# Container list -----
  
  score <- list()

# Globals ------
  
  insert_msg('Globals')

  score$outcomes <-  c('ctss_fup4', 'ctss_class_fup4')

  ## baseline and baseline n numbers
  
  score$base_levels <- radio$long_expl_var %>% 
    map_chr(~levels(radio$clear[[.x]])[1]) %>% 
    set_names(radio$long_expl_var)

  ## faceting formulas 
  
  score$facet_forms <- paste('.~', radio$long_expl_var) %>% 
    map(as.formula)
  
# Serial univariate modeling -----

  insert_msg('Serial univariate modeling')  
  
  ## modeling objects
  
  score$univariate$models <- list(response  = score$outcomes, 
                                  mod_fun = list(glm, polr), 
                                  family = list('poisson', NULL)) %>% 
    pmap(function(response, mod_fun, family) radio$long_expl_var %>% 
           map(~make_lm(data = radio$clear, 
                        response = response, 
                        indep_variable = .x, 
                        mod_fun = mod_fun, 
                        family = family)) %>% 
           set_names(radio$long_expl_var)) %>% 
    set_names(score$outcomes)

  ## residuals: normality and variance homogeneity testing
  
  score$univariate$resid_normality_eov <- score$univariate$models %>% 
    map(~map(.x, summary, type = 'assumptions') %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))

  ## diagnostic plots: residuals
  
  score$univariate$diagnostic_plots <- score$univariate$models %>% 
    map(~map(.x, plot, cust_theme = globals$common_theme))
  
  ## diagnostic plots: response distribution
  
  score$univariate$distr_plots <- map2(score$univariate$models, 
                                       c(10, 4), 
                                       function(models, bin_no) list(x = models, 
                                                                     facet_formula = score$facet_forms) %>% 
                                         pmap(plot, 
                                              type = 'distribution', 
                                              bins = bin_no, 
                                              cust_theme = globals$common_theme))
  
  ## diagnostic plots: response relationship
  
  score$univariate$response_plots <- score$univariate$models %>% 
    map(~map(.x, plot, type = 'relationship')) %>% 
    map(unlist, recursive = FALSE)
  
  ## fit statistics
  
  score$univariate$fit_stats <- score$univariate$models %>% 
    map(~map(.x, summary, type = 'fit') %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y)))
  
  ## inference summary, appending with the baseline levels
  
  score$univariate$inference <- map2(score$univariate$models, 
                                     c('default', 'default'), 
                                     function(model_lst, ci_method) model_lst %>% 
                                       map(summary, 
                                           type = 'inference', 
                                           transf_fun = exp, 
                                           ci_method = ci_method))
  
  score$univariate$inference <- score$univariate$inference %>% 
    map(~map(.x, 
             ~mutate(.x, 
                     plot_order = 1:nrow(.x), 
                     n = ifelse(is.na(n), n_complete[1] - sum(n, na.rm = TRUE), n))) %>% 
          map2(., names(.), ~mutate(.x, variable = .y)) %>% 
          map2_dfr(., score$base_levels, ~mutate(.x, level = ifelse(level == 'baseline', .y, level))))

# Univariate Forest plots ----
  
  insert_msg('Univariate Forest plots')
  
  score$univariate$plots <- list(summary_tbl = score$univariate$inference, 
                                plot_title = c('CTSS, 1-year follow-up', 
                                               'CTSS class, 1-year follow-up'), 
                                plot_subtitle = c('Univariable Poisson regression', 
                                                  'Univariable ordinal regression')) %>% 
    pmap(my_plot_forest, 
         x_trans = 'log2', 
         hide_baseline_est = TRUE, 
         x_lab = 'OR')
  
# Development of multivariate models ----
  
  insert_msg('Multivariate modeling')
  
  ## optimization of the models by AIC-driven backward elimination
  
  score$multivariate$models <- list(response = score$outcomes, 
                                    mod_fun  = list(glm, polr), 
                                    family = list('poisson', NULL)) %>% 
    pmap(make_lm, 
         data = radio$clear, 
         indep_variable = radio$long_expl_var) %>% 
    set_names(score$outcomes) %>% 
    map(step, 
        step_fun = stepAIC, 
        direction = 'backward')

  # residuals: normality and variance homogeneity testing

  score$multivariate$resid_normality_eov <- score$multivariate$models %>% 
    map(summary, type = 'assumptions') %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
  ## diagnostic plots: residuals
  
  score$multivariate$diagnostic_plots <- score$multivariate$models %>% 
    map(plot, cust_theme = globals$common_theme)
  
  ## fit statistics
  
  score$multivariate$fit_stats <- score$multivariate$models %>% 
    map(summary, type = 'fit')
  
  ## inference summary, appending with the baseline levels
  
  score$multivariate$inference <- score$multivariate$models %>% 
    map(summary, type = 'inference', transf_fun = exp) %>% 
    map(~mutate(.x, plot_order = 1:nrow(.x)))
  
# Multi-parameter model cross-validation -----
  
  insert_msg('Cross-validation of the multi-parameter models')

  set.seed(1234)
  
  registerDoParallel(cores = 7)
  
  score$cv$caret_models$ctss_fup4 <- train(form = formula(score$multivariate$models$ctss_fup4), 
                                           data = model.frame(score$multivariate$models$ctss_fup4),
                                           method = 'glm', 
                                           metric = 'RMSE', 
                                           trControl = trainControl(method = 'cv', 
                                                                    number = 20, 
                                                                    savePredictions = 'final', 
                                                                    classProbs = TRUE), 
                                           family = 'poisson')
  
  score$cv$caret_models$ctss_class_fup4 <- train(form = formula(score$multivariate$models$ctss_class_fup4), 
                                                 data = model.frame(score$multivariate$models$ctss_class_fup4),
                                                 method = 'polr', 
                                                 metric = 'Kappa', 
                                                 trControl = trainControl(method = 'cv', 
                                                                          number = 20, 
                                                                          savePredictions = 'final', 
                                                                          classProbs = TRUE), 
                                                 tuneGrid = data.frame(method = 'logistic'))
  
  stopImplicitCluster()
  
  score$cv$caret_models <- score$cv$caret_models %>% 
    map(as_caretx)

  ## diagnostic plots of the residuals and calibration plot for the Poisson model
  ## confusion matrix for the ordinal model
  
  score$cv$diagnostic_plots$ctss_fup4 <- plot(score$cv$caret_models$ctss_fup4, 
                                              type = 'diagnostic', 
                                              cust_theme = globals$common_theme)
  
  score$cv$diagnostic_plots$ctss_class_fup4 <- plot(score$cv$caret_models$ctss_class_fup4, 
                                                    type = 'confusion', 
                                                    scale = 'percent', 
                                                    cust_theme = globals$common_theme)
  
  score$cv$fit_plots$ctss_fup4 <- plot(score$cv$caret_models$ctss_fup4, 
                                       type = 'fit', 
                                       cust_theme = globals$common_theme) %>% 
    map(~.x + 
          expand_limits(x = 15, y = 15))

  ## fit statistics, ROC statistics to be shown in the plot captions
  
  score$cv$fit_stats <- score$cv$caret_models %>% 
    map(summary, ci = FALSE)

  score$cv$plot_captions$ctss_fup4 <- paste0('n = ', nobs(score$cv$caret_models$ctss_fup4), 
                                             ', CV: RMSE = ', signif(score$cv$fit_stats$ctss_fup4$cv$estimate[3], 2), 
                                             ', Rsq = ', signif(score$cv$fit_stats$ctss_fup4$cv$estimate[4], 2))
  
  score$cv$plot_captions$ctss_class_fup4 <- paste0('n = ', nobs(score$cv$caret_models$ctss_class_fup4), 
                                                   ', CV: Kappa = ', signif(score$cv$fit_stats$ctss_class_fup4$cv$estimate[3], 2), 
                                                   ', Accuracy = ', signif(score$cv$fit_stats$ctss_class_fup4$cv$estimate[2], 2))
  
# Multi-parameter model Forest plots ------
  
  insert_msg('Multi-parameter Forest plot')
  
  score$multivariate$plots <- list(summary_tbl = score$multivariate$inference, 
                                   plot_title = c('CTSS, 1-year follow-up', 
                                                  'CTSS class, 1-year follow-up'), 
                                   plot_subtitle = c('Multivariable Poisson regression', 
                                                     'Multivariable ordinal regression'), 
                                   x_lab = list(expression('exp '*beta), 
                                                'OR')) %>% 
    pmap(my_plot_forest, 
         x_trans = 'log2', 
         hide_baseline_est = TRUE, 
         hide_ns = TRUE)
  
# END ----
  
  insert_tail()