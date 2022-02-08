# Models risk of any CT abnormalities, any opacity, GGO and reticulations with logistic GAM.
# The numeric variables: age and pack-years are treated with cubic splines.

  insert_head()
  
# container list ----
  
  risk_gam <- list()
  
# Globals ------
  
  insert_msg('Globals')
  
  risk_gam$outcomes <-  c('ggo_fup4', 
                          'retic_fup4', 
                          'ctss_any_fup4', 
                          'opacity_fup4')
  
  ## baseline levels
  
  risk_gam$base_levels <- c('sex', 'bmi_class', 'smoking', 'severity') %>% 
    map_chr(~levels(radio$clear[[.x]])[1]) %>% 
    set_names(c('sex', 'bmi_class', 'smoking', 'severity'))
  
  ## univariate modeling formulas
  
  risk_gam$uni_forms <- risk_gam$outcomes %>% 
    map(function(resp) c('s(age, bs = "tp", k = 20)', 's(pky, bs = "tp", k = 15)') %>% 
          map(~paste(resp, .x, sep = '~')) %>%
          map(as.formula) %>%
          set_names(c('age', 'pky'))) %>% 
    set_names(risk_gam$outcomes)
  
  ## multi-variate modeling formulas
  
  risk_gam$multi_forms <- risk_gam$outcomes %>% 
    map(~paste(.x, '~ sex + severity + s(age, bs = "tp", k = 20)')) %>% 
    map(as.formula) %>% 
    set_names(risk_gam$outcomes)
  
# Univariate modeling -----
  
  insert_msg('Univariate modeling')
  
  ## model objects
  
  risk_gam$univariate$models <- risk_gam$uni_forms  %>% 
    map(~map(.x, 
             ~make_lm(formula = .x, 
                      data = radio$clear, 
                      family = 'binomial', 
                      mod_fun = gam, 
                      method = 'REML')))
  

  ## residuals, normality testing
  
  risk_gam$univariate$resid_normality_eov <- risk_gam$univariate$models %>% 
    map(~map(.x, summary, type = 'assumptions') %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
  ## diagnostic plots: residuals
  
  risk_gam$univariate$diagnostic_plots <- risk_gam$univariate$models %>% 
    map(~map(.x, plot, cust_theme = globals$common_theme))
  
  ## fit statistics
  
  risk_gam$univariate$fit_stats <- risk_gam$univariate$models %>% 
    map(~map(.x, summary, type = 'fit') %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y)))
  
  ## inference summary, appending with the baseline levels
  
  risk_gam$univariate$inference <- risk_gam$univariate$models %>% 
    map(~map(.x, summary, type = 'inference', transf_fun = exp) %>% 
          map(~mutate(.x, 
                      plot_order = 1:nrow(.x), 
                      n = ifelse(is.na(n), n_complete[1] - sum(n, na.rm = TRUE), n))) %>% 
          map2(., names(.), ~mutate(.x, variable = .y)))
  
  ## significance of the smoothers
  
  risk_gam$univariate$anova <- risk_gam$univariate$models %>%
    map(~map(.x, anova))
  
# Univariate prediction plots for smoothers -----
  
  insert_msg('Univariate modeling prediction plots')
  
  risk_gam$univariate$pred_plots <- risk_gam$univariate$models %>% 
    map(~map(.x, plot_gam_predictions) %>% 
          unlist(recursive = FALSE)) %>% 
    unlist(recursive = FALSE)
  
  ## ANOVA p values in the captions
  
  risk_gam$univariate$pred_plots <- map2(risk_gam$univariate$pred_plots, 
                                         unlist(risk_gam$univariate$anova, recursive = FALSE), 
                                         ~.x + labs(subtitle = paste('Smoother: p =', signif(.y$p_value, 2)))) %>% 
    map2(., unlist(map(c('GGO', 'Reticulations', 'Any abnormalities', 'Any opacity'), rep, 2)), 
         ~.x + labs(title = .y, y = paste('Risk of', tolower(.y)))) %>% 
    map2(., rep(c('Age, years', 'Smoking, pack-years'), length(risk_gam$outcomes)), 
         ~.x + labs(x = .y))

# Multi-variate modeling ------
  
  insert_msg('Multivariate model construction and diagnostics')
  
  ## model building
  
  risk_gam$multivariate$models <- risk_gam$multi_forms %>% 
    map(~make_lm(data = radio$clear, 
                 formula = .x, 
                 mod_fun = gam, 
                 family = 'binomial', 
                 method = 'REML'))
  
  ## residuals, normality and EOV testing
  
  risk_gam$multivariate$resid_normality_eov <- risk_gam$multivariate$models %>% 
    map(summary, type = 'assumptions') %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
  ## diagnostic plots: residuals
  
  risk_gam$multivariate$diagnostic_plots <- risk_gam$multivariate$models %>% 
    map(plot, cust_theme = globals$common_theme)
  
  ## fit statistics
  
  risk_gam$multivariate$fit_stats <- risk_gam$multivariate$models %>% 
    map_dfr(summary, type = 'fit')
  
  ## inference summary
  
  risk_gam$multivariate$inference <- risk_gam$multivariate$models %>% 
    map(summary, type = 'inference')
  
  ## model term summary
  
  risk_gam$multivariate$anova <-  risk_gam$multivariate$models %>% 
    map(anova)
  
# Cross-validation ------
  
  insert_msg('Cross-validation')
  
  ## cross-validation. the Caret defaults are not exactly the same (different k: k = 10 for Caret)
  
  set.seed(1234)
  
  registerDoParallel(cores = 7)
  
  risk_gam$cv$caret_models <-  risk_gam$outcomes %>% 
    map(~paste(.x, '~ sex + severity + age')) %>% 
    map(as.formula) %>%  
    set_names(risk_gam$outcomes) %>% 
    map(~train(form = .x, 
               data = radio$clear, 
               method = 'gam', 
               family = 'binomial', 
               metric = 'Kappa', 
               trControl = trainControl(method = 'cv', 
                                        number = 20, 
                                        savePredictions = 'final', 
                                        classProbs = TRUE), 
               tuneGrid = data.frame(method = 'REML', 
                                     select = FALSE))) %>% 
    map(as_caretx)
  
  stopImplicitCluster()
  
  ## diagnostic plots of the residuals
  
  risk_gam$cv$diagnostic_plots <- risk_gam$cv$caret_models %>% 
    map(plot, 
        type = 'diagnostic', 
        cust_theme = globals$common_theme)
  
  ## fit statistics, ROC statistics to be shown in the plot captions
  
  risk_gam$cv$fit_stats <- risk_gam$cv$caret_models %>% 
    map(summary, ci = FALSE)
  
# Comparison of the multi-parameter smoother models and models with manual age stratification ------
  
  insert_msg('Comparison of GAM and cutoff models')
  
  ## GAM models
  
  risk_gam$comparison$gam_models <- list(caretx_model = risk_gam$cv$caret_models, 
                                         plot_title = list(c('GGO, GAM, training', 
                                                             'GGO, GAM, CV'), 
                                                           c('Reticulations, GAM, training', 
                                                             'Reticulations, GAM, CV'), 
                                                           c('Any CT abnormalities, GAM, training', 
                                                             'Any CT abnormalities, GAM, CV'), 
                                                           c('Any opacity, GAM, training',
                                                             'Any opacity, GAM, CV'))) %>% 
    pmap(plot, 
         type = 'roc', 
         line_color = 'coral3', 
         cust_theme = globals$common_theme) %>%  
    map(~map(.x, ~.x + labs(tag = paste0('\n', .x$labels$tag))))
  
  ## Cutoff-models
  
  risk_gam$comparison$cutoff_models <- list(caretx_model = risk$cv$caret_models, 
                                            plot_title = list(c('GGO, age cutoff, training', 
                                                                'GGO, age cutoff, CV'), 
                                                              c('Reticulations, age cutoff, training', 
                                                                'Reticulations, age cutoff, CV'), 
                                                              c('Any CT abnormalities, age cutoff, training', 
                                                                'Any CT abnormalities, age cutoff, CV'), 
                                                              c('Any opacity, age cutoff, training',
                                                                'Any opacity, age cutoff, CV'))) %>% 
    pmap(plot, 
         type = 'roc', 
         line_color = 'steelblue4', 
         cust_theme = globals$common_theme) %>% 
    map(~map(.x, ~.x + labs(tag = paste0('\n', .x$labels$tag))))
  
# END -----
  
  insert_tail()