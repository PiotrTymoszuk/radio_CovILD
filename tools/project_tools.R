# This is a container script for the project-specific tools

  require(plyr)
  require(tidyverse)
  require(rlang)
  require(stringi)
  require(foreign)
  require(vcd)
  require(rcompanion)
  
# data import and transformation -----
  
  recode_var <- function(data, 
                         old_var, 
                         new_var, 
                         ID_var = 'ID', 
                         trans_fun = 'my_identity', 
                         args = NULL) {
    
    ## renames a variable, if time and id variable names provided, they're included
    ## in  the output as well
    ## the transformation function enables
    ## args specifies the arguments to the transforming function
    
    trans_call <- call2(trans_fun, 
                        data[[old_var]], 
                        !!!args)

    new_data <- data %>% 
      mutate(!!ensym(new_var) := eval(trans_call))

    return(new_data[c(ID_var, enexpr(new_var))])
    
  }
  
  rm_spaces <- function(vector, out_format = 'character') {
    
    ## removes trailing spaces
    
    cut_vector <- vector %>% 
      stri_replace_all(regex = '\\s+$', replacement = '')
    
    cut_vector <- ifelse(stri_length(cut_vector) == 0, NA, cut_vector)
    
    formatter <- switch(out_format,
                        character = as.character,
                        numeric = as.numeric,
                        logical = as.logical)
    
    return(formatter(cut_vector, out_format))
    
  }
  
  recode_vec <- function(vector, recodes, ...) {
    
    return(car::recode(vector, 
                       recodes = recodes, 
                       ...))
    
  }
  
  recode_yn <- function(vector, reverse = F, as.factor = T, ...) {
    
    ## recodes a 0/1 vector no/yes or the other way round
    
    if(reverse) {
      
      new_vec <- recode_vec(vector, 
                            "'yes' = 1;
                             'no' = 0",
                            as.factor = as.factor)

    } else {
      
      new_vec <- recode_vec(vector, 
                            "1 = 'yes';
                             0 = 'no'", 
                            as.factor = as.factor)
      
    }

    return(new_vec)
    
  }

  binarize <- function(vector, cutoff, labels = "'yes';'no'", new_levels = NULL) {
    
    cut_vec <- cut(as.numeric(vector), 
                   c(-Inf, cutoff, Inf), 
                   unlist(stri_split_fixed(labels, ';')))
    
    
    if(!is.null(new_levels)) {
      
      new_levels <- unlist(stri_split_fixed(new_levels, ';'))
      
      cut_vec <- factor(cut_vec, new_levels)
      
    }
    
    return(cut_vec)
    
  }
  
  my_identity <- function(x, ...) {
    
    identity(x)
    
  }
  
  my_factor <- function(x, levels, ...) {
    
    factor(x, levels =  unlist(stri_split_fixed(levels, ';')))
    
  }
  
  my_cut <- function(vector, cutpoints, labels, new_levels = NULL, ...) {
    
    ## cutpoints provided as ;-separated string
    
    cutpoints <- unlist(stri_split_fixed(cutpoints, ';'))
    
    cutpoints <- c(-Inf, cutpoints, Inf)
    
    labels <- unlist(stri_split_fixed(labels, ';'))
    
    cut_vec <- cut(as.numeric(vector), 
                   cutpoints, 
                   labels, ...)
    
    if(!is.null(new_levels)) {
      
      new_levels <- unlist(stri_split_fixed(new_levels, ';'))
      
      cut_vec <- factor(cut_vec, new_levels)
      
    }
    
    return(cut_vec)
    
  }
  
  my_extract <- function(vector, regex, 
                         out_format = 'character', 
                         mode = 'first') {
    
    mode <- match.arg(mode, 
                      c('first', 'last', 'all'))
    
    formatter <- switch(out_format,
                        character = as.character,
                        numeric = as.numeric,
                        logical = as.logical)
    
    extractor <- switch(mode,
                        first = stri_extract_first_regex,
                        last = stri_extract_last_regex,
                        all = stri_extract_all_regex)
    
    vector %>% 
      extractor(pattern = regex) %>% 
      formatter
    
    
  }
  
  outer_rbind <- function(tbl1, tbl2) {
    
    ## binds two data frames by rows, missing variables are filled with NA
    
    ## missing variables
    
    miss1 <- names(tbl2)[!names(tbl2) %in% names(tbl1)]
    miss2 <- names(tbl1)[!names(tbl1) %in% names(tbl2)]
    
    ## filling the tables
    
    for(i in miss1){
      
      tbl1 <- tbl1 %>% 
        mutate(!!sym(i) := NA)
      
    }
    
    for(i in miss2){
      
      tbl2 <- tbl2 %>% 
        mutate(!!sym(i) := NA)
      
    }
    
    return(rbind(tbl1, tbl2))
    
  }
  
  min_max <- function(vector) {
    
    (vector - min(vector, na.rm = T))/(max(vector, na.rm = T) - min(vector, na.rm = T))
    
  }
  
  pss2date <- function(vector) as.Date(vector/86400, origin = '1582-10-14')
  
  difftime_years <- function(vector1, vector2) round(as.numeric(pss2date(vector1) - pss2date(vector2), format = 'days')/365.25)
  

# variable translation ----
  
  translate_var <- function(variable, 
                            key = 'new_var', 
                            out_value = 'label', 
                            dict = radio$vars, 
                            unit = FALSE, 
                            suffix = FALSE) {
    
    naming_vec <- dict[[out_value]]
    
    if(suffix) {
      
      naming_vec <- ifelse(is.na(dict[['suffix']]), 
                           naming_vec, 
                           paste(naming_vec, dict[['suffix']]))
      
    }
    
    if(unit) {
      
      naming_vec <- ifelse(is.na(dict[['unit']]), 
                           naming_vec, 
                           paste(naming_vec, dict[['unit']], sep = ', '))
      
    }
    
    naming_vec <- set_names(naming_vec, 
                            dict[[key]])
    
    return(naming_vec[variable])
    
  }
  
# kinetic modeling and plotting -----
  
  post_eff_size <- function(data) {
    
    ## perforems post hoc testing with Wilcoxon test
    ## and r effect size. Consecutive visits are compared
    
    data <- data %>% 
      dlply(.(visit))
    
    test_lst <- list(fup1_fup2 = data[c('fup1', 'fup2')], 
                     fup2_fup3 = data[c('fup2', 'fup3')], 
                     fup3_fup4 = data[c('fup3', 'fup4')])
    
    p_values <- test_lst %>% 
      map(~compare_variables(!!!.x, 
                             variables = c('ctss', 'perc_opac'), 
                             what = 'eff_size', 
                             types = 'paired_wilcoxon_r', 
                             ci = FALSE, 
                             pub_styled = TRUE, 
                             simplify_p = TRUE)) %>% 
      map2_dfr(., names(.), ~mutate(.x, comparison = .y)) %>% 
      mutate(significance = stri_replace(significance, fixed = '0.', replacement = '.'))
    
    eff_sizes <- test_lst %>% 
      map(~compare_variables(!!!.x, 
                             variables = c('ctss', 'perc_opac'), 
                             what = 'eff_size', 
                             types = 'paired_wilcoxon_r', 
                             ci = FALSE, 
                             pub_styled = FALSE)) %>% 
      map2_dfr(., names(.), ~mutate(.x, comparison = .y)) %>% 
      select(comparison, variable, estimate_name, estimate)
    
    left_join(p_values, eff_sizes, by = c('comparison', 'variable'))
    
  }

  plot_effects <- function(data, 
                           variable = 'ctss', 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           x_lab = 'Months post COVID-19', 
                           y_lab = 'Recovery effect, r', 
                           ci = TRUE, 
                           dodge_w = 0) {
    
    ## plots effect size at the consecutive visits
    
    plotting_tbl <- data %>% 
      filter(variable == !!variable) %>% 
      mutate(subset = factor(subset, names(globals$subset_colors)), 
             time = car::recode(comparison, "'fup1_fup2' = '3 vs 2'; 'fup2_fup3' = '6 vs 3'; 'fup3_fup4' = '12 vs 6'"), 
             time = factor(time, c('3 vs 2', '6 vs 3', '12 vs 6')))
    
    n_numbers <- plotting_tbl %>% 
      dlply(.(subset), function(x) x$n[1]/2)
    
    plot_tag <- map2_chr(globals$subset_labels, 
                         n_numbers, 
                         paste, sep = ': n = ') %>% 
      paste(collapse = '\n')
    
    plotting_tbl %>% 
      ggplot(aes(x = time, 
                 y = estimate, 
                 color = subset, 
                 group = subset)) + 
      geom_line() + 
      geom_point(shape = 16, 
                 size = 2) + 
      scale_color_manual(values = globals$subset_colors, 
                         labels = globals$subset_labels, 
                         name = '') + 
      globals$common_theme + 
      theme(plot.tag.position = 'right') + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab) + 
      facet_grid(.~subset, labeller = as_labeller(globals$subset_labels)) + 
      guides(color = FALSE)

  }
  
  plot_ctss <- function(data, 
                        plot_var = 'ctss', 
                        y_lab = 'CTSS', 
                        plot_title = NULL, 
                        plot_subtitle = NULL, 
                        fill_color = 'firebrick') {
    
    
    ## kinetic line plot
    
    ## median table
    
    med_tbl <- data %>% 
      dlply(.(visit)) %>% 
      map(explore, 
          variables = plot_var) %>% 
      map(~.x[[1]][['statistic']]) %>% 
      map2_dfr(., names(.), 
               ~tibble(visit = .y, 
                       median = .x[['value']][3], 
                       perc_25 = .x[['value']][4],
                       perc_75 = .x[['value']][5])) %>% 
      mutate(time_months = c(2, 3, 6, 12))
    
    n_numbers <- data %>% 
      filter(visit == 'fup1') %>% 
      nrow
    
    ## plot
    
    data %>% 
      ggplot(aes(x = time_months, 
                 y = .data[[plot_var]])) + 
      geom_ribbon(data = med_tbl, 
                  aes(y = median, 
                      ymin = perc_25, 
                      ymax = perc_75), 
                  fill = fill_color, 
                  alpha = 0.25) + 
      geom_line(aes(group = ID), 
                color = 'gray75') + 
      geom_line(data = med_tbl, 
                aes(y = median), 
                size = 1, 
                color = fill_color) + 
      scale_x_continuous(breaks = c(2, 3, 6, 12)) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = paste('\nn =', n_numbers), 
           x = 'Months post COVID-19', 
           y = y_lab)
    
  }

  add_ctss_post_p <- function(plot, 
                              post_hoc_results, 
                              variable = 'ctss', 
                              p_var = 'p', 
                              hoffset = 0.15, 
                              line_y = 21, 
                              voffset = 2, 
                              text_offset = 0.35) {
    
    ## adds the results of sequential post-hoc testing to the ctss plot
    
    y <- c(line_y, line_y - voffset, line_y - 2*voffset)
    
    post_hoc_results <- post_hoc_results %>% 
      filter(variable == !!variable) %>% 
      mutate(x0 = c(2, 3, 6), 
             x1 = c(3, 6, 12), 
             y0 = y, 
             y1 = y, 
             x_text = (x1 + x0)/2, 
             x0 = x0 + hoffset, 
             x1 = x1 - hoffset)

    plot + 
      scale_y_continuous(limits = NULL) + 
      geom_segment(data = post_hoc_results, 
                   aes(x = x0, 
                       xend = x1, 
                       y = y0, 
                       yend = y1)) + 
      geom_text(data = post_hoc_results, 
                aes(x = x_text, 
                    y = y1 + text_offset, 
                    label = significance), 
                size = 2.75, 
                hjust = 0.2, 
                vjust = 0)
    
  }
  
# risk and score modeling -----
  
  my_plot_forest <- function(summary_tbl, 
                             hide_baseline_est = TRUE, 
                             plot_title = NULL, 
                             plot_subtitle = NULL, 
                             plot_tag = NULL, 
                             x_lab = 'OR', 
                             x_trans = 'log2', 
                             hide_ns = FALSE) {
    
    if(hide_ns) summary_tbl <- filter(summary_tbl, p_value < 0.05)
    
    var_dict <- c(age_60 = 'Age\nyears', 
                  sex = 'Sex', 
                  bmi_class = 'BMI', 
                  smoking = 'Smoking\nhistory', 
                  pky_class = 'Smoking\npack-years', 
                  severity = 'COVID-19\nseverity')
    
    summary_tbl <- summary_tbl %>% 
      mutate(var_lab = ifelse(variable == 'baseline', 'Ref.', var_dict[variable]), 
             xax_lab = ifelse(is.na(var_lab), 
                              'reference', 
                              ifelse(variable == 'baseline', 
                                     'reference', 
                                     ifelse(parameter == '(Intercept)' | stri_detect(parameter, fixed = '|'), 
                                            paste0(level, ' (ref.), n = ', n), 
                                            paste0(level, ', n = ', n)))), 
             var_lab = ifelse(is.na(var_lab), 'Ref.', var_lab), 
             var_lab = factor(var_lab, levels = c('Ref.', var_dict)), 
             est_lab = paste0(signif(estimate, 2), ' [', signif(lower_ci, 2), ' - ', signif(upper_ci, 2), ']'), 
             significant = ifelse(p_value < 0.05, 'yes', 'no'), 
             regulation = ifelse(significant == 'no', 'ns', 
                                 ifelse(estimate > 1, 'positive', 'negative')), 
             plot_order = ifelse(stri_detect(parameter, fixed = '|'), 0, plot_order))
    
    if(hide_baseline_est) {
      
      summary_tbl <- summary_tbl %>% 
        mutate(estimate = ifelse(parameter == '(Intercept)' | stri_detect(parameter, fixed = '|'), 1, estimate), 
               lower_ci = ifelse(parameter == '(Intercept)' | stri_detect(parameter, fixed = '|'), 1, lower_ci), 
               upper_ci = ifelse(parameter == '(Intercept)' | stri_detect(parameter, fixed = '|'), 1, upper_ci), 
               est_lab = ifelse(parameter == '(Intercept)' | stri_detect(parameter, fixed = '|'), 1, est_lab), 
               regulation = ifelse(parameter == '(Intercept)' | stri_detect(parameter, fixed = '|'), 'ns', regulation))
      
    }
    
    summary_tbl %>% 
      ggplot(aes(x = estimate, 
                 y = reorder(xax_lab, -plot_order), 
                 color = regulation)) + 
      facet_grid(var_lab ~ ., 
                 scales = 'free', 
                 space = 'free', 
                 switch = 'y') + 
      geom_vline(xintercept = 1, 
                 linetype = 'dashed') + 
      geom_errorbarh(aes(xmin = lower_ci, 
                         xmax = upper_ci), 
                     height = 0) + 
      geom_point(size = 2, 
                 shape = 16) + 
      geom_text(aes(label = est_lab), 
                size = 2.5, 
                hjust = 0.5, 
                vjust = -0.8) + 
      scale_color_manual(values = c('negative' = 'steelblue', 
                                    'ns' = 'gray50', 
                                    'positive' = 'coral3')) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            strip.placement = 'outside') + 
      labs(title = plot_title, 
           subtitle = plot_subtitle,
           tag = plot_tag, 
           x = x_lab) +
      scale_x_continuous(trans = x_trans) + 
      guides(color = FALSE)
    
    
  }

# inter-rater -----
  
  rater_tbl <- function(data, ...) {
    
    data %>% 
      dlply(.(visit)) %>% 
      map(select, ...) %>% 
      map(~filter(.x, complete.cases(.x)))
    
  }
  
  Kappa_pipe <- function(table) {
    
    ## calculates unweighted kappa
    
    kappa_obj <- Kappa(table)
    
    kappa_ci <- confint(kappa_obj)

    tibble(kappa = kappa_obj$Unweighted['value'], 
           se = kappa_obj$Unweighted['ASE'], 
           lower_ci = kappa_ci[1, 1], 
           upper_ci = kappa_ci[1, 2]) %>% 
      mutate(plot_lab = paste0(signif(kappa, 2), 
                               ' [', signif(lower_ci, 2), 
                               ' - ', signif(upper_ci, 2), ']'))
        
  }
  
  plot_kappa <- function(data, 
                         plot_title = NULL, 
                         plot_subtitle = NULL, 
                         plot_tag = NULL) {
    
    data %>% 
      ggplot(aes(x = kappa, 
                 y = visit, 
                 color = visit)) + 
      geom_errorbarh(aes(xmin = lower_ci, 
                         xmax = upper_ci), 
                     height = 0) +
      geom_point(shape = 16, 
                 size = 2) + 
      geom_text(aes(label = plot_lab), 
                size = 2.75, 
                hjust = 0.7, 
                vjust = -0.7) + 
      scale_color_manual(values = globals$visit_colors, 
                         labels = globals$visit_labels, 
                         name = 'Follow-up') + 
      scale_y_discrete(labels = globals$visit_labels) + 
      globals$common_theme +
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = expression(kappa), 
           y = 'Follow-up')
    
  }
  
  plot_roc <- function(data, 
                       d_var, 
                       m_var, 
                       roc_stats,
                       plot_title = NULL, 
                       plot_subtitle = NULL, 
                       plot_tag = NULL) {
    
    ## plots a customized ROC plot
    
    data %>% 
      ggplot(aes(d = as.numeric(.data[[d_var]]) - 1, 
                 m = as.numeric(.data[[m_var]]) - 1, 
                 color = visit)) + 
      geom_roc(labels = FALSE) + 
      style_roc() + 
      geom_abline(intercept = 0, 
                  slope = 1, 
                  color = 'gray60') + 
      geom_text(data = roc_stats, 
                aes(x = x_annotation, 
                    y = y_annotation, 
                    label = plot_annotation), 
                size = 2.75, 
                hjust = 0, 
                vjust = 1, 
                show.legend = FALSE) + 
      scale_color_manual(values = globals$visit_colors, 
                         labels = globals$visit_labels, 
                         name = 'Follow-up') + 
      theme(axis.text = globals$common_text, 
            axis.title = globals$common_text, 
            plot.subtitle = globals$common_text, 
            plot.tag.position = 'bottom', 
            plot.tag = globals$common_text, 
            plot.margin = globals$common_margin, 
            legend.title = globals$common_text, 
            legend.text = globals$common_text, 
            plot.title = element_text(size = 8, face = 'bold')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag)
    
  }
  
  interrater <- function(data, 
                         d_var = 'ggo', 
                         m_var = 'opacity', 
                         plot_title = 'GGO', 
                         plot_subtitle = 'GGO versus automated opacity') {
    
    
    ## calculates and visualizes Cohen's kappa and ROC
    
    res <- list()
    
    ## table objects
    
    res$tables <- data %>% 
      rater_tbl(!!ensym(d_var), !!ensym(m_var)) %>% 
      map(~table(.x[c(d_var, m_var)]))
    
    ## n numbers
    
    res$n_numbers <- map2(map(res$tables, sum), 
                          map(res$tables, ~sum(.x[2, ])), 
                          ~paste0('total: n = ', .x, ', events: n = ', .y))
    
    res$n_tag <- map2_chr(globals$visit_labels, 
                          res$n_numbers, 
                          paste, sep = '\n')
    
    ## Kappa calculation
    
    res$kappa <- res$tables %>% 
      map(Kappa_pipe) %>% 
      map2_dfr(., names(.), ~mutate(.x, visit = .y)) %>% 
      mutate(visit = factor(visit, levels = rev(visit)))
    
    ## ROC
    
    res$cutpoint <- data %>% 
      rater_tbl(!!ensym(d_var), !!ensym(m_var)) %>% 
      map(~map_dfc(.x, ~as.numeric(.x) - 1)) %>% 
      map(find_optimal_cutoff, 
          status_variable = d_var, 
          marker = m_var)
    
    res$roc_stats <- res$cutpoint %>% 
      map(get_optimal_cutpoint_coefs) %>% 
      map(unlist, recursive = FALSE) %>% 
      map(~.x[2:3]) %>%
      as_tibble %>% 
      t %>% 
      as.data.frame %>% 
      set_names(c('Se', 'Sp')) %>% 
      rownames_to_column('visit')
    
    res$roc_stats <- res$cutpoint %>% 
      map_dfr(get_auc) %>% 
      cbind(res$roc_stats, .) %>% 
      as_tibble %>% 
      mutate(plot_annotation = paste0('Se = ', signif(Se, 2), 
                                      ', Sp = ', signif(Sp, 2), 
                                      ', AUC = ', signif(AUC, 2), 
                                      ' [', signif(lowerCI, 2), 
                                      ' - ', signif(upperCI, 2), ']'), 
             y_annotation = c(0.22, 0.16, 0.1, 0.04), 
             x_annotation = 0.26, 
             !!ensym(d_var) := 'yes', 
             !!ensym(m_var) := 'yes')
    
    ## kappa and ROC plots
    
    res$kappa_plot <- plot_kappa(data = res$kappa, 
                                 plot_title = plot_title, 
                                 plot_subtitle = plot_subtitle) + 
      scale_x_continuous(limits = c(0, 1))
    
    res$roc_plot <- plot_roc(data = data, 
                             d_var = d_var, 
                             m_var = m_var, 
                             roc_stats = res$roc_stats, 
                             plot_title = plot_title, 
                             plot_subtitle = plot_subtitle, 
                             plot_tag = paste(res$n_tag, collapse = '\n'))
    
    return(res)
    
  }

# GAM modeling -----
  
  plot_gam_predictions <- function(lm_analysis_object) {
    
    preds <- resid(lm_analysis_object, type.predict = 'response')
    
    mod_resp <- names(preds)[1]
    
    mod_vars <- names(preds)[!stri_detect(names(preds), fixed = '.')]
    
    mod_vars <- mod_vars[mod_vars != mod_resp]
    
    preds <- preds %>% 
      select(.fitted, 
             .lower_ci.fit, 
             .upper_ci.fit, 
             all_of(c(mod_resp, mod_vars))) %>% 
      mutate(!!mod_resp := if(is.factor(.data[[mod_resp]])) as.numeric(.data[[mod_resp]]) - 1 else .data[[mod_resp]])
    
    plots <- mod_vars %>% 
      map(~ggplot(data = preds, 
                  aes(x = .data[[.x]], 
                      y = .data[[mod_resp]])) + 
            geom_point(shape = 21, 
                       size = 2, 
                       fill = 'steelblue', 
                       alpha = 0.5, 
                       position = position_jitter(0.1, 0.02)) + 
            geom_ribbon(aes(ymin = .lower_ci.fit, 
                            ymax = .upper_ci.fit), 
                        fill = 'gray60', 
                        alpha = 0.25) + 
            geom_line(aes(y = .fitted), 
                      color = 'firebrick') + 
            globals$common_theme)
    
    plots %>% 
      set_names(mod_vars)
    
    
  }
  
  
# END -----