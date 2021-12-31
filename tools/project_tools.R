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
  
# table formatting ------
  
  add_stat_info <- function(data_frame, signif_var = c('p_value', 'p_adj')) {
    
    ## adds information on tests and adjusts p values
    
    signif_var <- match.arg(signif_var[1], c('p_value', 'p_adj'))
    
    data_frame %>% 
      mutate(p_adj = p.adjust(p_value, 'BH'), 
             significance = format_p(.data[[signif_var]]), 
             label = translate_var(variable), 
             test = ifelse(class == 'factor', 'Chi squared', 'one-way ANOVA')) %>% 
      map_dfc(function(x) if(is.character(x)) stri_replace(x, regex = '^.*\\nyes:\\s{1}', replacement = '') else x)
    
  }
  
# kinetic modeling and plotting -----
  
  compare_effects <- function(data, 
                              response = 'ctss', 
                              indep_var = 'visit', 
                              group_var = 'ID',
                              invert = FALSE, 
                              parallel = FALSE, 
                              ci.type = 'norm') {
    
    ## pairwise effect size
    
    start_time <- Sys.time()
    on.exit(message(paste('Elapsed:', Sys.time() - start_time)))
    
    pairs <- combn(unique(data[[indep_var]]), m = 2, simplify = FALSE)
    
    group_tbl <- pairs %>% 
      map_dfr(~tibble(.y = response, 
                      group1 = .x[1], 
                      group2 = .x[2]))
    
    mod_tbl <- pairs %>% 
      map(~filter(data, .data[[indep_var]] %in% .x)) %>% 
      map(arrange, .data[[group_var]]) %>% 
      map(mutate, 
          x_fct = droplevels(.data[[indep_var]]))
    
    N <- mod_tbl %>% 
      map(filter, !duplicated(.data[[group_var]])) %>% 
      map_dbl(nrow)
    
    ## pairwise effect size calculation
  
    my_kendall <- function(x, y) {
      
      kendall_res <- try(KendallTauB(x, y, conf.level = 0.95), silent = TRUE)
      
      if(any(class(kendall_res) == 'try-error')) {
        
        warning('The factor value is invariant!', call. = FALSE)
        
        kendall_res <- c(tau_b = 0, lwr.ci = NA, upr.ci = NA)
        
      }
      
      return(kendall_res)
      
    }
    
    my_freeman <- function(tbl) {
      
      freeman <- try(freemanTheta(tbl, 
                                  ci = TRUE, 
                                  type = ci.type, 
                                  R = 1000), 
                     silent = TRUE)
      
      if(any(class(freeman) == 'try-error')) {
        
        warning('The factor value is invariant!', call. = FALSE)
        
        freeman <- tibble(Freeman.theta = 0, 
                          lower.ci = NA, 
                          upper.ci = NA)  
      }
      
      return(freeman)
      
    }
    
    kendall_tau <- mod_tbl %>% 
      map(~my_kendall(x = as.numeric(.x[[response]]), 
                      y = as.numeric(.x[[indep_var]]))) %>% 
            map_dfr(~tibble(tau_b = .x[1], 
                            tau_lower_ci = .x[2], 
                            tau_upper_ci = .x[3]))
    
    if(invert) {
      
      kendall_tau <- kendall_tau %>% 
        mutate(tau_b = -tau_b, 
               trans_lower = tau_lower_ci, 
               trans_upper = tau_upper_ci, 
               tau_lower_ci = -trans_upper, 
               tau_upper_ci = -trans_lower) %>% 
        select( - trans_lower, - trans_upper)
      
    }
    
    if(is.numeric(data[[response]])) {
      
      wilcox_z <- mod_tbl %>% 
        map(~dlply(.x, indep_var) %>% 
              map(~.x[c(response, group_var)]) %>% 
              reduce(left_join, by = group_var) %>% 
              select(starts_with(response))) %>% 
        map_dbl(~wilcoxonZ(.x[[2]], .x[[1]], paired = TRUE)) %>% 
        tibble(z = ., 
               n = N, 
               r = ./sqrt(N))
      
      if(invert) {
        
        wilcox_z <- wilcox_z %>% 
          mutate(z = -z, 
                 r = -r)
        
      }
      
      ## output
      
      list(group_tbl, 
           kendall_tau, 
           wilcox_z) %>% 
        reduce(cbind) %>% 
        as_tibble
      
    } else {
      
      if(parallel) {
        
        plan('multisession')
        
        theta <- mod_tbl %>% 
          map(~table(.x[[response]], .x[['x_fct']])) %>% 
          future_map_dfr(my_freeman, 
                         .options = furrr_options(seed = TRUE)) %>% 
          set_names(c('theta', 'theta_lower_ci', 'theta_upper_ci'))
        
        plan('sequential')
        
      } else {
        
        theta <- mod_tbl %>% 
          map(~table(.x[[response]], .x[['x_fct']])) %>% 
          map_dfr(my_freeman) %>% 
          set_names(c('theta', 'theta_lower_ci', 'theta_upper_ci'))
        
      }

      ## output
      
      list(group_tbl, 
           kendall_tau, 
           theta) %>% 
        reduce(cbind) %>% 
        as_tibble
  
    }
    
  }
  
  plot_effects <- function(eff_size_data, 
                           eff_var = c('r', 'tau_b', 'theta'), 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           plot_tag = NULL, 
                           x_lab = 'Months post COVID-19', 
                           y_lab = 'Recovery effect, R', 
                           ci = TRUE, 
                           dodge_w = 0) {
    
    ## plots effect size at the consecutive visits
    
    eff_var <- match.arg(eff_var[1], c('r', 'tau_b', 'theta'))
    
    plotting_tbl <- eff_size_data %>% 
      mutate(comparison = paste(group1, group2, sep = '-'), 
             severity = factor(severity, names(globals$subset_colors))) %>% 
      filter(comparison %in% c('fup1-fup2', 'fup2-fup3', 'fup3-fup4')) %>% 
      mutate(time = car::recode(comparison, "'fup1-fup2' = '3 vs 2'; 'fup2-fup3' = '6 vs 3'; 'fup3-fup4' = '12 vs 6'"), 
             time = factor(time, c('3 vs 2', '6 vs 3', '12 vs 6')))
    
    if(ci) {
      
      base_plot <- switch(eff_var, 
                          r = ggplot(plotting_tbl, 
                                     aes(x = time, 
                                         y = r, 
                                         color = severity) + 
                                       geom_hline(yintercept = 0, 
                                                  linetype = 'dashed')), 
                          tau_b = ggplot(plotting_tbl, 
                                         aes(x = time, 
                                             y = tau_b, 
                                             color = severity)) + 
                            geom_hline(yintercept = 0, 
                                       linetype = 'dashed') + 
                            geom_errorbar(aes(ymin = tau_lower_ci, 
                                              ymax = tau_upper_ci), 
                                          width = 0, 
                                          position = position_dodge(dodge_w)), 
                          theta = ggplot(plotting_tbl, 
                                         aes(x = time, 
                                             y = theta, 
                                             color = severity)) + 
                            geom_hline(yintercept = 0, 
                                       linetype = 'dashed') + 
                            geom_errorbar(aes(ymin = theta_lower_ci, 
                                              ymax = theta_upper_ci), 
                                          width = 0, 
                                          position = position_dodge(dodge_w)))
      
    } else {
      
      base_plot <- switch(eff_var, 
                          r = ggplot(plotting_tbl, 
                                     aes(x = time, 
                                         y = r, 
                                         color = severity)), 
                          tau_b = ggplot(plotting_tbl, 
                                         aes(x = time, 
                                             y = tau_b, 
                                             color = severity)), 
                          theta = ggplot(plotting_tbl, 
                                         aes(x = time, 
                                             y = theta, 
                                             color = severity)))
      base_plot <- base_plot + 
        geom_hline(yintercept = 0, 
                   linetype = 'dashed')
      
    }
    
    base_plot + 
      geom_line(aes(group = severity), 
                position = position_dodge(dodge_w)) + 
      geom_point(shape = 16, 
                 size = 2, 
                 position = position_dodge(dodge_w)) + 
      scale_color_manual(values = globals$subset_colors, 
                         labels = globals$subset_labels, 
                         name = '') + 
      globals$common_theme +
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
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
      map_dfr(analyze_feature, 
              variable = plot_var) %>% 
      mutate(time_months = c(2, 3, 6, 12))
    
    ## plot
    
    data %>% 
      ggplot(aes(x = time_months, 
                 y = .data[[plot_var]])) + 
      geom_ribbon(data = med_tbl, 
                  aes(y = median, 
                      ymin = perc25, 
                      ymax = perc75), 
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
           tag = paste('\nn =', med_tbl$n_complete[1]), 
           x = 'Months post COVID-19', 
           y = y_lab)
    
  }
  
  plot_feature_kinetic <- function(data, 
                                   plot_var = 'ggo', 
                                   plot_title = NULL, 
                                   plot_subtitle = NULL, 
                                   fill_color = 'firebrick', 
                                   y_lab = '% subset') {
    
    ## frequency plot
    
    ## percent table
    
    med_tbl <- data %>% 
      dlply(.(visit)) %>% 
      map(count, .data[[plot_var]], .drop = FALSE) %>% 
      map(mutate, 
          total_n = sum(n), 
          percent = 100 * n/sum(n)) %>% 
      map2_dfr(., names(.), ~mutate(.x, visit = .y)) %>% 
      filter(.data[[plot_var]] == 'yes') %>% 
      mutate(time_months = car::recode(visit, "'fup1' = 2; 'fup2' = 3; 'fup3' = 6; 'fup4' = 12", as.factor = FALSE), 
             time_months = as.numeric(time_months), 
             perc_lab = signif(percent, 2))
    
    ## plot
    
    med_tbl %>% 
      ggplot(aes(x = time_months, 
                 y = percent)) + 
      geom_point(shape = 16, 
                 color = fill_color) + 
      geom_line(color = fill_color) + 
      geom_text(aes(label = perc_lab), 
                size = 2.75, 
                hjust = -0.2, 
                vjust = -0.3, 
                color = fill_color) + 
      scale_x_continuous(breaks = c(2, 3, 6, 12)) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = paste('\nn =', med_tbl$total_n[1]), 
           x = 'Months post COVID-19', 
           y = y_lab)
    
    
  }
  
  add_ctss_post_p <- function(plot, 
                              post_hoc_results, 
                              p_var = 'p', 
                              hoffset = 0.15, 
                              line_y = 21, 
                              voffset = 2, 
                              text_offset = 0.35) {
    
    ## adds the results of sequential post-hoc testing to the ctss plot
    
    cmm_rocode <- "'fup1' = 2; 'fup2' = 3; 'fup3' = 6; 'fup4' = 12"
    
    y <- c(line_y, line_y - voffset, line_y - 2*voffset)
    
    post_hoc_results <- post_hoc_results %>% 
      mutate(comparison = paste(group1, group2, sep = ':')) %>% 
      filter(comparison %in% c('fup1:fup2', 'fup2:fup3', 'fup3:fup4')) %>% 
      mutate(x0 = car::recode(group1, cmm_rocode), 
             x1 = car::recode(group2, cmm_rocode), 
             y0 = y, 
             y1 = y, 
             x_text = (x1 + x0)/2, 
             x0 = x0 + hoffset, 
             x1 = x1 - hoffset, 
             p_lab = format_p(.data[[p_var]], 2))
    
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
                    label = p_lab), 
                size = 2.75, 
                hjust = 0.2, 
                vjust = 0)
    
  }
  
# risk modeling -----
  
  get_baseline <- function(data, variable) {
    
    data <- data %>% 
      mutate(!!variable := droplevels(.data[[variable]]))
    
    count(data, .data[[variable]]) %>% 
      set_names(c('level_fct', 'n_level')) %>% 
      filter(!is.na(level_fct)) %>% 
      mutate(level = c('baseline', levels(data[[variable]])[-1]), 
             variable = !!variable, 
             plot_order = as.numeric(level_fct))
    
  }
  
  plot_forest <- function(summary_tbl, 
                          hide_baseline_est = TRUE, 
                          plot_title = NULL, 
                          plot_subtitle = NULL, 
                          plot_tag = NULL, 
                          x_lab = 'OR', 
                          x_trans = 'log2') {
    
    summary_tbl <- summary_tbl %>% 
      mutate(var_lab = translate_var(variable), 
             var_lab = ifelse(variable == 'baseline', 'Ref.', var_lab),
             var_lab = factor(var_lab, levels = unique(c('Ref.', unique(var_lab)))), 
             xax_lab = ifelse(level != 'baseline', 
                              paste0(level_fct, ', n = ', n_level), 
                              paste0(level_fct, ' (ref.), n = ', n_level)), 
             xax_lab = ifelse(variable == 'baseline', 'reference', xax_lab), 
             est_lab = paste0(signif(estimate, 2), ' [', signif(lower_ci, 2), ' - ', signif(upper_ci, 2), ']'), 
             significant = ifelse(p_value < 0.05, 'yes', 'no'), 
             regulation = ifelse(significant == 'no', 'ns', 
                                 ifelse(estimate > 1, 'positive', 'negative')))
    
    if(hide_baseline_est) {
      
      summary_tbl <- summary_tbl %>% 
        mutate(estimate = ifelse(level == 'baseline', 1, estimate), 
               lower_ci = ifelse(level == 'baseline', 1, lower_ci), 
               upper_ci = ifelse(level == 'baseline', 1, upper_ci), 
               est_lab = ifelse(level == 'baseline', 1, est_lab))
      
    }
    
    summary_tbl %>% 
      ggplot(aes(x = estimate, 
                 y = reorder(xax_lab, -plot_order), 
                 color = regulation)) + 
      facet_grid(var_lab ~ ., 
                 scales = 'free', 
                 space = 'free') + 
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
      theme(axis.title.y = element_blank()) + 
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
  
  plot_roc <- function(data, d_var, m_var, 
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
  
  correlation_plot <- function(data, 
                               x_var = 'ctss', 
                               y_var = 'perc_opac', 
                               x_lab = 'CTSS', 
                               y_lab = 'Opacity, % lung', 
                               plot_title = NULL, 
                               plot_subtitle = NULL, 
                               plot_tag = NULL, 
                               fill_color = 'steelblue', 
                               jitter_w = 0, 
                               jitter_h = 0, 
                               point_alpha = 0.75) {
    
    data %>% 
      ggplot(aes(x = .data[[x_var]], 
                 y = .data[[y_var]])) + 
      geom_point(shape = 21, 
                 size = 2, 
                 fill = fill_color, 
                 alpha = point_alpha, 
                 position = position_jitter(width = jitter_w, 
                                            height = jitter_h)) + 
      geom_smooth(method = 'lm') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
  }
  
# number formatting -----
  
  short_number <- function(vec, signif_digits = 2) {
    
    stopifnot(is.numeric(vec))
    
    signif(vec, signif_digits) %>% 
      as.character %>% 
      stri_replace(regex = '^0.', replacement = '.')
    
  }

  format_p <- function(vec, signif_digits = 2) {
    
    stopifnot(is.numeric(vec))
    
    ifelse(vec < 0.001, 
           'p < .001', 
           ifelse(vec >= 0.05, 
                  paste0('ns (p = ', short_number(vec, signif_digits), ')'), 
                  paste('p =', short_number(vec, signif_digits))))
    
  }
    
# END -----