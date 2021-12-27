# This is a container script for the project-specific tools

  require(plyr)
  require(tidyverse)
  require(rlang)
  require(stringi)
  require(foreign)

  
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
             significance = ifelse(.data[[signif_var]] < 0.05, 
                                   paste('p =', signif(p_value, 2)), 
                                   paste0('ns (p = ', signif(p_value, 2), ')')), 
             label = translate_var(variable), 
             test = ifelse(class == 'factor', 'Chi squared', 'one-way ANOVA')) %>% 
      map_dfc(function(x) if(is.character(x)) stri_replace(x, regex = '^.*\\nyes:\\s{1}', replacement = '') else x)
    
  }
  
# kinetic plotting -----
  
  plot_ctss <- function(data, 
                        plot_title = NULL, 
                        plot_subtitle = NULL, 
                        fill_color = 'firebrick') {
    
    
    ## kinetic line plot
    
    ## median table
    
    med_tbl <- data %>% 
      dlply(.(visit)) %>% 
      map_dfr(analyze_feature, 
              variable = 'ctss') %>% 
      mutate(time_months = c(2, 3, 6, 12))
    
    ## plot
    
    data %>% 
      ggplot(aes(x = time_months, 
                 y = ctss)) + 
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
           y = 'CTSS')
    
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
                              hoffset = 0.15) {
    
    ## adds the results of sequential post-hoc testing to the ctss plot
    
    cmm_rocode <- "'fup1' = 2; 'fup2' = 3; 'fup3' = 6; 'fup4' = 12"
    
    y <- c(21, 19, 17)
    
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
             p_lab = ifelse(.data[[p_var]] < 0.05, 
                            paste('p =', signif(.data[[p_var]], 2)), 
                            paste0('ns (p = ', signif(.data[[p_var]], 2), ')')))
    
    plot + 
      scale_y_continuous(limits = NULL) + 
      geom_segment(data = post_hoc_results, 
                   aes(x = x0, 
                       xend = x1, 
                       y = y0, 
                       yend = y1)) + 
      geom_text(data = post_hoc_results, 
                aes(x = x_text, 
                    y = y1 + 0.25, 
                    label = p_lab), 
                size = 2.75, 
                hjust = 0.3, 
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
             var_lab = ifelse(variable == 'baseline', 'Baseline', var_lab),
             var_lab = factor(var_lab, levels = unique(c('Baseline', unique(var_lab)))), 
             xax_lab = ifelse(level != 'baseline', 
                              paste0(level_fct, ', n = ', n_level), 
                              paste0(level_fct, ' (baseline), n = ', n_level)), 
             xax_lab = ifelse(variable == 'baseline', 'baseline', xax_lab), 
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
  

# END -----