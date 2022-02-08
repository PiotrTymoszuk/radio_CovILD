# Data import from a SPSS file and wrangling

# tools -----

  library(plyr)
  library(tidyverse)
  library(foreign)
  library(stringi)
  library(readxl)
  library(soucer) ## available from https://github.com/PiotrTymoszuk/soucer

  c('./tools/project_globals.R', 
    './tools/project_tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)
      
  insert_head()
  
# container list ----
  
  radio <- list()

# Reading the SPSS datafile -----
  
  insert_msg('Reading the SPSS data file')
  
  radio$raw <- read.spss(file = './data/20211228PILI.sav', 
                          to.data.frame = TRUE) %>% 
    as_tibble %>% 
    mutate(ID = rm_spaces(RAD_ID, 'character'))

  
# Reading the recoding scheme ----
  
  insert_msg('Reading the recoding scheme')
  
  radio$vars <- read_excel('./data/coding_scheme.xlsx') %>% 
    mutate(args = list(args1, args2, args3, args4) %>% 
             pmap(function(a1, a2, a3, a4) list(if(!is.na(a1)) a1 else NULL, 
                                                if(!is.na(a2)) a2 else NULL, 
                                                if(!is.na(a3)) a3 else NULL, 
                                                if(!is.na(a4)) a4 else NULL)), 
           args = map(args, compact))
  
  radio$vars <- left_join(radio$vars, 
                          tibble(old_var = names(attr(radio$raw, 'variable.labels')), 
                                 description = attr(radio$raw, 'variable.labels')), 
                          by = 'old_var')
  
# serial clearing -----
  
  insert_msg('Serial cleaning')
  
  radio$clear <- radio$vars %>% 
    filter(recode == 'yes') %>% 
    select(old_var, new_var, trans_fun, args) %>% 
    pmap(recode_var, 
         data = radio$raw) %>% 
    reduce(left_join, by = 'ID')
  
  difftime_years(vector1 = radio$raw$DOE_1, vector2 = radio$raw$DOB)
  
  radio$raw %>% 
    mutate(age = difftime_years(DOE_1, DOB))
  
# manual re-coding, cutting down to the patients having completed the last visit ----
  
  insert_msg('Manual recoding')
  
  radio$clear <- radio$clear %>% 
    mutate(sex = car::recode(sex, "'m' = 'male'; 'w' = 'female'"), 
           sex = factor(sex, c('female', 'male')), 
           age = date_fup4 - date_birth - 1, 
           age = round(as.numeric(age, format = 'days')/365.25), 
           age_60 = cut(age, c(-Inf, 60, Inf), c('up to 60', '>60')), 
           smoking = ifelse(ex_smoker == 'yes', 'ex', 
                            ifelse(curr_smoker == 'yes', 'active', 'never')), 
           smoking = factor(smoking, c('never', 'ex', 'active')), 
           ctss_any_fup4 = factor(ifelse(ctss_fup4 > 0, 'yes', 'no')), 
           ctss_mod_severe_fup4 = factor(ifelse(ctss_fup4 > 5, 'yes', 'no')), 
           ctss_severe_fup4 = factor(ifelse(ctss_fup4 > 10, 'yes', 'no')), 
           opacity_fup4 = factor(ifelse(perc_opac_fup4 > 0, 'yes', 'no')), 
           hiopacity_fup4 = factor(ifelse(perc_hiopac_fup4 > 0, 'yes', 'no'))) %>% 
    filter(!is.na(date_fup4)) %>% 
    map_dfr(function(x) if(is.factor(x)) droplevels(x) else x)
  
  radio$complete_long_ids <- radio$clear %>% 
    filter(!is.na(date_fup1), 
           !is.na(date_fup2), 
           !is.na(date_fup3)) %>% 
    .$ID
  
# Long format -----
  
  insert_msg('Long format analysis table')
  
  radio$long_outcome_var <- radio$vars %>% 
    filter(modeling == 'long_outcome') %>% 
    arrange(order) %>% 
    .$new_var
  
  radio$long_expl_var <- radio$vars %>% 
    filter(modeling == 'explanatory') %>% 
    arrange(order) %>% 
    .$new_var
  
  radio$long <- radio$long_outcome_var %>% 
    map(~select(radio$clear, 
                ID, 
                all_of(radio$long_expl_var), 
                starts_with(paste0(.x, '_fup')))) %>% 
    map2(., radio$long_outcome_var, 
         ~gather(data = .x, 
                 key = 'visit', 
                 value = !!sym(.y), 
                 starts_with(.y))) %>% 
    map(mutate, 
        visit = stri_extract(visit, regex = 'fup\\d{1}'), 
        visit = factor(visit)) %>% 
    reduce(left_join, by = c('ID', 'visit', radio$long_expl_var)) %>% 
    mutate(time_months = car::recode(visit, "'fup1' = 2; 'fup2' = 3; 'fup3' = 6; 'fup4' = 12", as.factor = FALSE), 
           time_months = as.numeric(time_months))
  
  ## recoding the outcomes as factors

  for(i in radio$long_outcome_var) {
    
    if(is.character(radio$long[[i]])) {
      
      radio$long <- radio$long %>% 
        mutate(!!i := factor(.data[[i]]))
      
    }
    
  }
  
  ## manual edits, coding the severity scores strata as binary variables for logistic modeling
  
  radio$long <- radio$long %>% 
    mutate(ards = factor(ards, c('no', 'yes')), 
           ctss_class = factor(ctss_class, levels = levels(radio$clear$ctss_class_fup1)), 
           ctss_any = factor(ifelse(ctss > 0, 'yes', 'no')), 
           ctss_mod_severe = factor(ifelse(ctss > 5, 'yes', 'no')), 
           ctss_severe = factor(ifelse(ctss > 10, 'yes', 'no')), 
           opacity = factor(ifelse(perc_opac > 0, 'yes', 'no')), 
           hiopacity = factor(ifelse(perc_hiopac > 0, 'yes', 'no')))

# convenience vectors with score and categorical outcome variables ----
  
  insert_msg('Score and categorical outcome variables')
  
  radio$cat_outcome_var <- c('ctss_any', 
                             'ctss_mod_severe', 
                             'ctss_severe', 
                             'opacity', 
                             'hiopacity', 
                             radio$long_outcome_var[!stri_detect(radio$long_outcome_var, regex = 'ctss|opac')])
  
  radio$score_outcome_var <- radio$long_outcome_var[!radio$long_outcome_var %in% radio$cat_outcome_var]
  
# END ---
  
  rm(i)
  
  insert_tail()