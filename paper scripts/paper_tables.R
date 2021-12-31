# This script generates paper tables

 insert_head()
 
# container list ----
 
 paper_tables <- list()
 suppl_tables <- list()
 
# Table 1: baseline cohort characteristic -----
 
 insert_msg('Table 1: baseline cohort characteristic')
 
 paper_tables$baseline_cohort <- eda$baseline$summary_cohort %>% 
    select(label, statistic) %>% 
    map_dfc(stri_replace, regex = '\\ncomplete.*', replacement = '') %>% 
    set_names(c('Variable', paste('Statistic, n =', nrow(radio$clear))))
 
# Table 2: CT abnormalities at the visits, whole cohort ----
 
 insert_msg('Table 2: CT abnormalities, whole cohort, consecutive visits')
 
 paper_tables$ct_cohort <- eda$ct$summary_cohort %>% 
    select(label, 
           starts_with('fup'), 
           significance) %>% 
    set_names(c('Variable', 
                '2-month FUP', 
                '3-month FUP', 
                '6-month FUP', 
                '1-year FUP', 
                'Significance'))
 
# Table S1: Study variables -----
 
 insert_msg('Table S1: study variables')
 
 suppl_tables$study_vars <- radio$vars %>% 
    select(old_var, 
           new_var, 
           label, 
           description, 
           modeling) %>% 
    mutate(modeling = car::recode(modeling, "'no' = 'not used in modeling'; 'long_outcome' = 'outcome'")) %>% 
    set_names(c('SPSS variable name', 
                'R variable name', 
                'R variable label', 
                'Description', 
                'Variable type'))
 
# Table S2: baseline characteristic, severity groups -----
 
 insert_msg('Table S2: baseline characteristic, severity groups')
 
 suppl_tables$baseline_severity <- eda$baseline$summary_severity %>% 
    select(label, 
           mild, 
           moderate, 
           severe, 
           critical, 
           significance) %>% 
    map_dfc(stri_replace, regex = '\\ncomplete.*', replacement = '') %>% 
    set_names(c('Variable', 
                map2_chr(c('Mild', 'Moderate', 'Severe', 'Critical'), 
                         map_chr(eda$baseline$analyses_severity$age$stat_tables, ~.$n_complete), 
                         ~paste0(.x, ', n = ', .y)), 
                'Significance'))
 
# Table S3: CT abnormalities at the visits, severity ----
 
 insert_msg('Table S3: CT abnormalities, severity groups')
 
 suppl_tables$ct_visits <- eda$ct$summary_visit %>% 
    select(severity, 
           label, 
           starts_with('fup'), 
           significance) %>% 
    set_names(c('Severity', 
                'Variable', 
                '2-month FUP', 
                '3-month FUP', 
                '6-month FUP', 
                '1-year FUP', 
                'Significance'))
 
# Table S4: CTSS -----
 
 insert_msg('Table S4: CTSS at the consecutive visits')
 
 suppl_tables$ctss <- eda$numeric$summary$ctss %>% 
    map_dfc(stri_replace, regex = 'mean\\(SD\\).*\\n', replacement = '') %>% 
    select(subset, 
           starts_with('fup'), 
           significance) %>% 
    set_names(c('Severity',  
                '2-month FUP', 
                '3-month FUP', 
                '6-month FUP', 
                '1-year FUP', 
                'Significance'))
 
# Table S5: percent opacity and high opacity -----
 
 insert_msg('Table S5: Percent opacity and high opacity')
 
 suppl_tables$opacity <- eda$numeric$summary[c('perc_opac', 'perc_hiopac')] %>% 
    map2_dfr(., c('Opacity, % lung volume', 
                  'High opacity, % lung volume'),  
             ~mutate(.x, variable = .y)) %>% 
    map_dfc(stri_replace, regex = 'mean\\(SD\\).*\\n', replacement = '') %>% 
    select(variable, 
           subset, 
           starts_with('fup'), 
           significance) %>% 
    set_names(c('Variable', 
                'Severity',  
                '2-month FUP', 
                '3-month FUP', 
                '6-month FUP', 
                '1-year FUP', 
                'Significance'))
 
# Saving the tables as excel files ----
 
 insert_msg('Saving the tables as an Excel files')
 
 paper_tables %>% 
   set_names(paste('Table', 1:length(paper_tables))) %>% 
   write_xlsx('./paper/tables.xlsx')
 
 suppl_tables %>% 
    set_names(paste0('Table S', 1:length(suppl_tables))) %>% 
    write_xlsx('./paper/supplementary_tables.xlsx')
 
# END -----
 
 insert_tail()