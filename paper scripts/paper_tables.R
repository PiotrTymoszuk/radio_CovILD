# This script generates paper tables

 insert_head()
 
# container list ----
 
 paper_tables <- list()
 suppl_tables <- list()
 
# Table 1: baseline cohort characteristic -----
 
 insert_msg('Table 1: baseline cohort characteristic')
 
 paper_tables$baseline_cohort <- ed_analysis$baseline %>% 
    mutate(variable = translate_var(variable)) %>% 
    map_dfc(stri_replace, regex = '\\nComplete.*', replacement = '') %>% 
    set_names(c('Variable', paste('Statistic, n =', nrow(radio$clear))))
 
# Table 2: CT abnormalities at the visits, whole cohort ----
 
 insert_msg('Table 2: CT abnormalities, whole cohort, consecutive visits')
 
 paper_tables$ct_cohort <- ed_analysis$ct %>% 
    filter(!variable %in% c('opacity', 'hiopacity', 'ards')) %>% 
    mutate(variable = translate_var(variable)) %>% 
    map_dfc(stri_replace, regex = 'no.*\\nyes:\\s{1}', replacement = '') %>% 
    select(-test) %>% 
    set_names(c('Variable', 
                '2-month FUP', 
                '3-month FUP', 
                '6-month FUP', 
                '1-year FUP', 
                'Significance', 
                'Effect size'))
 
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
 
 suppl_tables$baseline_severity <- ed_analysis$severity %>% 
    mutate(variable = translate_var(variable)) %>% 
    set_names(c('Variable', 
                'Mild', 
                'Moderate', 
                'Severe', 
                'Critical', 
                'Test', 
                'Significance', 
                'Effect size'))
 
# Table S3: CTSS -----
 
 insert_msg('Table S3: CTSS at the consecutive visits')
 
 suppl_tables$ctss <- ed_analysis$ct_scoring %>% 
    mutate(variable = translate_var(variable)) %>% 
    select(- test) %>% 
    map_dfc(stri_replace, regex = 'mean\\(SD\\).*\\n', replacement = '') %>% 
    set_names(c('Severity',  
                '2-month FUP', 
                '3-month FUP', 
                '6-month FUP', 
                '1-year FUP', 
                'Significance', 
                'Effect size'))

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