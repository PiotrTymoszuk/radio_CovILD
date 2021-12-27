# This script generates paper tables

 insert_head()
 
# container list ----
 
 paper_tables <- list()
 
# Table 1: Study variables -----
 
 insert_msg('Table 1: study variables')
 
 paper_tables$study_vars <- radio$vars %>% 
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
 
# Table 2: baseline cohort characteristic -----
 
 insert_msg('Table 2: baseline cohort characteristic')
 
 paper_tables$baseline_cohort <- eda$baseline$summary_cohort %>% 
   select(label, statistic) %>% 
   set_names(c('Variable', 'Statistic'))
 
# Table 3: baseline characteristic, severity groups -----
 
 insert_msg('Table 3: baseline characteristic, severity groups')
 
 paper_tables$baseline_severity <- eda$baseline$summary_severity %>% 
   select(label, 
          mild, 
          moderate, 
          severe, 
          critical, 
          significance) %>% 
  set_names(c('Variable', 
              'Mild', 
              'Moderate',
              'Severe', 
              'Critical', 
              'Significance'))
 
# Table 4: CT abnormalities at the visits, whole cohort ----
 
 insert_msg('Table 4: CT abnormalities, whole cohort, consecutive visits')
 
 paper_tables$ct_cohort <- eda$ct$summary_cohort %>% 
   select(label, 
          starts_with('fup'), 
          significance) %>% 
   set_names(c('Variable', 
               '60-day FUP', 
               '100-day FUP', 
               '180-day FUP', 
               '360-day FUP', 
               'Significance'))
 
# Table 6: CT abnormalities at the visits, severity ----
 
 insert_msg('Table 5: CT abnormalities, severity groups')
 
 paper_tables$ct_visits <- eda$ct$summary_visit %>% 
   select(severity, 
          label, 
          starts_with('fup'), 
          significance) %>% 
   set_names(c('Severity', 
               'Variable', 
               '60-day FUP', 
               '100-day FUP', 
               '180-day FUP', 
               '360-day FUP', 
               'Significance'))
 
# Table 6: CT abnormalities between the severity groups -----
 
 insert_msg('Table 6: CT abnormalities, visits, severity groups')
 
 paper_tables$ct_severity <- eda$ct$summary_severity %>% 
   select(visit, 
          label, 
          mild,
          moderate,
          severe, 
          critical, 
          significance) %>% 
   mutate(visit = car::recode(visit, "'fup1' = '60-day'; 'fup2' = '100-day'; 'fup3' = '180-day'; 'fup4' = '360-day'")) %>% 
   set_names(c('Visit', 
               'Variable', 
               'Mild', 
               'Moderate', 
               'Severe', 
               'Critical', 
               'Significance'))
 
# Table 7: CTSS -----
 
 insert_msg('Table 7: CTSS at the consecutive visits')
 
 paper_tables$ctss <- eda$ctss$summary %>% 
   select(subset, 
          starts_with('fup'), 
          significance) %>% 
   set_names(c('Severity',  
               '60-day FUP', 
               '100-day FUP', 
               '180-day FUP', 
               '360-day FUP', 
               'Significance'))
 
# Saving the tables as an excel file ----
 
 insert_msg('Saving the tables as an Excel files')
 
 paper_tables %>% 
   set_names(paste('Table', 1:length(paper_tables))) %>% 
   write_xlsx('./paper/tables.xlsx')
 
# END -----
 
 insert_tail()