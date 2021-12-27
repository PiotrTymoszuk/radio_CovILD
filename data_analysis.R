# This script performs data analysis 

  insert_head()
  
# tools -----
  
  c('./tools/sys_tools.R', 
    './tools/project_tools.R', 
    './tools/project_globals.R', 
    './tools/counting_tools.R', 
    './tools/modeling_tools.R', 
    './tools/lm_qc_tools.R') %>% 
    walk(source)
  
  library(DescTools)
  library(rstatix)
  library(ggpubr)
  library(furrr)
  library(caret)
  
# analysis scripts -----
  
  c('./analysis scripts/eda.R', 
    './analysis scripts/kinetic.R', 
    './analysis scripts/risk_score.R') %>% 
    walk(source)
  
# END -----
  
  insert_tail()