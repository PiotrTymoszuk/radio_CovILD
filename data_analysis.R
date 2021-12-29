# This script performs data analysis 

  insert_head()
  
# tools -----
  
  c('./tools/sys_tools.R', 
    './tools/project_globals.R', 
    './tools/counting_tools.R', 
    './tools/modeling_tools.R', 
    './tools/lm_qc_tools.R', 
    './tools/roc_toolbox.R', 
    './tools/project_tools.R') %>% 
    walk(source)
  
  library(DescTools)
  library(rstatix)
  library(ggpubr)
  library(furrr)
  library(caret)
  library(vcd)
  
# analysis scripts -----
  
  c('./analysis scripts/distribution.R', ## distribution checks
    './analysis scripts/eda.R', ## explorative data analysis
    './analysis scripts/kinetic.R', ## kinetic analysis
    './analysis scripts/risk_score.R', ## risk and score modeling
    './analysis scripts/interrater.R') %>% ## inter-rater comparison of the CTSS and opacity
    walk(source)
  
# END -----
  
  insert_tail()