# This script performs data analysis 

  insert_head()
  
# tools -----
  
  library(soucer) ## available from https://github.com/PiotrTymoszuk/soucer
  library(DescTools)
  library(rstatix)
  library(ggpubr)
  library(furrr)
  library(caret)
  library(doParallel)
  library(vcd)
  library(MASS)
  library(broom)
  library(mgcv)
  library(exda) ## available from https://github.com/PiotrTymoszuk/ExDA
  library(lmqc) ## available from https://github.com/PiotrTymoszuk/lmqc
  library(caretExtra) ## available from https://github.com/PiotrTymoszuk/caretExtra
  
  select <- dplyr::select
  
  c('./tools/roc_toolbox.R', 
    './tools/project_globals.R', 
    './tools/project_tools.R') %>% 
  source_all(message = TRUE, crash = TRUE)
  
# analysis scripts -----
  
  c('./analysis scripts/distribution.R', ## distribution checks
     './analysis scripts/eda.R', ## explorative data analysis
     './analysis scripts/kinetic.R', ## kinetic analysis
    './analysis scripts/interrater.R', ## inter-rater comparison of the CTSS and opacity
    './analysis scripts/risk_modeling.R', ## abnormality risk modeling
    './analysis scripts/score_modeling.R', ## CTSS modeling
    './analysis scripts/risk_modeling_gam.R') %>% ## GAM modeling and comparison with the age-cutoff models
    source_all(message = TRUE, crash = TRUE) 

# END -----
  
  insert_tail()