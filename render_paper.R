# This script renders the paper tables and figures

  insert_head()
  
# tools ----
  
  library(writexl)
  library(figur) ## available from https://github.com/PiotrTymoszuk/figur
  library(soucer) ## available from https://github.com/PiotrTymoszuk/soucer
  library(cowplot)
  library(rmarkdown)
  library(knitr)
  library(flextable)
  library(bookdown)
  
  c('./tools/project_tools.R', 
    './tools/project_globals.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# paper scripts -----
  
  insert_msg('Sourcing the paper scripts')
  
  c('./paper scripts/paper_tables.R', 
    './paper scripts/paper_figures.R', 
    './paper scripts/deploy_paper.R') %>% 
    walk(source)
  
# END -----
  
  insert_tail()