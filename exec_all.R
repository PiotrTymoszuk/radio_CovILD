# This program executes the project scripts

  library(soucer)

# listing scripts ----

  exec_log <- source_all(c('data_import.R', 
                           'data_analysis.R', 
                           'render_paper.R'), 
                         message = TRUE, crash = FALSE)
  
  print(exec_log)

  'render_paper.R'
  
  save.image()
  
# END ----