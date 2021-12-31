# This script renders the legends for tables and figures ----

# data and tools ----

  source('./tools/sys_tools.R')

  library(knitr)
  library(bookdown)
  library(kableExtra)
  library(rmarkdown)
  library(flextable)

  insert_head()
  
# Specific functions -----
  
  mm_inch <- function(input_mm) {
    
    return(0.0393700787 * input_mm)
    
  }

# Generating figure legend file -----
  
  insert_msg('Rendering figure and table legend file')
  
  render('./paper/markdown/figures.Rmd', 
         output_format = word_document2(number_sections = F, theme = 'readable'), 
         output_dir = './paper/')
  
  render('./paper/markdown/supplementary_material.Rmd', 
         output_format = word_document2(number_sections = F, theme = 'readable'), 
         output_dir = './paper/')


# END -----
  
  insert_tail()

