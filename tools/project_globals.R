# This script sets up graphical and variable globals for the project

  require(plyr)
  require(tidyverse)
  
  insert_head()
  
# data container ----
  
  globals <- list()
  
# Graphics -----
  
  insert_msg('Graphics globals')
  
  globals$common_text <- element_text(size = 8, face = 'plain', color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 8, 
                                                                            face = 'bold', 
                                                                            color = 'black', 
                                                                            hjust = 0), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95', color = 'gray80'), 
                                                  plot.margin = globals$common_margin, 
                                                  panel.grid.major = element_line(color = 'gray90'))
  
# Variable representation -----
  
  insert_msg('Variable representation')

  globals$subset_colors <- c('cohort' = 'plum3', 
                             'mild' = 'steelblue', 
                             'moderate' = 'cornsilk4', 
                             'severe' = 'coral3', 
                             'critical' = 'firebrick3')  
  
  globals$subset_labels <- c(cohort = 'All participants', 
                             mild = 'Mild COVID-19', 
                             moderate = 'Moderate COVID-19', 
                             severe = 'Severe COVID-19', 
                             critical = 'Critical COVID-19')
  
  globals$visit_labels <- c('fup1' = '2-month', 
                            'fup2' = '3-month', 
                            'fup3' = '6-month', 
                            'fup4' = '1-year')
  
  globals$visit_colors <- c('fup1' = 'firebrick3', 
                            'fup2' = 'cornsilk4', 
                            'fup3' = 'darkolivegreen4', 
                            'fup4' = 'steelblue')
  
# END -----
  
