# This script renders the legends for tables and figures ----

  insert_head()

# Exporting the figure chunks -----
  
  insert_msg('Figure chunk export')
  
  insert_figure(paper_figures$consort, 
                paper_figures$ct_images, 
                paper_figures$abnormality_risk, 
                paper_figures$ct_images2, 
                paper_figures$ctss_change, 
                suppl_figures$ctss_severity, 
                suppl_figures$ctss_correlation, 
                rev_figures$age_spline, 
                rev_figures$pois_ord_comp, 
                file = './paper/markdown/figure_chunks.Rmd', 
                ref_names = stri_replace_all(c(names(paper_figures), 
                                               names(suppl_figures), 
                                               names(rev_figures)), 
                                             fixed = '_', 
                                             replacement = '-'), 
                captions = c('Study Flow chart.', 
                             'Non-contrast axial and sagittal chest CTs corresponding to the CT severity score.', 
                             'Risk of developing persistent CT abnormalities at the one-year post-COVID-19 follow-up visit.', 
                             'Serial non-contrast axial chest CTs of three study participants with prior COVID-19 pneumonia.', 
                             'Change in CT severity score (CTSS) over time.', 
                             'Predictors of CT abnormality severity at the 1-year follow-up.', 
                             'Correlation of CTSS and lung opacity.', 
                             'Effects of the age variable splining.', 
                             'Comparison of the Poisson and logistic ordinal regression results.'), 
                append = FALSE, 
                add_extern_legend = TRUE)

# Generating figure legend file -----
  
  insert_msg('Rendering figure and table legend file')
  
  render('./paper/markdown/figures.Rmd', 
         output_format = word_document2(number_sections = FALSE, theme = 'readable'), 
         output_dir = './paper/')
  
  render('./paper/markdown/supplementary_material.Rmd', 
         output_format = word_document2(number_sections = FALSE, theme = 'readable'), 
         output_dir = './paper/')
  
  render('./paper/markdown/response_stat_reviewer.Rmd', 
         output_format = word_document2(number_sections = FALSE, theme = 'readable'), 
         output_dir = './paper/')

# END -----
  
  insert_tail()