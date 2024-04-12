#!/bin/sh

R --slave --silent <<RSCRIPT
rmarkdown::render(
    input = '00-SB_mboissel.Rmd', 
    output_file = '00-SB_mboissel.html', 
    output_format = "bookdown::html_document2", 
    output_dir = '.',
    encoding = 'UTF-8',
    params = list(dpi = 120, ggFontSize=6, theme_dark = TRUE)
)

RSCRIPT

