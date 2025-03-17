# indycon-indi

Analysis scripts and data for the indicon-indi project which is looking at whether there are individual and claim type differences in sensitivity to consensus effects. 

To install all required R packages, type `renv::restore()` into the Rstudio console. 

- The ``power-simulation`` folder includes scripts for the power simulation which we used to justify the number of trials per participant. 

- The ``data`` folder contains all of the anonymised raw data (in json format), clean data (in .Rdata and .csv formamt), and derived data (various transformations of the clean data). 

- The ``analysis`` folder provides all of the scripts (mainly R, Rmarkdown, or Quarto scripts) used to run the analyses. 

## Requirements

 R 4.2.2 and RTools42 are necessary in order to reproduce these results (older versions of these may work, but this is what we used). 
 
 Some form of LaTeX needs to be installed to knit the Quarto/Markdown to PDF. If you cannot install latex, you can change the default output to html. 