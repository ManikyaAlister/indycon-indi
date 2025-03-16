This folder contains all of the scripts that were used to run the Bayesian linear models and all follow up analyses. 

The general file structure is as follows: 

- ``00_HPC_scripts``: These are scripts that allow me to run jobs from my university's high performance computing cluster. 
- ``01_models``: This folder contains the R scripts that run the BRMS models. See above for how to run the group-level and individual-level models. 
- ``02_output``: The raw output of the BRMS models. This folder is mostly empty because the output files are too big to store on github. You can download them directly from https://osf.io/mtuyv/. 
- ``03_combine_output_individuals``: A script that loads and combines the output of each individual model. 
- ``04_combine_output_group``: A script that loads and combines the output of each group-level model.
- ``05_analyse_individuals``: A quarto document (and the rendered PDF) that contains the analyses for the individual-level modelling.
- ``06_analyse_group``: A quarto document (and the rendered PDF) that contains the analyses for the group-level modelling.
- ``06.1_follow-up-group-modelling``: Supplementary material looking at various follow up analyses of the group level modelling. 
- ``06.2_group_coefficients``: taken from analyse group, this is a supplementary material going into more detail about the model coefficients for the group level analyses, including post hoc comparisons for the interactions. 
- ``07_Plots``: Various plots 
- ``08_exploratory``: Supplementary material looking at the moderating effects of various demographic variables. 
- ``09_follow_up_individuals``: Supplementary material investigating the reliability of the individual level estimates. 
- ``10_change-mind-analyses``: Supplementary material investigating how often people changed their mind. 
- ``11_design-check``: Supplementary material checking that various aspects of our experimental design did not affect our results. 
- `derived`: Contains various transformations of the clean data generated from the analysis scripts. `Source_data.csv` is output from the experiment code, so cannot be generated here. 

