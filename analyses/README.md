This folder contains all of the scripts that were used to run the Bayesian linear models and all follow up analyses. 

For efficiency I ran the individual-level models in parallel by running each participant and model combination as seperate "jobs" (defined in job_array.txt). If I didn't do this, it would take much longer to run, since BRMS models can be pretty slow, and I have a lot of models and participants. For people not familiar with this method though, this could be confusing (sorry!). 

To make things further complicated (but much faster), I send these models to my university's High Performance Computer (HPC), as it allows me to run hundreds of jobs in parallel, which I wouldn't be able to do locally. This means that my model scripts probably aren't going to run on your machine in their current form unless you have access to an HPC or you can run them using your terminal or a shell script. If you cannot do these, you will need to adjust the appropriate scripts so that the participant can be accessed. One simple way, would be wrapping each modelling script within a "for" loop. Generally though, you will need at lease some base level understanding of running scripts from the terminal, or enough R knowledge to adjust the scripts, to run the models from scratch. If you do not need to run the models from scratch, all of the output from the models is available from https://osf.io/mtuyv/, so you can download the output from there directly, and copy it into the "output" folder. Once you have done that, all sccripts from 03 onwards should work, so long as you have all of the necessary packages installed. 

Many of these files are quarto/rmarkdown documents that have rendered PDFs that make reading the output easy (though the PDFs don't include code). 

The general file structure is as follows: 

00_HPC_scripts: These are scripts that allow me to run jobs from my university's high performance computing cluster. 
01_models: This folder contains the R scripts that run the BRMS models. The individual level models will not work in their current form if run directly from Rstudio, since they are set up to run one participant at a time and participant is read from the HPC scripts (as described above). The will work if you run them directly from the terminal (e.g, Rscript <pathtomodelscript> <subject>)
02_output: The raw output of the BRMS models. This folder is mostly empty because the output files are too big to store on github. You can download them directly from https://osf.io/mtuyv/. 
03_combine_output_individuals: A script that loads and combines the output of each individual model. 
04_combine_output_group: A script that loads and combines the output of each group-level model.
05_analyse_individuals: A quarto document (and the rendered PDF) that contains the analyses for the individual-level modelling.
06_analyse_group: A quarto document (and the rendered PDF) that contains the analyses for the group-level modelling.
06.1_follow-up-group-modelling: Supplementary material looking at various follow up analyses of the group level modelling. 
06.2_group_coefficients: taken from analyse group, this is a supplementary material going into more detail about the model coefficients for the group level analyses, including post hoc comparisons for the interactions. 
07_Plots: Various plots 
08_exploratory: Supplementary material looking at the moderating effects of various demographic variables. 
09_follow_up_individuals: Supplementary material investigating the reliability of the individual level estimates. 
10_change-mind-analyses: Supplementary material investigating how often people changed their mind. 
11_design-check: Supplementary material checking that various aspects of our experimental design did not affect our results. 
