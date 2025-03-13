This folder contains all of the scripts that were used to run the Bayesian linear models and all follow up analyses. 

For efficiency I ran the individual-level models in parallel by running each participant and model combination as seperate "jobs" (defined in job_array.txt) on my university’s HPC. If I didn't do this, it would take much longer to run, since BRMS models can be pretty slow, and I have a lot of models and participants. For people not familiar with this method though, this could be confusing (sorry!). **If you wish to run the individual-level models locally yourself**, I have made some shell scripts that will loop through each participant and fit the models to data sequentially: 

`01_models/run-indi-1.sh`: Fits the null model to each of the 78 participants. 
`01_models/run-indi-2.sh`: Fits the alternative model to each of the 78 participants. 

To run them, run the lines: `sh analyses/01_models/run-indi-1.sh` (for null model) and `sh analyses/01_models/run-indi-2.sh` for alternative model in the *terminal*. 

Please note that running the individual models sequentially **may take a long time**. On my Mac Book Air M2 it took roughly 1.5 minutes per subject/model, so it could take upwards of ~200 minutes to run both models for all subjects sequentially. 

**To run the group-level models** reported in the manuscript, run 01_models/run_group_models_local.R. This script uses the Parallel function in R to run the analyses on multiple cores on your computer. It means that you won’t see any output in your console as things are running, so you may need to just be patient until you wait for it to finish (even if nothing seems like it’s happening, it probably is). 

If you’re running modelling scripts and you get an error that looks like “/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: complex: No such file or directory” just ignore it and let it keep running., it does not effect anything as far as I can tell. 

Once the models are run (or if you use the output available on the OSF) most of the files that analyse the output of the models are quarto/rmarkdown documents that have rendered PDFs that make reading the output easy (though the PDFs don't include code). 

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

