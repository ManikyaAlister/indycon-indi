File structure (read below for more information):

- `indi_1`: Individual-level null model.
- `indi_2`: Individual-level alternative model.
- `indi_1/2_session`: Individual-level models split by experimental session (for an analysis presented in the supplementary materials).
- `run-indi-1/2.sh`: Shell script to actually run the individual-level null models sequentially and locally. 
- `run_group_models_local.R`: Runs the group-level models.
- `run_broad_group_models_local.R`: Runs the group-level models as a function of broad claim type (knowable/unknowable), as described as a follow up in the pre-registration.
- `run-follow-up-group-modelling.R`: Runs various follow-up group-level models necessary for some analyses reported in the supplementary materials.

For efficiency I ran the individual-level models in parallel by running each participant and model combination as separate "jobs" (defined in job_array.txt) on my university’s HPC. If I didn't do this, it would take much longer to run, since BRMS models can be pretty slow, and I have a lot of models and participants. For people not familiar with this method though, this could be confusing (sorry!). **If you wish to run the individual-level models locally yourself**, I have made some shell scripts that will loop through each participant and fit the models to data sequentially: 

`run-indi-1.sh`: Fits the null model to each of the 78 participants. 
`run-indi-2.sh`: Fits the alternative model to each of the 78 participants. 


To run them, run the lines: `sh analyses/01_models/run-indi-1.sh` (for null model) and `sh analyses/01_models/run-indi-2.sh` for alternative model in the *terminal*. You can adjust the path in this script to also run this for the `by_session` models. 

Please note that running the individual models sequentially **may take a long time**. On my Mac Book Air M2 it took roughly 1.5 minutes per subject/model, so it could take upwards of ~200 minutes to run both models for all subjects sequentially. **You could make them run faster by including cores=4 to the brm() functions** so that each of the 4 chains run in parallel. 

**To run the group-level models** reported in the manuscript, run 01_models/run_group_models_local.R. This script uses the Parallel function in R to run the analyses on multiple cores on your computer. It means that you won’t see any output in your console as things are running, so you may need to just be patient until you wait for it to finish (even if nothing seems like it’s happening, it probably is). 

If you're running modelling scripts and you get an error that looks like “/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: complex: No such file or directory” just ignore it and let it keep running, it does not effect anything as far as I can tell. 

Once the models are run (or if you use the output available on the OSF) most of the files that analyse the output of the models are quarto/rmarkdown documents that have rendered PDFs that make reading the output easy (though the PDFs don't include code). 