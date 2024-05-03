For efficiency I'm running the models in parallel by submitting a separate job for each participant and model combination (defined in job_array.txt). If I didn't do this, it would take much longer to run, since BRMS models can be pretty. slow, and I have a lot of models and participants. For people not familiar with this method though, this could be confusing. 

To make things further complicated (but much faster), I send these models to my university's High Performance Computer (HPC), as it allows me to run hundreds of jobs in parallel, which I wouldn't be able to do locally. This means that my model scripts probably aren't going to run on your machine in their current form.
Instead, you will need to adjust them so that the participant can be accessed. One simple way, would be wrapping each modelling script within a "for" loop. 

The general file structure is as follows: 

00. HPC Scripts: These are scripts that allow me to run jobs from my university's high performance computing cluster. 
01. Models: This folder contains the R scripts that run the BRMS models. The individual level models will not work in their current form, since they are set up to run one participant at a time and participant is read from the HPC scripts (as described above).
02. Output: The raw output of the BRMS models.
03. A script that loads and combines the output of each individual model. 
04. A script that loads and combines the output of each group-level model.
05. 