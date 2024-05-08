For efficiency I'm running the individual-level models in parallel by submitting a separate job for each participant and model combination (defined in job_array.txt). If I didn't do this, it would take much longer to run, since BRMS models can be pretty. slow, and I have a lot of models and participants. For people not familiar with this method though, this could be confusing. 

To make things further complicated (but much faster), I send these models to my university's High Performance Computer (HPC), as it allows me to run hundreds of jobs in parallel, which I wouldn't be able to do locally. This means that my model scripts probably aren't going to run on your machine in their current form unless you have access to an HPC or you can run them using your terminal or a shell script. If you cannot do these, you will need to adjust the appropriate scripts so that the participant can be accessed. One simple way, would be wrapping each modelling script within a "for" loop. 

The general file structure is as follows: 

00. HPC Scripts: These are scripts that allow me to run jobs from my university's high performance computing cluster. 
01. Models: This folder contains the R scripts that run the BRMS models. The individual level models will not work in their current form, since they are set up to run one participant at a time and participant is read from the HPC scripts (as described above).
02. Output: The raw output of the BRMS models.
03. A script that loads and combines the output of each individual model. 
04. A script that loads and combines the output of each group-level model.
05. A quarto document that contains the analyses for the individual-level modelling.
06. A quarto document that contains the analyses for the group-level modelling.
06.1. Supplementary material looking at various follow up analyses of the group level modelling. 
07. Various plots 
08. Supplementary material looking at whether pro/con trials affected updating as well as various demographic characteristics. 