# Setting up Bioko Island MacroSimulations on the Cluster

1. Pull the files down from this directory using get_infected

2. Make sure that you use the correct containerized version of MASH when opening R.  I recommend adding the following line to your `bash_profile` and then sourcing so that `r_mash` becomes a function you can call from the command line.

    `alias r_mash="/share/local/singularity/bin/singularity exec /share/singularity-images/malaria_modeling/r_mash /usr/local/bin/R"`

    It may be tricky to try and start an interactive session (RStudio) that has the correct build of MASH in it, since the container that lets us run MASH is different from the one that allows us to run RStudio.  If this is something we really need then I can get to work on it but in the meantime we might just need to run `BI_n_patch_trial.R` using `r_mash` in the command line and then using the interactive session to look at the outputs.

3. Open all 3 `.R` files and adjust the paths.
    * `Bioko_Island_Simulation_Setup.R` needs to see all four `.csv` input files
    * `BI_n_patch_trial.R` needs to see both `Bioko_Island_Simulation_Setup.R` and `Multipatch_data_transform.R`, as well as the output directories need to be defined correctly on your path.  I recommend saving to the empty folders at `BI_n_patch_trial_outputs/Single_trajectory` and at `BI_n_patch_trial_outputs/Ensemble_trajectories`

4. Try to replicate the results stored in BI_n_patch_trial_outputs - step through each of the lines inside `BI_n_patch_trial.R`
    * There is a single trajectory run that we do first.  Stepping through should get you to produce a plot that looks like this one.  This takes only about a minute to run on my laptop.
    ![single](BI_n_patch_trial_outputs/Single_trajectory_verify/Single_trajectory_check.jpeg)
    * After that, we reset and start an ensemble of 10 trajectories and plot a summary of that ensemble.  This takes about 10 minutes to run on my laptop.
    ![ensemble](BI_n_patch_trial_outputs/Ensemble_trajectories_verify/Ensemble_trajectory_check.jpeg)
