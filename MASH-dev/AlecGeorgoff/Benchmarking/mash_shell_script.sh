#$ -S /bin/sh
#/usr/local/R-current/bin/R <$1 --no-save $2 $3 $4 $5
#/usr/local/R-current/bin/R <$1 --no-save $@

# changed rstudio image from 3403 to 3431
alias r_dock="/share/local/singularity-2.4.2/bin/singularity exec /share/singularity-images/rstudio/ihme_rstudio_3431.img /usr/local/bin/R"

r_dock <$1 --no-save $@
