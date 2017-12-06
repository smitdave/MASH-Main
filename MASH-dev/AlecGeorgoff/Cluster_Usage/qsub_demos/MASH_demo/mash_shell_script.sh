#$ -S /bin/sh
#/usr/local/R-current/bin/R <$1 --no-save $2 $3 $4 $5
#/usr/local/R-current/bin/R <$1 --no-save $@

alias r_dock="/share/local/singularity/bin/singularity exec /share/singularity-images/rstudio/ihme_rstudio_3403.img /usr/local/bin/R"

r_dock <$1 --no-save $@
