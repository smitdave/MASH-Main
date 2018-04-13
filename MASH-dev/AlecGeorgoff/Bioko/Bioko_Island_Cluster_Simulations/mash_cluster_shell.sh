#$ -S /bin/sh
#/usr/local/R-current/bin/R <$1 --no-save $2 $3 $4 $5
#/usr/local/R-current/bin/R <$1 --no-save $@

alias r_mash="/share/local/singularity-2.4.2/bin/singularity exec /share/singularity-images/malaria_modeling/r_mash.img /usr/local/bin/R"

r_mash < $1 --no-save $@
