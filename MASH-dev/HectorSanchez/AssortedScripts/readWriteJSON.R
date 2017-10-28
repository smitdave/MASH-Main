library(jsonlite)
library(MASHmicro)
#Test parameters
DIR="/Users/chipdelmal/Desktop/MASHOUT/"
mosquito_par=mbites_par_female = MBITES.Complex.Parameters(PfEIP=1)
#Actual routine
jsonOut=prettify(toJSON(mosquito_par))
write(jsonOut,paste0(DIR,"mosquitoParameters.json"))
par=read_json(paste0(DIR,"mosquitoParameters.json"),simplifyVector=TRUE)
