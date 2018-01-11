library(jsonlite)
library(rjson)
library(MASHmicro)
#Test parameters
DIR="/Users/chipdelmal/Desktop/MASHOUT/"
mosquito_par=mbites_par_female = MBITES.Complex.Parameters(PfEIP=1)
#Actual routine
jsonOut=prettify(toJSON(mosquito_par))
write(jsonOut,paste0(DIR,"mosquitoParameters.json"))
par=read_json(paste0(DIR,"mosquitoParameters.json"),simplifyVector=TRUE)




jsonOut=prettify(toJSON(mbites_par_female))
write(jsonOut,paste0(DIR,"mbites_par_female.json"))
par=read_json(paste0(DIR,"mbites_par_female.json"),simplifyVector=TRUE)

jsonOut=prettify(toJSON(mbites_par_male))
write(jsonOut,paste0(DIR,"mbites_par_male.json"))
par=read_json(paste0(DIR,"mbites_par_male.json"),simplifyVector=TRUE)


jsonOut=toJSON(human_par)
write(jsonOut,paste0(DIR,"jsonTest.json"))
par=fromJSON(paste0(DIR,"jsonTest.json"))


x=toJSON(human_par)
fromJSON(human_par)


a=toJSON(human_par)
write(a,paste0(DIR,"jsonTest.json"))
b=fromJSON(file=paste0(DIR,"jsonTest.json"))
