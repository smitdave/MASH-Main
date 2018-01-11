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


a=toJSON(mbites_par_female)
write(a,paste0(DIR,"jsonTest.json"))
b=fromJSON(file=paste0(DIR,"jsonTest.json"))



# HUMAN Parameters I/O working -------------------------------------------------
write(rjson::toJSON(human_par),paste0(DIR,"human_par.json"))
human_par=rjson::fromJSON(file=paste0(DIR,"human_par.json"))
# ------------------------------------------------------------------------------



serial=serializeJSON(mbites_par_female,pretty=FALSE)
write(serial,paste0(DIR,"mbites_par_female.json"))
con=file(paste0(DIR,"mbites_par_female.json"))
unserializeJSON(con)



# ------------------------------------------------------------------------------
export_MASH_parameters=function(file,parameters_structure){
  serial=jsonlite::serializeJSON(parameters_structure,pretty=FALSE)
  write(serial,file)
}
import_MASH_parameters=function(file){
  connection=file(file)
  jsonlite::unserializeJSON(connection)
}
# ------------------------------------------------------------------------------
