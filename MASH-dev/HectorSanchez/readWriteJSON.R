library(jsonlite)
jsonOut=prettify(toJSON(mosquito_par))
write(jsonOut,paste0(DIR,"mosquitoParameters.json"))
read_json(paste0(DIR,"mosquitoParameters.json"))