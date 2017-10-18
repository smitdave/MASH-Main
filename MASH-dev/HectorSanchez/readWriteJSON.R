library(jsonlite)
jsonOut=prettify(toJSON(mosquito_par))
write(jsonOut,paste0(DIR,"mosquitoParameters.json"))
par=read_json(paste0(DIR,"mosquitoParameters.json"),simplifyVector=TRUE)