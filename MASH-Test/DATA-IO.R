##############################################################################################################
#
#       ___                                __                    __  ___      __            _       __
#      /   |  ____  ____  ____ ___  ____ _/ /___  __  _______   /  |/  /___ _/ /____  _____(_)___ _/ /____
#     / /| | / __ \/ __ \/ __ `__ \/ __ `/ / __ \/ / / / ___/  / /|_/ / __ `/ __/ _ \/ ___/ / __ `/ / ___/
#    / ___ |/ / / / /_/ / / / / / / /_/ / / /_/ / /_/ (__  )  / /  / / /_/ / /_/  __/ /  / / /_/ / (__  )
#   /_/  |_/_/ /_/\____/_/ /_/ /_/\__,_/_/\____/\__,_/____/  /_/  /_/\__,_/\__/\___/_/  /_/\__,_/_/____/
#
#   Test Data I/O
#
#
##############################################################################################################

library(jsonlite)

myDIR = "/Users/slwu89/Desktop/MASHOUT/"
if(!dir.exists(myDIR)){
  dir.create(path = myDIR)
}

makeData = function(id){
  list(
    id = as.character(id),
    a = rlnorm(n = 2),
    b = rexp(n = 2)
  )
}

dat = lapply(X = 1:10,FUN = makeData)

toJSON(dat,pretty = T)

# figure out how to write JSON out "line by line"

# make a connection
makeConnection <- function(directiory, fileName){
  con = file(description = paste0(directory,fileName),open = "wt")
  return(con)
}

fileName = "testConTemp.json"
con = makeConnection(directiory = myDIR,fileName = fileName)


# simulate iterating through some list of data and outputting line by line
writeLines(text = "[",con = con)

# now we can write to it
for(i in 1:length(dat)){
  # writeLines(text = jsonlite::toJSON(x = dat[[i]],pretty = TRUE),con = con)
  cat(jsonlite::toJSON(x = dat[[i]],pretty = TRUE),",\n",sep="",file = con)
}

writeLines(text = "]",con = con)

close(con)

fileNameFinal = sub(pattern = "Temp",x = fileName,replacement = "")


sedLinux = "sed: 1: 'x;${s/,$//;p;x}; 2,$ p' "
sedMac = "sed -n -e x -e '${s/,$//;p;x;}' -e '2,$ p' "

switch()

system(command = paste0("sed -n -e x -e '${s/,$//;p;x;}' -e '2,$ p' ",directory,fileName," > ",directory,fileNameFinal))
system(command = paste0("rm ",directiory,fileName))
