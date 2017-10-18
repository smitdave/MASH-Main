####################################################################################################
# MASH IOFiles
# HMSC
#
####################################################################################################

listToXML=cmpfun(function(item,tag="PARAMETERS"){
  #. listToXML: converts a named list into an XML object
  if(typeof(item)!='list'){return(xmlNode(tag, item))}
  xml=xmlNode(tag)
  if(is.null(names(item))){
    names(item)=sapply(1:length(item),
    function(x){paste0("e_",x)})
  }
  for(name in names(item)){xml=append.xmlNode(xml,listToXML(item[[name]],name))}
  return(xml)
})
dataFrameToXML=function(data,tag="EXPERIMENT_DATA"){
  #. dataFrameToXML: converts a data frame into an XML object
  # Needs to be recursive
  xml=xmlNode(tag)
  for (i in 1:nrow(data)){
    tempXML=xmlNode("timeStamp")
    for (j in names(data)){
      tempXML=append.xmlNode(tempXML,xmlNode(j,data[i,j]))
    }
    xml=append.xmlNode(xml,tempXML)
  }
  return(xml)
}
dataFrameToXML2=function(item,tag="EXPERIMENT_DATA"){
  if(typeof(item)!='list'){return(xmlNode(tag, item))}
  xml=xmlNode(tag)
  for (i in 1:nrow(item)){
    tempXML=xmlNode("timeStamp")
    for (j in names(item)){
      print(j)
      tempXML=append.xmlNode(tempXML,dataFrameToXML2(item[i,j],j))
    }
    xml=append.xmlNode(xml,tempXML)
  }
  return(xml)
}
generateMASHExperimentalDataXML=function(
    dataFrame,
    randomSeed=RNDSEED,
    mbitesP=P
  ){
  #. exportMBITESExperimentalData: exports an experiment from a dataframe into XML format
  xmlDocument=xmlNode("MASH_EXPERIMENT")
  xmlHeader=xmlNode("EXPERIMENT_SETUP")
  xmlHeader=append.xmlNode(xmlHeader,xmlNode("RNGSeed",randomSeed))
  xmlHeader=append.xmlNode(xmlHeader,xmlNode("Hash_P",hashParametersList(P)))
  #This line can be changed to accept namedLists with a verification
  if(class(dataFrame)=="data.frame"){xmlBody=dataFrameToXML(dataFrame,"EXPERIMENT_DATA")}
  if(class(dataFrame)=="list"){xmlBody=listToXML(dataFrame,"EXPERIMENT_DATA")}
  xmlDocument=append.xmlNode(xmlDocument,xmlHeader)
  xmlDocument=append.xmlNode(xmlDocument,xmlBody)
  return(xmlDocument)
}
exportMASHExperimentalDataXML=function(mashExperimentalDataXML,randomID=round(runif(1,1,1000000000000000)),path="./"){
  #. exportMASHExperimentalDataXML: exports an XML experiment data to a file
  saveXML(mashExperimentalDataXML,paste0(path,randomID,".xml"))
}
exportMBITESMosquitoParameters=function(P=P,name="SETUP_MBITES_MOSQUITO_PARAMETERS.xml",path="./",tag="MBITES_MOSQUITO_PARAMETERS"){
  #. exportMBITESMosquitoParameters: creates XML file with the values of global P, which stores MBITES parameters
  xmlOut=listToXML(P,tag)
  #append.xmlNode(xmlOut,listToXML(list(RNDSEED=RNDSEED),"Test"))
  saveXML(xmlOut,paste0(path,name))
}
importMBITESMosquitoParameters=function(name="SETUP_MBITES_MOSQUITO_PARAMETERS.xml",path="./"){
  #. importMBITESMosquitoParameters: reads XML file with the values of global P, which stores MBITES parameters
  readXML=xmlParse(file=paste0(path,name))
  xmlToList(readXML)
}
exportSimulationParameters=function(name="SETUP_MASH_SIMULATION_PARAMETERS.xml",path="./"){
  #. exportSimulationParameters: creates XML file with simulation parameters
  simParameters=list(
    RNDSEED=RNDSEED
  )
  xmlOut=listToXML(simParameters,"SETUP_MASH_SIMULATION_PARAMETERS")
  saveXML(xmlOut,paste0(path,name))
}
importSimulationParameters=function(name="SETUP_MASH_SIMULATION_PARAMETERS.xml",path="./"){
  #. importSimulationParameters: reads XML file with simulation parameters
  readXML=xmlParse(file=paste0(path,name))
  xmlToList(readXML)
}
listToString=function(list){
  #. listToString: converts a named list into a flat string (for hashing purposes)
  collapsedList=paste(deparse(P,control = NULL),collapse='')
  gsub('(',"",gsub("list","",gsub(")","",gsub(" ","",collapsedList))),fixed=TRUE)
}
hashParametersList=function(list,algorithm="md5"){
  #. hashParametersList: converts a named list into a hash value (for experiment in-out verification)
  digest(listToString(list),algo=algorithm)
}


#n = c(2, 3, 5)
#s = c(data.frame(a="aa",b="bb",c="cc"))
#b = c(TRUE, FALSE, TRUE)
#df = data.frame(n, s, b)
#"data.frame"==class(df)

#nl=list(n=n,s=s,b=b)
#"list"==class(nl)

#exportMBITESMosquitoParameters(P)
#importMBITESMosquitoParameters()
#simulationParametersListExport()
