library(circlize)
# Setup Paths ############################################################
setwd("/Users/sanchez.hmsc/Documents/GitHub/MASH-Main/MASH-dev/HectorSanchez/ChordDiagram")
matricesSubDirectory=paste0(getwd(),"/matrices/")
# File ###################################################################
fileName="transitions_2.csv"
transitionsData=read.csv2(paste0(matricesSubDirectory,fileName),header=TRUE,row.names=1)
states=names(transitionsData)
states
transitionsMatrix=as.matrix(transitionsData)
# Plot ###################################################################
colors=c("purple","red","yellow","cyan","blue","grey")
png(paste0(strsplit(fileName, "\\.")[[1]][[1]],".png"),res=500,width=2000,height=2000)
chordDiagramFromMatrix(transitionsMatrix
                       ,directional=1
                       ,direction.type="arrows"
                       ,grid.col=colors
                       ,self.link=2
)
dev.off()
############################################################################
# Looped ###################################################################
############################################################################
for(i in 1:20){
  # File ###################################################################
  fileName=paste0("transitions_",toString(i),".csv")
  transitionsData=read.csv2(paste0(matricesSubDirectory,fileName),header=TRUE,row.names=1)
  states=names(transitionsData)
  states
  transitionsMatrix=as.matrix(transitionsData)
  # Plot ###################################################################
  colors=c("purple","red","yellow","cyan","blue","grey")
  png(paste0(strsplit(fileName, "\\.")[[1]][[1]],".png"),res=500,width=2000,height=2000)
  chordDiagramFromMatrix(transitionsMatrix
                         ,directional=1
                         ,direction.type="arrows"
                         ,grid.col=colors
                         ,self.link=2
  )
  dev.off()  
}