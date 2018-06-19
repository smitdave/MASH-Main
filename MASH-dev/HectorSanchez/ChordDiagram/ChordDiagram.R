library(circlize)
# Setup Paths ############################################################
setwd("/Users/sanchez.hmsc/Documents/GitHub/MASH-Main/MASH-dev/HectorSanchez/ChordDiagram")
matricesSubDirectory=paste0(getwd(),"/matrices/")
# File ###################################################################
fileName="transitions_1.csv"
transitionsData=read.csv2(paste0(matricesSubDirectory,fileName),header=TRUE,row.names=1)
states=names(transitionsData)
states
transitionsMatrix=as.matrix(transitionsData)
# Plot ###################################################################
colors=c("purple","red","yellow","cyan","blue","grey")
chordDiagramFromMatrix(transitionsMatrix
  ,directional=1
  ,direction.type="arrows"
  ,grid.col=colors
  ,self.link=2
)
