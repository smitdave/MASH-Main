library(circlize)

############################################################################
# Looped ###################################################################
############################################################################
for(i in 1:20){
  # File ###################################################################
  #fileName=paste0("transitions_",toString(i),".csv")
  transitionsData= approx[[i]]
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
  #dev.off()
}
