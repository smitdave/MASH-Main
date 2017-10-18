################################################################################################################
# Bionomics Plotly Routines
# HMSC
################################################################################################################
aggregateMosquitoStateTimes=function(mosquitoHistory,deadDelta=.5){
  #Constants
  statesPattern=c("D","M","F","B","R","L","O","S","E")
  #Required vectors
  states=mosquitoHistory$stateH
  times=mosquitoHistory$timeH
  #Operations
  paddedTimes=c(times,times[[length(times)]]+deadDelta)
  stateTimes=diff(paddedTimes)
  #Aggregating the time spent in each state
  statesList=data.frame(state=states,time=stateTimes)
  aggregate(statesList$time,list(timeSpent=statesList$state),sum)
}
aggregateMosquitoPopulationStateTimes=function(history,clusters=NULL){
  statesPattern=c("D","M","F","B","R","L","O","S","E")
  aggregates=lapply(history,aggregateMosquitoStateTimes)
  statesPop=unlist(sapply(aggregates,function(x){as.character(x$timeSpent)}),recursive=FALSE,use.names=FALSE)
  timesPop=unlist(sapply(aggregates,function(x){x$x}),recursive=FALSE,use.names=FALSE)
  statesPopList=data.frame(states=statesPop,times=timesPop)
  totalPopStateTimes=aggregate(statesPopList$time,list(timeSpent=statesPopList$state),sum)
  df=totalPopStateTimes
  ordered=df[match(statesPattern,df$timeSpent),]
}
graphStatesHistoryInMosquitoPopulation=function(history){
  transitions=transitionsInMosquitoPopulation(history)
  popSpentTimes=aggregateMosquitoPopulationStateTimes(history,cl)
  times=replace(popSpentTimes$x,is.na(popSpentTimes$x),0)
  scaledTimes=times/500

  net=graph.adjacency(transitions,mode="directed",weighted=TRUE,diag=FALSE)
  summary(net)
  statesPattern=c("D","M","F","B","R","L","O","S","E")
  colors=c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
  colors=sapply(colors,function(x){adjustcolor(x,alpha.f=.75)})
  coords <- layout_(net, in_circle())
  plot.igraph(net,
      vertex.label=V(net)$name,vertex.label.color="black",
      vertex.frame.color=rgb(1,1,1,1),layout=coords,vertex.label.family="sans",vertex.label.font=2,
      vertex.size=scaledTimes,edge.color=NULL,
      edge.width=E(net)$weight/5000,edge.arrow.size=0.5,
      vertex.color=colors,edge.curved=.2#,edge.curved=seq(-.5,.5, length = ecount(net))#,edge.curved=autocurve.edges2(net)
  )
}
##########################################################################################
# Transitions matrix
##########################################################################################
plotStatesTransitionsMatrix=function(history){
  transitions=transitionsInMosquitoPopulation(history)
  dataFrame=as.data.frame(transitions)
  f1=list(size=30,color="grey")
  f2=list(size=25,color="black")
  a1=list(title="Initial State",titlefont=f1,showticklabels = TRUE,tickfont=f2,showgrid=FALSE)
  a2=list(title="End State",titlefont=f1,showticklabels = TRUE,tickfont=f2,showgrid=FALSE)
  m <- list(l=75,r=10,b=75,t=10)
  plot_ly(x=names(dataFrame),y=names(dataFrame),z=t(transitions),type="heatmap",colorscale="Greys") %>% layout(xaxis=a1,yaxis=a2,margin=m)
}
##########################################################################################
# Chord diagram
##########################################################################################
transitionsInMosquitoStates=function(mosquitoHistory){
  states=mosquitoHistory$stateH
  statesPattern=c("D","M","F","B","R","L","O","S","E")
  createSequenceMatrix(states,possibleStates=statesPattern)
}
transitionsInMosquitoPopulation=function(history){
  statesPattern=c("D","M","F","B","R","L","O","S","E")
  transMatrices=lapply(history,transitionsInMosquitoStates)
  transitions=Reduce('+',transMatrices)
  transitions[statesPattern,statesPattern]
}
chorddiagramPopulationStates=function(history){
  statesPattern=c("D","M","F","B","R","L","O","S","E")
  colors=c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
  transitionsInStates=transitionsInMosquitoPopulation(history)
  chorddiag(transitionsInStates,width="1000px",height="1000px",groupColors=colors,chordedgeColor=NULL,groupnameFontsize=20,groupPadding=5,groupnamePadding=45,type="directional")
}
circlizeStatesTransitionMatrix=function(history){
  transitions=transitionsInMosquitoPopulation(history)
  colors=c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
  chordDiagramFromMatrix(transitions,directional=1,grid.col=colors,direction.type="arrows",self.link=2)
}
circlizeStatesTransitionMatrixNormalized=function(history){
  transitions=transitionsInMosquitoPopulation(history)
  mat2<-transitions/rowSums(transitions)
  mat2[is.nan(mat2)] = 0

  colors=c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
  chordDiagramFromMatrix(mat2,directional=1,grid.col=colors,direction.type="arrows",self.link=2)
}
##########################################################################################
# Lifespan histograms
##########################################################################################
getLifespan=function(bionomics){sapply(bionomics,function(x){x$lifespan})}
plotLifespansHistogram=function(bionomicsFemale,bionomicsMale){
  lifespanF=getLifespan(bionomicsFemale)
  lifespanM=getLifespan(bionomicsMale)+10
  fitF=density(lifespanF)
  fitM=density(lifespanM)
  p=plot_ly(alpha=0.5) %>%
    #add_trace(x=fitF$x, y = fitF$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Female Density", type = "scatter") %>%
    #add_trace(x=fitM$x, y = fitM$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Male Density", type = "scatter") %>%
    add_histogram(x=lifespanF,name="Female",histnorm='probability') %>%
    add_histogram(x=lifespanM,name="Male",histnorm='probability') %>%
    layout(
      barmode="overlay",#comment out for side to side bars
      legend=list(x=1.05,y=.5,bgcolor="#FFFFFF"),
      title = "Mosquito Lifespans",
      bargap=0.2
      #yaxis2 = list(overlaying="y",side="right")
    )
  return(p)
}
##########################################################################################
# Blood meal size histograms
##########################################################################################
getBmInt=function(bionomics,fun=mean){sapply(bionomics,function(x){mean(x$bmInt)})}
plotBmIntHistogram=function(bionomicsFemale){
  bmIntF=getBmInt(bionomicsFemale)
  #fitF=density(bmIntF)
  p=plot_ly(alpha=.5) %>%
    #add_trace(x=fitF$x, y = fitF$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Female Density", type = "scatter") %>%
    #add_trace(x=fitM$x, y = fitM$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Male Density", type = "scatter") %>%
    add_histogram(x=bmIntF,name="Female",histnorm='probability') %>%
    layout(
      barmode="overlay",#comment out for side to side bars
      legend=list(x=1.05,y=.5,bgcolor="#FFFFFF"),
      title = "Blood Mean Interval",
      bargap=0.2
      #yaxis2 = list(overlaying="y",side="right")
    )
  return(p)
}
##########################################################################################
# T Batch histogram
##########################################################################################
getTBatch=function(bionomics,fun=mean){sapply(bionomics,function(x){x$tBatch})}
plotTBatchHistogram=function(bionomicsFemale){
  bmIntF=getTBatch(bionomicsFemale)
  #fitF=density(bmIntF)
  p=plot_ly(alpha=.5) %>%
    #add_trace(x=fitF$x, y = fitF$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Female Density", type = "scatter") %>%
    #add_trace(x=fitM$x, y = fitM$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Male Density", type = "scatter") %>%
    add_histogram(x=bmIntF,name="Female",histnorm='probability') %>%
    layout(
      barmode="overlay",#comment out for side to side bars
      legend=list(x=1.05,y=.5,bgcolor="#FFFFFF"),
      title = "tBatch Mean Interval",
      bargap=0.2
      #yaxis2 = list(overlaying="y",side="right")
    )
  return(p)
}
##########################################################################################
# Scatter Dissection
##########################################################################################
dissectMosquito=function(mosquitoHistory){
  testHistory=mosquitoHistory
  statesPattern=  c("D","M","F","B","R","L","O","S","E")
  coloursPattern= c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
  #coloursPatternSafe= c("#555555","#95E455","#ABDED1","#4394A8","#D61E43","#1C3B5B","#52C465","#B600EA","purple")

  separation=1
  ys=seq(1,length(statesPattern),by=separation)
  states=testHistory$stateH
  ixs=testHistory$ixH
  pSet=testHistory$pSetH
  times=testHistory$timeH#c(testHistory$timeH,testHistory$timeH[length(testHistory$timeH)]+1)[2:(length(testHistory$timeH)+1)]
  timesCumSum=cumsum(times)

  df=data.frame(index=1:(length(states)+1),states=c(states,"X"),timesCumSum=c(timesCumSum,timesCumSum[[length(timesCumSum)]]+5),ixs=c(ixs[2:length(ixs)],"NA","NA"),pSet=c(pSet[2:length(pSet)],"NA","NA"))
  p=plot_ly()
  for(i in 1:length(statesPattern)){
    sub=subset(df,states==statesPattern[[i]])
    if((dim(sub)[[1]])>0){
      for(j in 1:(dim(sub)[[1]])){
        row=sub[j,]
        index=row$index
        xInit=row$timesCumSum
        xEnd=df[index+1,]$timesCumSum
        yC=c(ys[[i]],ys[[i]])
        xC=c(xInit,xEnd)
        p=add_trace(p,x=xC,y=yC,type='scatter',mode='lines',line=list(shape="linear",color=coloursPattern[[i]],width=20),hoverinfo=FALSE)
        p=add_trace(p,x=xC,y=yC,type='scatter',mode='markers',marker=list(color=coloursPattern[[i]],size=20),name=statesPattern[[i]],text=paste('Site ID: ',row$ixs,"\nType: ",row$pSet))
      }
    }
  }
  fontTicks=list(size=18)
  fontLegend=list(size=30)
  yaxisT=list(tickvals=seq(from=1,to=length(statesPattern),by=separation),ticktext=statesPattern,title='State',zeroline=FALSE,titlefont=fontLegend,tickfont=fontTicks)
  xaxisT=list(title='Time',zeroline=FALSE,titlefont=fontLegend,tickfont=fontTicks)
  p %>% layout(yaxis=yaxisT,xaxis=xaxisT,showlegend=FALSE,margin = list(l=75,r=0,b=75,t=0,pad=0))
}
##########################################################################################
# Constants
##########################################################################################
LINE_WIDTH = 4
LINE_STYLE = list(width=LINE_WIDTH)
FONT_LABEL <<- list(
  size = 15,
  color = "grey"
)
FONT_TICKS <- list(
  size = 10,
  color = "grey"
)
AxisStyle = function(label){
  list(
    title = label,
    tickwidth = 2,
    titlefont = FONT_LABEL,
    tickfont = FONT_TICKS,
    tickcolor = toRGB("grey"),
    rangemode = "tozero",
    linewidth = 4,
    linecolor = "grey",
    mirror = "ticks"
  )
}
