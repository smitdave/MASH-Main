################################################################################################################
# Plotly routines tests
# HMSC
################################################################################################################
library(plotly)
library(igraph)
library(markovchain)
library(riverplot)
library(igraph)
library(reshape2)
library(RColorBrewer)
library(networkD3)
library(circlize)
library(parallel)
library(MASH.MBPT)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
# Load data
#MASH="/Users/sanchez.hmsc/Documents/GitHub/MASH-Development/MAIN/"
#source("/Users/sanchez.hmsc/Documents/GitHub/MASH-Development/MAIN/Rdev/GUI/MBITES/BionomicsPlotLy.R")
#source("/Users/sanchez.hmsc/Documents/GitHub/MASH-Development/MAIN/Rdev/GUI/MBITES/LandscapePlotLy.R")
#source("/Users/sanchez.hmsc/Documents/GitHub/MASH-Development/MAIN/Rdev/GUI/MBITES/CohortsPlotLy.R")
bionomics = importBionomics(directory = MASH)
history = importHistory(directory = MASH)
names(bionomics[[1]])

# Lifespan histograms
plotLifespansHistogram(bionomics,bionomics)
# Blood meal size histograms
plotBmIntHistogram(bionomics)
# T Batch histogram
plotTBatchHistogram(bionomics)
# Scatter Dissection
testHistory=history[[21]]
dissectMosquito(testHistory)
# Landscape Plot
plotLandscape(LANDSCAPE)
# Chord diagrams
circlizeStatesTransitionMatrix(history)
chorddiagramPopulationStates(history)
circlizeStatesTransitionMatrixNormalized(history)
# Aggregate graphs
aggregateMosquitoStateTimes(history[[1]],deadDelta=.5)
popSpentTimes=aggregateMosquitoPopulationStateTimes(history,cl)
graphStatesHistoryInMosquitoPopulation(history)

##########################################################################################
##########################################################################################
# UNDER DEVELOPMENT
##########################################################################################
##########################################################################################

##########################################################################################
# Bar Dissection
##########################################################################################
SwitchColour=function(state){
  switch(state,
    "D"="#555555",
    "M"="#95E455",
    "F"="#ABDED1",
    "S"="#4394A8",
    "B"="#D61E43",
    "O"="#1C3B5B",
    "L"="#52C465",
    "R"="#B600EA",
    "E"="purple"
  )
}

testHistory=history[[4]]
id=testHistory$id
states=testHistory$stateH
times=c(testHistory$timeH,testHistory$timeH[length(testHistory$timeH)]+1)[2:(length(testHistory$timeH)+1)]
ixs=testHistory$ixH
pSet=testHistory$pSetH
colors=lapply(states,SwitchColour)

# Create data
data=data.frame(t(times))
 # Let's do a fist ploty el chic
p<-plot_ly(data,y=data[[1]],type="bar",name=states[[1]],color=I(colors[[1]]),opacity=.9,width=300,height=500)
 # Add 5 trace to this graphic with a loop!
for(i in 2:length(data)){
    p<-add_trace(p,y=data[[i]],type="bar",name=states[[i]],color=I(colors[[i]]))
}
p=p%>%layout(yaxis = list(title='Time',zeroline=FALSE),xaxis=list(title = "",
  zeroline = FALSE,showline = FALSE,showticklabels = FALSE,showgrid = TRUE),barmode='stack',legend=list(x=1.05,y=.5,bgcolor="#FFFFFF"))
p
##########################################################################################
# Scatter Dissection
##########################################################################################
testHistory=history[[10]]
DissectMosquito(testHistory)
##########################################################################################
cohortIx = getCohortIndices.history(history = history,sites = FALSE)
cohortT = getStateTraj.history(ix = cohortIx[[1]],history = history,female = TRUE)

stateT=cohortT
logX = FALSE
normY = FALSE

  times = sapply(stateT,function(x){x$time})*10
  timesLabel = times
  maxY = max(unlist(stateT[[1]][-1]))

  if(logX){
    if(0 %in% times){
      times = log(times+1)
    } else {
      times = log(times)
    }
    xTitle = "Time (log)"
  } else {
    xTitle = "Time"
  }

  if(normY){
    yLimU = 1
    yTitle = "Proportion"
  } else {
    yLimU = maxY
    yTitle = "Count"
  }

  stateSpace = names(stateT[[1]])[-1]
  stateCol = ggCol(n = length(stateSpace))

  p <- plot_ly(data,x=times,y=sapply(stateT,function(x){x$F}), type = 'scatter', mode = 'lines', line = list(shape = "linear"),name=stateSpace[[1]])
  # plot state trajectories
  for(st in 2:length(stateSpace)){
    stateY = sapply(stateT,function(x){x[[stateSpace[st]]]})
    if(normY){
      stateY = stateY / maxY
    }
    p=add_trace(p,x = times,y = stateY,name=stateSpace[[st]])
  }
  p

##########################################################################################
mosquito=history[[1]]

states=mosquito$stateH
times=mosquito$timeH

getMosquitoTimeEvents=function(mosquito){
  list(states=mosquito$stateH,times=mosquito$timeH)
}
##########################################################################################
pfsiOnePlotLy=function(ixH,p){
  eT = HUMANS[[ixH]]$Pathogens$Pf$eventT[-1]
  eV = HUMANS[[ixH]]$Pathogens$Pf$events[-1]

  ixF = which(eV == "F")
  if(length(ixF >0)){
    fevers = eT[ixF]
    eT = eT[-ixF]
    eV = eV[-ixF]
    p=add_trace(p,x=c(fevers),y=c(0*fevers+ixH),type='scatter',mode='markers',mode='markers',marker=list(color="red",size=10),name="Fever",legendgroup='Fever')
  }

  if(length(eT[-1]) > 0){
    for(i in 1:length(eT[-1])){
      if(eV[i] == "I"){
        p=add_trace(p,x=c(eT[i],eT[i+1]),y=c(ixH,ixH),type='scatter',mode='lines',line=list(shape="linear",color="red",width=3),hoverinfo=TRUE,name="I",legendgroup='I')
      }
      if(eV[i] == "S"){
        p=add_trace(p,x=c(eT[i],eT[i+1]),y=c(ixH,ixH),type='scatter',mode='lines',line=list(shape="linear",color="#C931AA",width=3),hoverinfo=TRUE,name="S",legendgroup='S')
      }
      if(eV[i] == "P"){
        p=add_trace(p,x=c(eT[i],eT[i+1]),y=c(ixH,ixH),type='scatter',mode='lines',line=list(shape="linear",color="blue",width=3),hoverinfo=TRUE,name="P",legendgroup='P')
      }
    }
  }

  ixP = which(eV == "P")
  if(length(ixP >0)){
    treated = eT[ixP]
    p=add_trace(p,x=treated,y=0*treated+ixH,type='scatter',mode='markers',mode='markers',marker=list(color="blue",size=10),name="Prophylaxis",legendgroup='Prophylaxis')
  }

  return(p)
}
pfsiPlotLy=function(){
  fontTicks=15
  fontLegend=25

  rng = c(0,1)
  for(i in 1:length(HUMANS)){rng = range(rng,HUMANS[[i]]$Pathogens$Pf$eventT[-1])}
  #plot(c(0,tMax), c(0.9,length(HUMANS)+0.1), type = "n", xaxt = "n", yaxt = "n", ylab = "Humans", xlab = "Time")
  p=plot_ly(type='scatter',mode='lines')%>% layout(
    title='PfSI',
    titlefont=list(size=50),
    yaxis=list(title="Human ID",titlefont=list(size=fontLegend),tickfont=list(size=fontTicks)),
    xaxis=list(title="Time",titlefont=list(size=fontLegend),tickfont=list(size=fontTicks)),
    margin=list(l=75,r=0,b=75,t=80,pad=0),
    showlegend=FALSE
  )

  ttMax = tMax/365
  #axis(1, c(0:ttMax)*365, c(0:ttMax))
  for(i in 1:length(HUMANS)){p=pfsiOnePlotLy(i,p)}
}
pfsiPlotLy()

##########################################################################################
# Deprecated
##########################################################################################

# mosquitoHistory=history[[12]]
# mosquitoHistory
#
# #Constants
# statesPattern=c("D","M","F","B","R","L","O","S","E")
# delta=1
# #Required vectors
# states=mosquitoHistory$stateH
# times=mosquitoHistory$timeH
# #Operations
# paddedTimes=c(times,times[[length(times)]]+delta)
# stateTimes=diff(paddedTimes)
# #Aggregating the time spent in each state
# statesList=data.frame(state=states,time=stateTimes)
# aggregate(statesList$time,list(timeSpent=statesList$state),sum)
# #


# autocurve.edges2 <-function (graph, start = 0.5)
# {
#     cm <- count.multiple(graph)
#     mut <-is.mutual(graph)  #are connections mutual?
#     el <- apply(get.edgelist(graph, names = FALSE), 1, paste,
#         collapse = ":")
#     ord <- order(el)
#     res <- numeric(length(ord))
#     p <- 1
#     while (p <= length(res)) {
#         m <- cm[ord[p]]
#         mut.obs <-mut[ord[p]] #are the connections mutual for this point?
#         idx <- p:(p + m - 1)
#         if (m == 1 & mut.obs==FALSE) { #no mutual conn = no curve
#             r <- 0
#         }
#         else {
#             r <- seq(-start, start, length = m)
#         }
#         res[ord[idx]] <- r
#         p <- p + m
#     }
#     res
# }

# t=transitionsInMosquitoStates(history[[1]])
# transitions=transitionsInMosquitoPopulation(history)
#
# statesPattern=c("D","M","F","B","R","L","O","S","E")
# transitions[statesPattern,statesPattern]
#
# mosquitoHistory=history[[2]]$stateH
# statesTransitions=factor(x=mosquitoHistory,levels=statesPattern)
# createSequenceMatrix(statesTransitions,toRowProbs=FALSE)
#
# createSequenceMatrix(mosquitoHistory,possibleStates=statesPattern)


# ##########################################################################################
# # Sankey and Force Network test
# ##########################################################################################
# transitions=transitionsInMosquitoPopulation(history)
# #g <- graph.adjacency(transitions)
# #as_edgelist(g)
#
# melted=melt(transitions)
# #melted$value=melted$value/sum(melted$value)
#
# nodes=data.frame(name=statesPattern)
# key=data.frame(id=0:(length(statesPattern)-1),name=statesPattern)
# df=melted
# df[["Var1"]] <- key[match(df[['Var1']], key[['name']] ) , 'id']
# df[["Var2"]] <- key[match(df[['Var2']], key[['name']] ) , 'id']
#
# sankeyData=list(nodes=nodes,links=df)
#
# sankeyNetwork(Links=sankeyData$links,Nodes=sankeyData$nodes,Source="Var1",Target="Var2",Value="value",NodeID="name",units = "TWh", fontSize = 12, nodeWidth = 30)
# forceNetwork(Links=sankeyData$links,Nodes=sankeyData$nodes,NodeID="name",Source="Var1",zoom=T,radiusCalculation = JS("1000"),linkWidth = JS("function(d) { return Math.sqrt(d.value)/10; }"),arrows=TRUE,Group=1,charge=-10000,Target="Var2",Value="value", opacity = 0.8)
#
# ##########################################################################################
# transitions=transitionsInMosquitoPopulation(history)
# net=network(transitions,matrix.type = "adjacency")
# ggnet2(net,node.color="black",edge.label.color = "darkred",arrow.size=5,edge.label = "weights",arrow.gap=0.025,size=sample(0:10,length(statesPattern),replace=TRUE),mode = "circle", edge.color = "grey",label=statesPattern,label.color="white")


# statesPattern=c("D","M","F","B","R","L","O","S","E")
# aggregates=parLapply(cl,history,aggregateMosquitoStateTimes)
# statesPop=unlist(sapply(aggregates,function(x){as.character(x$timeSpent)}),recursive=FALSE,use.names=FALSE)
# timesPop=unlist(sapply(aggregates,function(x){x$x}),recursive=FALSE,use.names=FALSE)
# statesPopList=data.frame(states=statesPop,times=timesPop)
# totalPopStateTimes=aggregate(statesPopList$time,list(timeSpent=statesPopList$state),sum)
# df=totalPopStateTimes
# ordered=df[match(statesPattern,df$timeSpent),]

# transitions=transitionsInMosquitoPopulation(history)
# popSpentTimes=aggregateMosquitoPopulationStateTimes(history,cl)
# times=replace(popSpentTimes$x,is.na(popSpentTimes$x),0)
# scaledTimes=times/500
#
# net=graph.adjacency(transitions,mode="directed",weighted=TRUE,diag=FALSE)
# summary(net)
# statesPattern=c("D","M","F","B","R","L","O","S","E")
# colors=c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
# colors=sapply(colors,function(x){adjustcolor(x,alpha.f=.75)})
# coords <- layout_(net, in_circle())
# plot.igraph(net,
#     vertex.label=V(net)$name,vertex.label.color="black",
#     vertex.frame.color=rgb(1,1,1,1),layout=coords,vertex.label.family="sans",vertex.label.font=2,
#     vertex.size=scaledTimes,edge.color=NULL,
#     edge.width=E(net)$weight/5000,edge.arrow.size=0.5,
#     vertex.color=colors,edge.curved=.2#,edge.curved=seq(-.5,.5, length = ecount(net))#,edge.curved=autocurve.edges2(net)
# )
