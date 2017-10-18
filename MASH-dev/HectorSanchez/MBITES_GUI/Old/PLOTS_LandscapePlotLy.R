################################################################################################################
# Landscape Plotly Routines
# HMSC
################################################################################################################

plotLandscape=function(LANDSCAPE){
  feedXY=PrepareSiteDataFrame(LANDSCAPE$feedSites) #t(sapply(LANDSCAPE$feedSites,function(x){x$siteXY}))
  aquaXY=PrepareSiteDataFrame(LANDSCAPE$aquaSites)
  sugarXY=PrepareSiteDataFrame(LANDSCAPE$sugarSites)
  mateXY=PrepareSiteDataFrame(LANDSCAPE$swarmSites)

  fontTicks=list(size=10,color="gray")
  fontLegend=list(size=20)

  p=plot_ly() %>% layout(
    #title='Landscape',
    titlefont=list(size=50),
    yaxis=list(zeroline=FALSE,title="Y Coordinate",titlefont=fontLegend,tickfont=fontTicks),
    xaxis=list(zeroline=FALSE,title="X Coordinate",titlefont=fontLegend,tickfont=fontTicks),
    margin=list(l=75,r=0,b=75,t=75,pad=0)
  )
  p=add_trace(p,data=data.frame(feedXY),x=~X,y=~Y,type='scatter',mode='markers',name="Feed",
            text = ~paste("ID: ",ID,"\nHumans: ",Humans,"\nw: ",w,"\nhaz: ",haz),
            marker=list(
              size=15,
              color='rgba(255,0,0,.25)',
              width=2,
              symbol="square",
              line=list(color='rgba(255,0,0,.9)',width=2)
            )
          )
  p=add_trace(p,data=data.frame(aquaXY),x=~X,y=~Y,type='scatter',mode='markers',name="Aqua",
            text = ~paste("ID: ",ID,"\nHumans: ",Humans,"\nw: ",w,"\nhaz: ",haz),
            marker=list(
              size=15,
              color='rgba(0,0,152,.2)',
              width=2,
              symbol="circle",
              line=list(color='rgba(0,0,152,.8)',width=2)
            )
          )
  p=add_trace(p,data=data.frame(sugarXY),x=~X,y=~Y,type='scatter',mode='markers',name="Sugar",
            text = ~paste("ID: ",ID,"\nHumans: ",Humans,"\nw: ",w,"\nhaz: ",haz),
            marker=list(
              size=15,
              color='rgba(0,152,0,.2)',
              width=2,
              symbol="diamond",
              line = list(color = 'rgba(0,152,0,.8)',width = 2)
            )
          )
  p=add_trace(p,data=data.frame(mateXY),x=~X,y=~Y,type='scatter',mode='markers',name="Mate",
            text = ~paste("ID: ",ID,"\nHumans: ",Humans,"\nw: ",w,"\nhaz: ",haz),
            marker=list(
              size=15,
              color='rgba(152,152,0,.2)',
              width=2,
              symbol="x",
              line = list(color='rgba(152,152,0,.8)',width = 2)
            )
          )
  p
}
PrepareSiteDataFrame=function(sitesList=LANDSCAPE$feedSites){
  siteTemp=t(sapply(sitesList,function(x){x$siteXY}))
  siteTemp=cbind(siteTemp,sapply(sitesList,function(x){x$ix}))
  siteTemp=cbind(siteTemp,sapply(sitesList,function(x){x$humanN}))
  #siteTemp=cbind(siteTemp,sapply(sitesList,function(x){x$humanIx}))
  siteTemp=cbind(siteTemp,sapply(sitesList,function(x){x$w}))
  siteTemp=cbind(siteTemp,sapply(sitesList,function(x){x$haz}))
  #siteTemp=cbind(siteTemp,sapply(sitesList,function(x){x$enterHouseP}))
  #siteTemp=cbind(siteTemp,sapply(sitesList,function(x){x$riskList}))
  colnames(siteTemp)=c("X","Y","ID","Humans","w","haz")
  siteTemp
}
KernelPlots=function(S=LANDSCAPE$aquaSites,D=LANDSCAPE$feedSites,N=50){

  dMesh = 0.01
  dMax=NULL
  mtl = NULL

  M1 = powerKernel(S,D) #Markov transition matrix
  M2 = distanceMat(S,D) #distance matrix
  M1n = M1/sum(M1) #normalized transition matrix

  if(is.null(dMax)){
    dMax = max(M2) + max(M2)*0.05
  }

  xCDF = function(x, M2, M1){ #return movement CDF by distance bins
    sum(M1[which(M2<=x, arr.ind = TRUE)])
  }

  x=seq(0, dMax, by = dMesh) #distance bins
  cdfT = sapply(X = x,FUN = xCDF, M2=M2, M1=M1n) #generate movement CDF
  d = length(S)
  cdf = matrix(0,N,length(x))
  for(i in 1:N){
    j = sample(x = 1:d,size = 1) #pick a random starting site
    cdf[i,] = sapply(x, xCDF, M2=M2[j,], M1=M1[j,])
  }

  xmx = max(cdfT[1], diff(cdfT), cdf[2,]-cdf[1,], cdf[3,]-cdf[2,])
  cdf = cdf[order(apply(cdf,MARGIN = 1,which.max)),] #sort rows by probability of long-range movement

  pdf=KernelPDFPlot(cdfT,cdf)
  cdf=KernelCDFPlot(cdfT,cdf)
  subplot(pdf,cdf)
}

KernelCDFPlot=function(cdfT,cdf){
  p=plot_ly(x=1:length(cdfT),y=cdfT,type="scatter",mode="lines",line=list(color='#000000',width=4),name="Average CDF",showlegend=FALSE)
  for(i in 1:length(cdf[,1])){
    p=add_trace(p,x=1:length(cdfT),y=cdf[i,],type="scatter",mode="lines",showlegend=FALSE,line=list(color=rgb(0,0,.5,.025)))
  }
  return(p)
}
KernelPDFPlot=function(cdfT,cdf){
  p=plot_ly(x=1:length(cdfT),y=c(cdfT[1],diff(cdfT)),type="scatter",mode="lines",line=list(color='#000000',width=4),name="Average PDF",showlegend=FALSE)
  for(i in 1:length(cdf[,1])){
    p=add_trace(p,x=1:length(cdfT),y=c(cdf[i,1],diff(cdf[i,])),type="scatter",mode="lines",showlegend=FALSE,line=list(color=rgb(0,0,.5,.025)))
  }
  return(p)
}

#showKernelsPlot(LANDSCAPE$feedSites,LANDSCAPE$feedSites)
#KernelPlots(LANDSCAPE$feedSites,LANDSCAPE$feedSites)


  # ##################################################
  # p=plot_ly(x=1:length(cdfT),y=cdfT,type="scatter",mode="lines",line=list(color='#000000',width=4),name="Average CDF",showlegend=TRUE)
  # for(i in 1:length(cdf[,1])){
  #   p=add_trace(p,x=1:length(cdfT),y=cdf[i,],type="scatter",mode="lines",showlegend=FALSE,line=list(color=rgb(0,0,.5,.025)))
  # }
  # p %>% layout(title="CDF")


#  plot_ly(z=diff(c(cdf[1],diff(cdf))))
#  plot_ly(z=cdf,type="heatmap",colors=colorRamp(c("purple","white")),showlegend=FALSE,yaxis=list(zeroline=FALSE))

  # ##################################################
  # ay <- list(
  #   overlaying = "y",
  #   side = "right",
  #   title = "second y axis",
  #   linewidth=6,
  #   zeroline=FALSE
  # )
  # p=plot_ly(z=cdf,type="heatmap",colors=colorRamp(c("purple","white")),showlegend=FALSE,yaxis=list(zeroline=FALSE)) %>%
  #   add_trace(x=1:length(cdfT),y=cdfT*N,type="scatter",mode="lines",line=list(color='#000000'),name="Average CDF",showlegend=FALSE,yaxis=list(zeroline=FALSE)) #%>%
  #   #add_trace(x=1:length(cdfT),y=cdf[i,])
  # #p=plot_ly()
  # for(i in 1:length(cdf[,1])){
  #   p=add_trace(p,x=1:length(cdfT),y=cdf[i,],type="scatter",mode="lines",showlegend=FALSE,line=list(color=rgb(0,0,.5,.025)),yaxis="y2")
  # }
  # p %>% layout(yaxis2=ay,xaxis=list(linewidth=10),yaxis=list(linewidth=10),yaxis=list(zeroline =FALSE))
  #
  # plot_ly(z=cdf,type="heatmap",colors=colorRamp(c("purple","white")),showlegend=FALSE)



# showKernelsPlot <- function(S,D,N=50, dMesh = 0.01, dMax=NULL, mtl = NULL){
#
#   if(exists("defaultPar",.GlobalEnv)){ #reset plot parameters
#     suppressWarnings(par(defaultPar))
#   }
#
#   M1 = powerKernel(S,D) #Markov transition matrix
#   M2 = distanceMat(S,D) #distance matrix
#   M1n = M1/sum(M1) #normalized transition matrix
#
#   if(is.null(dMax)){
#     dMax = max(M2) + max(M2)*0.05
#   }
#
#   xCDF = function(x, M2, M1){ #return movement CDF by distance bins
#     sum(M1[which(M2<=x, arr.ind = TRUE)])
#   }
#
#   x=seq(0, dMax, by = dMesh) #distance bins
#   cdfT = sapply(X = x,FUN = xCDF, M2=M2, M1=M1n) #generate movement CDF
#   d = length(S)
#   cdf = matrix(0,N,length(x))
#   for(i in 1:N){
#     j = sample(x = 1:d,size = 1) #pick a random starting site
#     cdf[i,] = sapply(x, xCDF, M2=M2[j,], M1=M1[j,])
#   }
#
#   xmx = max(cdfT[1], diff(cdfT), cdf[2,]-cdf[1,], cdf[3,]-cdf[2,])
#
#   par(mfrow=c(1,2))
#   colors = viridis(n=N)
#   plot(x, cdfT, type = "l", xlab = "Distance", ylab = "CDF", ylim = c(0,1), main = mtl)
#   grid()
#   cdf = cdf[order(apply(cdf,MARGIN = 1,which.max)),] #sort rows by probability of long-range movement
#   for(i in 1:N){
#     lines(x, cdf[i,], col = colors[i])
#   }
#   lines(x,cdfT, lwd =3)
#
#   #need to plot the max of the y values as ylin.
#   plot(x, c(cdfT[1],diff(cdfT)), xlab = "Distance", ylab = "PDF", type = "l", ylim = c(0, 1), main = mtl)
#   grid()
#   for(i in 1:N){
#     lines(x, c(cdf[i,1],diff(cdf[i,])), col = colors[i])
#   }
#   lines(x,c(cdfT[1],diff(cdfT)), lwd =3)
#   par(mfrow=c(1,1))
# }




