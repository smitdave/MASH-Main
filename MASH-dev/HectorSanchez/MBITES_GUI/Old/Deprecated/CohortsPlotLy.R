################################################################################################################
# Cohort PlotLy Routines
# HMSC
################################################################################################################
histogramPlotLyGenericBionomics=function(data,title,color){
  p=plot_ly(x=data,name=title,marker=list(color=color),type="histogram") %>%
    layout(
      barmode="overlay",
      legend=list(x=1.05,y=.5,bgcolor="#FFFFFF"),
      title=paste(title, "( Mean: ",signif(mean(data),3),")"),
      bargap=0.1
    )
  p
}
PlotLyMosquitoLifespans=function(MBitesBro_CohortsOut){histogramPlotLyGenericBionomics(MBitesBro_CohortsOut$data$L,"Mosquito Lifespans",rgb(0,.5,.5,.5))}
PlotLyBMInterval=function(MBitesBro_CohortsOut){histogramPlotLyGenericBionomics(MBitesBro_CohortsOut$data$f,"BM Interval",rgb(0,.5,0,.5))}
PlotLyHumanBMInterval=function(MBitesBro_CohortsOut){histogramPlotLyGenericBionomics(MBitesBro_CohortsOut$data$a,"Human BM Interval",rgb(1,.5,0,.5))}
PlotLyHumanBM=function(MBitesBro_CohortsOut){histogramPlotLyGenericBionomics(MBitesBro_CohortsOut$data$N,"Human BM",rgb(1,0,0,.5))}
###############################################################################################
###############################################################################################
# PP=DHM.Basic.Pset(ela.p=0)
# test=DHM.Basic.Cohort(PP)
# histogramPlotLyGenericBionomics(test$data$a,"Test",rgb(.5,.5,.5,.5))
# PlotLyMosquitoLifespans(test)
# PlotLyBMInterval(test)
# PlotLyHumanBMInterval(test)
# PlotLyHumanBM(test)
###############################################################################################
###############################################################################################
