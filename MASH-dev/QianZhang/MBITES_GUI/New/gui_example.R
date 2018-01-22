# example for GUI

################################################################################################################
# 
# functions to do plots
# HMSC,SLW
# 
################################################################################################################

###############################################################################
# Histograms
###############################################################################

# these functions take parameter 'data' which is raw JSON read back into R

bionomics_lifespan <- function(data){
  lifespans = vapply(X = data,FUN = function(x){
    x$bionomics_lifespan[[1]]
  },FUN.VALUE = numeric(1),USE.NAMES = FALSE)
  return(lifespans)
}

bionomics_BMinterval <- function(data){
  BMintervals = vapply(X = data,FUN = function(x){
    if(x$bionomics_bmInt[[1]]>0){
      return(x$bionomics_bmInt[[1]])  
    } else {
      return(NaN) 
    }
  },FUN.VALUE = numeric(1),USE.NAMES = FALSE)
  BMintervals = Filter(Negate(is.nan),BMintervals)
  return(BMintervals)
}

bionomics_HumanBMinterval <- function(data){
  HumanBMintervals = vapply(X = data,FUN = function(x){
    if(x$bionomics_bmIntH[[1]]>0){
      return(x$bionomics_bmIntH[[1]])
    } else {
      return(NaN)
    }
  },FUN.VALUE = numeric(1),USE.NAMES = FALSE)
  HumanBMintervals = Filter(Negate(is.nan),HumanBMintervals)
  return(HumanBMintervals)
}

bionomics_HumanBM <- function(data){
  HumanBM = vapply(X = data,FUN = function(x){
    x$feedHumanH[[1]]
  },FUN.VALUE = numeric(1),USE.NAMES = FALSE)
  return(HumanBM)
}

bionomics_vc <- function(data, eip=10){
  vc = vapply(X = data,FUN = function(x,eip){
    feedT = unlist(x$feedAllT)
    if(length(feedT)<2 | is.null(feedT)){
      return(NaN)
    } else {
      if((diff(feedT)>eip)[1]){
        # sum all pairs of bites that are more than EIP days apart
        sum(apply(X = combn(feedT,2),MARGIN = 2,FUN = function(x){
          diff(x)>eip
        }))
      } else {
        return(NaN)
      }
    }
  },FUN.VALUE = numeric(1),eip = eip,USE.NAMES = FALSE)
  vc = Filter(Negate(is.nan),vc)
  return(vc)
}

# HMSC plotly histogram
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


###############################################################################
# Chord diagram
###############################################################################


# oneHistory: a single mosquito's JSON outfile
transitionsInMosquitoStates <- function(oneHistory, stateSpace = c("D","M","F","B","R","L","O","S","E")){
  states = oneHistory$stateH
  createSequenceMatrix(stringchar = unlist(states[-1]),possibleStates = stateSpace)
}

transitionsInMosquitoPopulation <- function(popHistory, stateSpace = c("D","M","F","B","R","L","O","S","E")){
  transMatrices = lapply(X = popHistory,FUN = transitionsInMosquitoStates)
  transitions = Reduce(f = "+",x = transMatrices)
  transitions[stateSpace,stateSpace]
}

circlizeStatesTransitionMatrix <- function(history, stateSpace = c("D","M","F","B","R","L","O","S","E")){
  transitions=transitionsInMosquitoPopulation(history,stateSpace=stateSpace)
  colors=c("#555555","#95E455","pink","red","purple","cyan","blue","yellow","grey")
  chordDiagramFromMatrix(transitions,directional=1,grid.col=colors,direction.type="arrows",self.link=2)
}


################################################################################################################
# 
# make plots
# 
################################################################################################################

library(stringr)
library(chorddiag)
library(jsonlite)
library(markovchain)
library(circlize)
library(ggplot2)
library(gridExtra)
library(plotly)
library(igraph)
library(igraph)
library(reshape2)
library(RColorBrewer)
# if you do not have chorddiag, use: devtools::install_github("mattflor/chorddiag")

mosquito_dir = "/Users/qianzh/project/MASH-Main/MASH-dev/QianZhang/MBITES_GUI/New/demo_json/" # wherever the json files are

# files from each simulation
files_l = system(command = paste0("ls ",mosquito_dir),intern = TRUE)
hist_l = files_l[grep(pattern = "History",x = files_l)] # individual json histories
pop_l = files_l[grep(pattern = "Pop",x = files_l)] # population csv

# output
mHist = fromJSON(txt = paste0(mosquito_dir,hist_l),flatten = FALSE,simplifyVector=FALSE)
mPop = read.table(file = paste0(mosquito_dir,pop_l),header = TRUE,sep = ",")

nullIx = which(vapply(X = mHist,FUN = function(x){x$ID[[1]]},FUN.VALUE = character(1)) == "NULL")
mHist = mHist[-nullIx]

axisSize = 12
titleSize = 14.5

lifespans = bionomics_lifespan(mHist)
lifespans_plot = ggplot(data = data.frame(lifespan=lifespans)) +
  geom_histogram(aes(lifespan),fill=rgb(0,.5,.5,.5)) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title=element_text(size=axisSize),
        plot.title = element_text(size=titleSize)) +
  guides(fill = FALSE) + 
  labs(x="Days",y="Frequency",title="Mosquito Lifespans")

BMintervals = bionomics_BMinterval(mHist)
BMintervals_plot = ggplot(data = data.frame(BMinterval=BMintervals)) +
  geom_histogram(aes(BMinterval), fill = rgb(0,.5,0,.5)) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title=element_text(size=axisSize),
        plot.title = element_text(size=titleSize)) +
  guides(fill = FALSE) + 
  labs(x="Days",y="Frequency",title="Bloodmeal Interval")

vectorialCapacity = bionomics_vc(mHist,eip = 8)
vectorialCapacity_plot = ggplot(data = data.frame(vectorialCapacity=vectorialCapacity)) +
  geom_histogram(aes(vectorialCapacity), fill = rgb(0,.5,0,.5),stat = "count") +
  scale_x_continuous(breaks=0:(max(vectorialCapacity)+2)) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title=element_text(size=axisSize),
        plot.title = element_text(size=titleSize)) +
  guides(fill = FALSE) + 
  labs(x="Vectorial Capacity",y="Frequency",title="Individual Vectorial Capacity")

HumanBMs = bionomics_HumanBM(mHist)
HumanBMs_plot = ggplot(data = data.frame(HumanBM=HumanBMs)) +
  geom_histogram(aes(HumanBM), fill = rgb(1,0,0,0.5),stat = "count") +
  scale_x_continuous(breaks=0:(max(HumanBMs)+2)) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title=element_text(size=axisSize),
        plot.title = element_text(size=titleSize)) +
  guides(fill = FALSE) + 
  labs(x="Count",y="Frequency",title="Human Bloodmeals")

grid.arrange(BMintervals_plot,HumanBMs_plot,lifespans_plot,vectorialCapacity_plot,nrow=2)

circlizeStatesTransitionMatrix(history = mHist)
