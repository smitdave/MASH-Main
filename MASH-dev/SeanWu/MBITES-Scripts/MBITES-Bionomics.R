###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Bionomics
#     MBITES Team
#     July 2018
#
###############################################################################


###############################################################################
# load libraries and data
###############################################################################

rm(list=ls());gc()

library(jsonlite)
library(ggplot2)

# where the files can be found
output_dir <- "/Users/slwu89/Desktop/mbites/run1"

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_1.json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-nrow(mosquitos_df),]
human <- fromJSON(paste0(output_dir,"/human_1.json"), flatten = TRUE)


###############################################################################
# mosquito lifespans
###############################################################################

#Takes in a data frame with column called "time" that contains mosquito life events
#last time is death and first time is birth
#returns plot of lifespan
lifespan <- function(mosquitos_df) {

  n = nrow(mosquitos_df)
  w = rep(NaN,n)
  pb = txtProgressBar(min = 1, max = n, initial = 0)

  for(i in 1:n){
    # filter mosquitoes that were still alive at end of simulation
    if(tail(mosquitos_df[i,"behavior"][[1]],1) != "E"){
      w[i] = tail(mosquitos_df[i,"time"][[1]],1) - mosquitos_df[i,"time"][[1]][1]
    }
    setTxtProgressBar(pb,i)
  }

  w = Filter(Negate(is.nan),w)
  return(data.frame(lifespan=w))
}

lf <- lifespan(mosquitos_df)
mean(lf$lifespan)
sd(lf$lifespan)

#Mosquito lifespan chart
ggplot() + geom_histogram(data = lf, aes(lifespan), fill = "steelblue", bins = 20) +
  ggtitle("Mosquito Lifespans") + xlab("Days") + ylab("Frequency") + theme_bw()


###############################################################################
# mosquito human hosts
###############################################################################

# who: 'human','all','zoo'
humanBloodHost <- function(mosquitos_df, who = "human"){

  n = nrow(mosquitos_df)
  w = rep(NaN,n)
  pb = txtProgressBar(min = 1, max = n, initial = 0)

  for(i in 1:n){
    # filter mosquitoes that were still alive at end of simulation
    if(tail(mosquitos_df[i,"behavior"][[1]],1) != "E"){
      bh = mosquitos_df[i,"bloodHosts"][[1]]
      switch(who,
             human = {w[i] = length(bh[bh > 0])},
             all = {w[i] = length(bh[bh > 0 | bh == -1])},
             zoo = {w[i] = length(bh[bh == -1])},
             {stop("argument 'who' must be in: 'human', 'all', or 'zoo'")}
             )
    }
    setTxtProgressBar(pb,i)
  }

  w = Filter(Negate(is.nan),w)
  return(data.frame(humanHost=w))
}

bh <- humanBloodHost(mosquitos_df)
mean(bh$humanHost)
sd(bh$humanHost)

#human blood meals histogram
ggplot() + geom_histogram(data = bh, aes(humanHost), fill = "#fc9272", binwidth = 1) +
  ggtitle("Human Bloodmeals") + xlab("Count") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(0, 11, 1)) + theme_bw()


###############################################################################
# intervals between blood meals
###############################################################################

# who: 'human','all','zoo'
bloodIntervals <- function(mosquitos_df, who = "human"){

  # check args
  if(!(who %in% c("human","all","zoo"))){stop("argument 'who' must be in: 'human', 'all', or 'zoo'")}

  # only want mosquitoes with more than 1 bloodmeal and died before end of the simulation
  filter <- sapply(mosquitos_df[,"bloodHosts"],function(x){length(x)>1}) & sapply(mosquitos_df[,"behavior"],function(x){tail(x,1)!="E"})

  # get the intervals
  intervals <- mapply(function(host,time,who){
    # get indices
    ix <- switch(who,
                 human = {which(host>0)},
                 all = {which(host>0 | host==-1)},
                 zoo = {which(host == -1)}
                 )
    # check we haven't indexed nothing
    if(length(ix)==0){
      return(NaN)
    } else {
      return(diff(time[ix]))
    }
  },
  host=mosquitos_df[which(filter),"bloodHosts"],
  time=mosquitos_df[which(filter),"timeFeed"],
  MoreArgs = list(who=who),
  USE.NAMES = FALSE)
  # end mapply call

  # clean up and return
  intervals <- unlist(intervals)
  intervals <- Filter(Negate(is.nan),intervals)

  return(data.frame(bmIntervals=intervals))
}

bi <- bloodIntervals(mosquitos_df)
ggplot() + geom_histogram(data = bi, aes(bmIntervals), fill = "chartreuse4", binwidth = 1) +
  ggtitle("Bloodmeal Interval") + xlab("Days") + ylab("Frequency")


###############################################################################
# human biting rate
###############################################################################

# not really a rate, its Q
humanBitingRate <- function(mosquitos_df){

  # only want mosquitoes who died before the end of simulation
  filter <- sapply(mosquitos_df[,"behavior"],function(x){tail(x,1)!="E"})

  # dont count mosquitoes who never took a blood meal
  hbr <- sapply(mosquitos_df[which(filter),"bloodHosts"],function(x){
    if(length(x)==1 & x[1]==0){
      return(NaN)
    }
    nHuman <- sum(x > 0)
    return(nHuman/length(x))
  })

  hbr <- Filter(Negate(is.nan),hbr)

  return(hbr)
}

hbr <- humanBitingRate(mosquitos_df)
mean(hbr)

###############################################################################
# vectorial capacity
###############################################################################

# # biyonka
# #the number of pairs of human bites
# #separated by at least EIP days
# #divided by the number of humans
# # Counts number of diff() of bite_time that are >= EIP (is that right?)
# VC = function(mosquito_df, EIP = 0.5) {
#   num_humans = length(human$id)
#
#   bites = sapply(mosquito_df$bite_times, FUN = function(v) {
#     if (length(v) == 1) {return (0)}
#     diff(v)
#   })
#
#   len_bites = sapply(test, FUN = function(v) {
#     length(v[v >= EIP])/num_humans
#   })
#
#   #turn into dataframe for ggplot useage
#   e = data.frame(x = t)
#
#   ggplot() + geom_histogram(data = e, aes(x), fill = "chartreuse4", bins = 4) +
#     ggtitle("Vectorial Capacity") + xlab("Vectorial Capacity") + ylab("Frequency")
#
# }
#
# VC(mosquitos)

###############################################################################
# dispersion of vectorial capacity
###############################################################################


###############################################################################
# lifetime egg production
###############################################################################


###############################################################################
# dispersion of lifetime egg production
###############################################################################
