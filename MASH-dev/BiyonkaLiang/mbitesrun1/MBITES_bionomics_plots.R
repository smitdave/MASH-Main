library(spatstat)
library(truncdist)
library(viridis)
library(jsonlite)
set.seed(42)
library(ggplot2)
setwd("~/MASH-Main/MASH-dev/BiyonkaLiang/mbitesrun1")

mosquitos <- fromJSON("mosquito_F_1.json", flatten = TRUE)
human <- fromJSON("human_1.json", flatten = TRUE)

#Takes in a data frame with column called "time" that contains mosquito life events
#last time is death and first time is birth
#returns plot of lifespan
lifespan = function(mosquitos_df) {
  t = mosquitos_df$time
  w = sapply(t, FUN = function(v){
    tail(v, n=1) - v[1]
    })
  
  #unlist lifespan values
  w = unlist(w)
  #turn into dataframe for ggplot useage
  e = data.frame(x = w)

  #Mosquito lifespan chart
  ggplot() + geom_histogram(data = e, aes(x), fill = "steelblue", bins = 20) +
    ggtitle("Mosquito Lifespans") + xlab("Days") + ylab("Frequency")}

lifespan(mosquitos)

#Takes in a data frame with column called "bloodHosts" that contains IDs of blood hosts per mosquito
#returns plot of number of human bloodhosts

humanBloodHost = function(mosquitos_df) {
  blood_h = mosquitos_df$bloodHosts
  num_human_host = sapply(blood_h, FUN = function(v){
    #subset each bloodHost vector to only count humans (id >= 0)
  length(v[v>= 0])
  })
  
#turn into dataframe for ggplot useage
e = data.frame(x = num_human_host)

#Mosquito lifespan chart
ggplot() + geom_histogram(data = e, aes(x), fill = "#fc9272", binwidth = 1) +
  ggtitle("Human Bloodmeals") + xlab("Count") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(0, 11, 1))
}

humanBloodHost(mosquitos)

#Takes in a data frame with column called "bloodHosts" that contains IDs of blood hosts per mosquito
#subset each elem of time_feed such that it only contains values
#where corresponding Bloodhost is positive if human = TRUE. Else, get all intervals
#returns plot of number of bloodmeal intervals

bloodIntervals = function(mosquitos_df, human = TRUE) {
  blood_h = mosquitos_df$bloodHosts
  time_feed = mosquitos_df$timeFeed
  time_feed_copy = mosquitos_df$timeFeed
  if (human == TRUE){
    #get time_feed only where blood_h >= 0
  for (i in seq(1, length(time_feed), 1)){
    b_elem = blood_h[[i]]
    index = which(b_elem >= 0)
    time_feed_copy[[i]] = time_feed[[i]][index]
  }
}
  intervals = sapply(time_feed_copy, FUN = function(v){
    if (length(v) == 1) {return (0)}
    diff(v)
  })
  
  i = unlist(intervals)
  #turn into dataframe for ggplot useage
  e = data.frame(x = i)
  
  #Mosquito lifespan chart
  ggplot() + geom_histogram(data = e, aes(x), fill = "chartreuse4", binwidth = 1) +
    ggtitle("Bloodmeal Interval") + xlab("Days") + ylab("Frequency")
}

bloodIntervals(mosquitos)


#the number of pairs of human bites 
#separated by at least EIP days
#divided by the number of humans
# Counts number of diff() of bite_time that are >= EIP (is that right?)
VC = function(mosquito_df, EIP = 0.5) {
num_humans = length(human$id)

bites = sapply(mosquito_df$bite_times, FUN = function(v) { 
  if (length(v) == 1) {return (0)}
  diff(v)
})

len_bites = sapply(test, FUN = function(v) { 
  length(v[v >= EIP])/num_humans
})

#turn into dataframe for ggplot useage
e = data.frame(x = t)

ggplot() + geom_histogram(data = e, aes(x), fill = "chartreuse4", bins = 4) +
  ggtitle("Vectorial Capacity") + xlab("Vectorial Capacity") + ylab("Frequency")

}

VC(mosquitos)