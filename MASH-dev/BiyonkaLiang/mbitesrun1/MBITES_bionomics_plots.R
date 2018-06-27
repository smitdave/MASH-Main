rm(list=ls());gc()
library(spatstat)
library(truncdist)
library(viridis)
library(jsonlite)
set.seed(42)
library(ggplot2)
setwd("~/MASH-Main/MASH-dev/BiyonkaLiang/mbitesrun1")

mosquitos <- fromJSON("mosquito_F_1.json", flatten = TRUE)

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
