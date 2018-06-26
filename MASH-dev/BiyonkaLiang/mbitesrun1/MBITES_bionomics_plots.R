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
lifespan = function(mosquitos_df) {t = mosquitos_df$time
  w = sapply(t, FUN = function(v){tail(v, n=1) - v[1]})
  #unlist lifespan values
  w = unlist(w)
  #turn into dataframe for ggplot useage
  e = data.frame(x = w)

  #Mosquito lifespan chart
  ggplot() + geom_histogram(data = e, aes(x), fill = "steelblue", bins = 20) +
    ggtitle("Mosquito Lifespans") + xlab("Mosquito Lifespan") + ylab("Frequency")}

lifespan(mosquitos)
