#setwd("~/Downloads")
setwd("~/MASH-Main/MASH-Docs")
require(jsonlite)
require(data.table)
require(HiveR)
library(dplyr)
library(igraph)

hp = fromJSON("HumanPathogens_Run1.json")
mp = fromJSON("MosquitoPathogens_Run1.json", simplifyDataFrame = TRUE)

#for every index where there is an infection event, pull out the ID of the mosquito that caused it
#do this for every person

#gets me the list for infections 
#lapply over names(hp)
humanForMosquito = function(name){
  if ( hp[[name]]$vectorInf[2] == "initInf" & (length(hp[[name]]$vectorInf) > 2)) {
    mos = hp[[name]]$vectorInf[seq(3, length(hp[[name]]$vectorInf), 1)] 
  }
  else if (length(hp[[name]]$vectorInf) > 2){
    mos = hp[[name]]$vectorInf[seq(2, length(hp[[name]]$vectorInf), 1)]
  }
  else if (length(hp[[name]]$vectorInf) <= 2){
    return (NULL)
  }
  human = rep(name, length(mos))
  data.frame(source = mos, sink = human, stringsAsFactors = FALSE)
}

# ###############################################################################
# # BEGIN SEAN EDIT FOR temporalNetwork.R
# ###############################################################################
# humanForMosquito = function(name){
#   if ( hp[[name]]$vectorInf[2] == "initInf" & (length(hp[[name]]$vectorInf) > 2)) {
#     mos = hp[[name]]$vectorInf[seq(3, length(hp[[name]]$vectorInf), 1)] 
#   }
#   else if (length(hp[[name]]$vectorInf) > 2){
#     mos = hp[[name]]$vectorInf[seq(2, length(hp[[name]]$vectorInf), 1)]
#   }
#   else if (length(hp[[name]]$vectorInf) <= 2){
#     return (NULL)
#   }
#   human = rep(name, length(mos))
#   time = hp[[name]]$eventT[which(hp[[name]]$events == "I")]
#   if(length(human)!=length(time)){
#     # browser()
#     time = time[-1]
#   }
#   data.frame(source = mos, sink = human, time = time, stringsAsFactors = FALSE)
# }
# 
# mylist = lapply(names(hp), humanForMosquito)
# myList = Filter(Negate(is.null),mylist)
# df = bind_rows(myList)
# ###############################################################################
# # END SEAN EDIT FOR temporalNetwork.R
# ###############################################################################

mylist = lapply(names(hp), humanForMosquito)
myList = Filter(Negate(is.null),mylist)
df = bind_rows(myList)

#check if pfid = -1
#if PfID = -1, then we need to remove that name
m = mp[mp$PfID != -1, ]

mosquito_df = data.frame(source = unlist(m$humanInf), sink = unlist(m$MosquitoID), stringsAsFactors = FALSE)
test = df
test = bind_rows(test, mosquito_df[1:1000, ])#data.frame(source = c("31_1", "31_1"), sink = c("99_2_1", "101_6_1"))) ##
#get the mosquitos that have infection == TRUE
infection = unlist(m[m$infected==TRUE, ]$MosquitoID)

hive1 <- edge2HPD(edge_df = test)
hive1$axis.cols = rep('#00000000', 4) # make invisible
hive3 = mineHPD(hive1, option = "axis <- source.man.sink")
hive3$nodes = hive3$nodes[order(hive3$nodes$axis),]

setcolor = function(str){
  #set mosquitos to be UCSF orange
  if (length(gregexpr(pattern = "_",text = str)[[1]]) == 2){
    #UCSF 100% orange if the mosquito has infected == TRUE
    if (str %in% infection) {"#F7A665"}
    #UCSF 70% orange for mosquitos that have infected == FALSE
    else {"#F48024"}
    }
  #set humans to be UCSF 70% blue
  else "#5DAFDB"
}

# setradius = function(hive_axis){
#   sms = length(unique(hive_axis))
#   unique_sms = sort(unique(hive_axis))
#   if (sms == 3){
#     r1 = seq(1, sum(hive_axis == unique_sms[1]), 1)
#     r2 = seq(1, sum(hive_axis == unique_sms[2]), 1)
#     r3 = seq(1, sum(hive_axis == unique_sms[3]), 1)
#     c(r1, r2, r3)
#   }
#   else if (sms == 2) {
#     r1 = seq(1, sum(hive_axis == unique_sms[1]), 1)
#     r2 = seq(1, sum(hive_axis == unique_sms[2]), 1)
#     c(r1, r2)
#   }
#   else {
#     seq(1, sum(hive_axis == unique_sms[1]), 1)
#   }
# }

setedgecolor = function(id1){
  #check id1, if id1 human, then we have human to mosquito infection. make that color UCSF grey blue
  #if id1 mosquito, then we have mosquito to human infection. make that color UCSF turquoise
  label = hive3$nodes[hive3$nodes$id==id1,]$lab
  if (length(gregexpr(pattern = "_",text = label)[[1]]) == 2){
    "#18A3AC"
  }
  else {
    "#506380"
  }
}

#first, order on node color
hive3$nodes = hive3$nodes[order(hive3$nodes$color), ]

#assign edge weights
hive3$edges$weight = rep(0.1, nrow(hive3$edges))

#assign colors to nodes
hive3$nodes$color = sapply(hive3$nodes$lab, setcolor)

#set size
hive3$nodes$size = rep(0.3, nrow(hive3$nodes), 1)

#then, construct radius
#match each label to a radius, then match radius to label in nodes dataframe using a join
axis1 = hive3$nodes[hive3$nodes$axis==1,]
axis1$radius = seq(60, nrow(axis1)+59, 1)
axis1 = axis1[,c(2, 4)]
axis2 = hive3$nodes[hive3$nodes$axis==2,]
axis2 = axis2[order(axis2$color), ]
axis2$radius = seq(1, nrow(axis2), 1)
axis2 = axis2[,c(2, 4)]
axis3 = hive3$nodes[hive3$nodes$axis==3,]
axis3 = axis3[order(axis3$color), ]
axis3$radius = seq(1, nrow(axis3), 1)
axis3 = axis3[,c(2, 4)]
rad = left_join(hive3$nodes[, c(1, 2, 3, 5, 6)], rbind(axis1, axis2, axis3), by="lab")
rad = rad[, c("id", "lab", "axis", "radius", "size", "color")]
hive3$nodes = rad

#assign edge colors
hive3$edges$color = sapply(hive3$edges$id1, setedgecolor)
#hive3$nodes$radius = setradius(hive3$nodes$axis)

plotHive(hive3, method = "abs", bkgnd = "white", axLab.pos = 1)
#top axis is sources (mostly mosquitos), bottom right are managers, bottom left are sinks

