#setwd("~/Downloads")
setwd("C:/Users/Biyonka/OneDrive/MASH-Main/MASH-Docs")
require(jsonlite)
require(data.table)
require(HiveR)
library(dplyr)
d = ggplot2::diamonds

hp = fromJSON("HumanPathogens_Run1.json")
mp = fromJSON("MosquitoPathogens_Run1.json", simplifyDataFrame = TRUE)

#con = file(description = "/Users/anophelesgambiae/Downloads/MosquitoPathogens_Run1.json",open = "r")
# debug(stream_in)
# undebug(stream_in)
#mp = jsonlite::stream_in(con = con,verbose = TRUE)

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

mylist = lapply(names(hp), humanForMosquito)
myList = Filter(Negate(is.null),mylist)
df = bind_rows(myList)

#check if pfid = -1
#if PfID = -1, then we need to remove that name
m = mp[mp$PfID != -1, ]

mosquito_df = data.frame(source = unlist(m$MosquitoID), sink = unlist(m$humanInf), stringsAsFactors = FALSE)
finaldf = bind_rows(df, mosquito_df)

finaldf["edge_weight"] = rep(0.05, length(finaldf$source))

#first attempt
hive1 <- edge2HPD(edge_df = finaldf)
#hive2 <- mineHPD(hive1, option = "size <- tot.edge.count")
hive1$nodes$radius = c(seq(1, 1765*10, 10))
hive1$nodes$color = rep("white", 1765)
hive1$edges$color = c(rep("blue", 543), rep("gray", 1678))
hive1$nodes$size = c(.1, rep(c(.1, .2, .3), 1765/3))
hive3 <- mineHPD(hive1, option = "axis <- source.man.sink")
plotHive(hive3, method = "abs", bkgnd = "black", axLabs = c("humans", "mosquitos"), axLab.pos = 1)


#Trying again with adjacency matric
#update, it looks shitty
gD <- simplify(graph.data.frame(df, directed=FALSE)) 
gAdj <- get.adjacency(gD, type = "upper", edges = FALSE, names = TRUE, sparse = FALSE)

hive1 <- adj2HPD(gAdj, type = "2D")
hive2 <- mineHPD(hive1, option = "size <- tot.edge.count")
hive1$nodes$radius = c(seq(1, 87*10, 10), seq(1, 394*10, 10))
hive1$nodes$color = rep("white", 481)
hive1$nodes$size = c(.1, rep(c(.1, .2, .3), 481/3))
hive3 <- mineHPD(hive1, option = "axis <- source.man.sink")
plotHive(hive3, method = "abs", bkgnd = "black", axLabs = c("humans", "mosquitos"), axLab.pos = 1)
