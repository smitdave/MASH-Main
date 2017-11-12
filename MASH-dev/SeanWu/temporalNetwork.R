rm(list=ls());gc()

library(ndtv)
library(jsonlite)
library(dplyr)

###############################################################################
# read data
###############################################################################

BIYONKA_DIR = "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/BiyonkaLiang/"

hp = jsonlite::fromJSON(txt = paste0(BIYONKA_DIR,"HumanPathogens_Run1.json"))
mp = jsonlite::fromJSON(txt = paste0(BIYONKA_DIR,"MosquitoPathogens_Run1.json"))

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
  time = hp[[name]]$eventT[which(hp[[name]]$events == "I")]
  if(length(human)!=length(time)){
    # browser()
    time = time[-1]
  }
  data.frame(source = mos, sink = human, time = time, stringsAsFactors = FALSE)
}

mylist = lapply(names(hp), humanForMosquito)
myList = Filter(Negate(is.null),mylist)
df = bind_rows(myList)


###############################################################################
# parse transmission
###############################################################################

# read edges from BiyonkaLiang/hiveplot.R in my edits
edges = df
# from: mosy, to: human, active: when the bite happened
names(edges) = c("from","to","active")
edges$active = edges$active - (min(edges$active)%%1+1)
edges$active = floor(edges$active)

pfNet <- network.initialize(0,directed = TRUE,bipartite = length(unique(edges$to)))
add.vertices.networkDynamic(x = pfNet,nv = length(unique(edges$to)),vertex.pid = unique(edges$to))
add.vertices.networkDynamic(x = pfNet,nv = length(unique(edges$from)),vertex.pid = unique(edges$from))

pfNet %v% "vertex.names" <- c(unique(edges$to),unique(edges$from))
pfNet %v% "vertex.col" <- c(rep("#5DAFDB", length(unique(edges$to))),rep("#F7A665",length(unique(edges$from))))

set.network.attribute(pfNet,"vertex.pid","vertex.names")
add.edges.networkDynamic(pfNet,
                         tail = get.vertex.id(pfNet, edges$from),
                         head = get.vertex.id(pfNet, edges$to),
                         edge.pid = paste0(edges$from, "->", edges$to))
activate.edges(pfNet, e =  1:nrow(edges), at = edges$active)


# slice.par <- list(start = 1, end = max(edges$active)+1, interval = 1, aggregate.dur = 1, rule = "earliest")
slice.par <- list(start = 1, end = 10, interval = 1, aggregate.dur = 1,rule="earliest")
compute.animation(pfNet,
                  animation.mode = "Graphviz",
                  slice.par = slice.par)
render.d3movie(pfNet,
               slice.par = slice.par,
               displaylabels = FALSE,
               output.mode = "htmlWidget",
               vertex.col = 'vertex.col')