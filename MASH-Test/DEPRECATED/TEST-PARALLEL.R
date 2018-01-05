##############################################################################################################
#
#       ___                                __                    __  ___      __            _       __
#      /   |  ____  ____  ____ ___  ____ _/ /___  __  _______   /  |/  /___ _/ /____  _____(_)___ _/ /____
#     / /| | / __ \/ __ \/ __ `__ \/ __ `/ / __ \/ / / / ___/  / /|_/ / __ `/ __/ _ \/ ___/ / __ `/ / ___/
#    / ___ |/ / / / /_/ / / / / / / /_/ / / /_/ / /_/ (__  )  / /  / / /_/ / /_/  __/ /  / / /_/ / (__  )
#   /_/  |_/_/ /_/\____/_/ /_/ /_/\__,_/_/\____/\__,_/____/  /_/  /_/\__,_/\__/\___/_/  /_/\__,_/_/____/
#
#   Test Non-trivial Parallel Applications
#
##############################################################################################################

library(parallel)

# we use a socket cluster because we don't want to fork the current environment
cl = parallel::makePSOCKcluster(names = 2)

parallel::clusterEvalQ(cl = cl[1],expr = {
  pid <- Sys.getpid()
  name <- Sys.info()["nodename"]
  str <- paste("This is R running on", name, "with PID", pid, "!")
})

parallel::clusterEvalQ(cl = cl[2],expr = {
  x = rnorm(n = 1e3)
})

parallel::clusterEvalQ(cl = cl[2],expr = {
  return(x)
})

parallel::clusterEvalQ(cl = cl,expr = {ls()})

stopCluster(cl)
rm(cl)


###############################################################################
# second round, one small step for R, one giant leap for epidemiologists
###############################################################################

cl = parallel::makePSOCKcluster(names = 3)

DIR = "/Users/slwu89/Desktop/MASHOUT/"
if(!dir.exists(DIR)){
  dir.create(DIR)
}
file2erase = system(command = paste0("ls ",DIR),intern = T)
for(file in file2erase){
  file.remove(paste0(DIR,file))
}

# debug(parallel:::sendCall)
# undebug(parallel:::sendCall)

# send class generator to all nodes
parallel::clusterEvalQ(cl = cl,expr = {
  testClass <<- R6::R6Class(classname = "test",
                           public = list(
                             x = 10,
                             y = 20,
                             conOut = vector(mode="list",length=3)
                           )
  )
  classObj <<- testClass$new()
})

parallel::clusterEvalQ(cl = cl,expr = {ls()})

# send data to node 1 and do a 'calculation'
parallel:::sendCall(con = cl[[1]],fun = function(x){
  aClass <<- testClass$new()
  y <<- x + 5
},args = list(x = 10),return = FALSE,tag = 1)

node1out = parallel:::recvData(node = cl[[1]])

parallel::clusterEvalQ(cl = cl,expr = {ls()})

# get something specific out of node 1
node1specific = parallel::clusterCall(cl = cl[1],fun = function(){
  return(
    aClass$x
  )
})

# send data to node 2 and do a 'calculation', and write out the 'calculation'
parallel:::sendCall(con = cl[[2]],fun = function(DIR){
  x <<- rexp(n = 10)
  con = file(description = paste0(DIR,"node2out.txt"),open = "wt")
  pid = Sys.getpid()
  name = Sys.info()["nodename"]
  str = paste("Node 2 reporting in, running on", name, "with PID", pid, "!")
  writeLines(text = str,con =con)
  writeLines(text = paste0("node 2 is printing! the results of x were: ",x),con =con)
  close(con)
},args = list(DIR = DIR),return = FALSE,tag = 2)

node2out = parallel:::recvData(node = cl[[2]])

parallel::clusterEvalQ(cl = cl,expr = {ls()})

# do a more strenuous computation on node 3
# first, push the DIR to that node's memory
parallel:::sendCall(con = cl[[3]],fun = function(DIR){
  DIR <<- DIR
},args = list(DIR = DIR),return = FALSE,tag = 3)
# do a 'computation' and write to txt
parallel:::sendCall(con = cl[[3]],fun = function(){
  con = file(description = paste0(DIR,"node3out.txt"),open = "wt")
  pid = Sys.getpid()
  name = Sys.info()["nodename"]
  str = paste("Node 3 reporting in, running on", name, "with PID", pid, "!")
  writeLines(text = str,con =con)
  writeLines(text = paste0("node 3 is printing from pid: ",pid),con = con)
  for(i in 1:1e3){
    writeLines(text = paste0(i),con = get(x = "con"))
  }
  close(con)
},args = list(),return = FALSE,tag = 3)

# let's come back to node 1 and tell it to print out some stuff
parallel:::sendCall(con = cl[[1]],fun = function(DIR){
  con = file(description = paste0(DIR,"node1out.txt"),open = "wt")
  pid = Sys.getpid()
  name = Sys.info()["nodename"]
  str = paste("Node 1 reporting in, running on", name, "with PID", pid, "!")
  writeLines(text = str,con =con)
  close(con)
},args = list(DIR=DIR),return = FALSE,tag = 1)

parallel::clusterEvalQ(cl = cl,expr = {ls()})

# it's all good, shut it down...
parallel::stopCluster(cl)
rm(cl)



# ###############################################################################
# # parallel test
# ###############################################################################
# 
# cl = parallel::makeCluster(spec = 2,type = "PSOCK")
# 
# parallel::clusterEvalQ(cl = cl,expr = {
#   pid <- Sys.getpid()
#   name <- Sys.info()["nodename"]
#   str <- paste("This is R running on", name, "with PID", pid, "!")
#   return(str)
# })
# 
# parallel::clusterEvalQ(cl = cl,expr = {
#   
#   library(MASHmicro)
#   # set.seed(42L)
#   
#   DEBUG.MASHMICRO()
#   MASHcpp::DEBUG.MASHCPP()
#   MASHmacro::DEBUG.MASHMACRO()
#   
#   # make a tile
#   pid <- Sys.getpid()
#   DIR = paste0("/Users/slwu89/Desktop/MASHOUT",pid,"/")
#   
#   # setup
#   Humans.MICRO.Setup()
#   PfSI.MICRO.Setup(Pf_c = 1,Pf_b = 1,LatentPf = 1,DurationPf = 20)
#   AQUA.Emerge.Setup()
#   
#   MBITES.Generic.Setup()
#   MBITES.BRO.Setup(aquaModule = "emerge",timing = "exponential")
#   # MBITES.BRO.Cohort.Setup()
#   
#   SEARCH.Kernel.Setup(MBITES = "BRO")
#   
#   # landscape parameters
#   nAqua = 10
#   nFeed = 10
#   emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
#   landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,pointGen = "poisson",module = "emerge",modulePars = emerge_par)
#   
#   # human parameters
#   human_par = MASHmacro::HumanPop.Parameters(nSite = nFeed,siteSize = 10,siteMin = 2)
#   
#   # M-BITES parameters
#   nMosy = 50
#   mbites_par = MBITES.BRO.Parameters(PfEIP=1)
#   mosquito_par = list(
#     N_female = nMosy,
#     ix_female = rep(1,nMosy),
#     genotype_female = rep(1,nMosy),
#     MBITES_PAR = mbites_par
#   )
#   
#   MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)
#   
#   # MicroLandscapePlot_utility(Landscape = MicroTile$get_Landscape())
#   
#   # MicroTile$get_FemalePop()$simCohort(N = 1e3)
#   
#   MicroTile$get_HumanPop()$init_ActivitySpace()
#   
#   # MicroTile$get_ActivitySpace()
#   
#   MicroTile$get_HumanPop()$init_PfSI(PfPR = 0.95)
#   
#   MicroTile$simMICRO_oneRun(tMax = 50,verbose = FALSE,trackPop = TRUE)
#   
# })
# 
# parallel::clusterEvalQ(cl = cl,expr = {
#   MicroTile$get_ActivitySpace()
# })
# parallel::stopCluster(cl)
# rm(cl);gc()
