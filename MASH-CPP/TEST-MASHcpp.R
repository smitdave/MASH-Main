###############################################################################
#       __  ______   _____ __  __      __________  ____
#      /  |/  /   | / ___// / / /     / ____/ __ \/ __ \
#     / /|_/ / /| | \__ \/ /_/ /_____/ /   / /_/ / /_/ /
#    / /  / / ___ |___/ / __  /_____/ /___/ ____/ ____/
#   /_/  /_/_/  |_/____/_/ /_/      \____/_/   /_/
#
#   MASH-CPP
#   Testing Ground
#   August 18, 2017
#
###############################################################################

library(MASHcpp)
library(R6)

DEBUG.MASHCPP()

class = R6Class("class", public = list(
  queue = MASHcpp::HumanEventQ(),
  initialize = function() {
    eventT = rlnorm(10)
    for(i in 1:10){
      self$queue$addEvent2Q(list(tEvent=eventT[i],tag="test",PAR=NULL))
    }
    },
  eraseQueue = function(){
    self$queue = NULL
  }
))

myClass = class$new()
myClass$queue$get_EventQ()
myClass$eraseQueue()
gc()

# list vs. pairlist comparison

testList = function(N){
  xx = list()
  for(i in 1:N){
    xx[[i]] = list(numbers=1:100)
  }
  rm(xx)
}

testPairList = function(N){
  xx = pairlist()
  for(i in 1:N){
    xx[[i]] = list(numbers=1:100)
  }
  rm(xx)
}

# dataPts = NULL
for(N in c(1e1,1e2,1e3,1e4,1e5)){
  print(microbenchmark::microbenchmark(
    testList(N = N),
    testPairList(N = N),
    times = 100
  ))
}

