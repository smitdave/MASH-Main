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

