###############################################################################
#       __  ______   _____ __  __      __________  ____
#      /  |/  /   | / ___// / / /     / ____/ __ \/ __ \
#     / /|_/ / /| | \__ \/ /_/ /_____/ /   / /_/ / /_/ /
#    / /  / / ___ |___/ / __  /_____/ /___/ ____/ ____/
#   /_/  /_/_/  |_/____/_/ /_/      \____/_/   /_/
#
#   MASH-CPP
#   Testing Ground
#   August 31, 2017
#
###############################################################################

class = R6Class("class", public = list(

  initialize = function(n=10) {
    private$queue = MASHcpp::HumanEventQ()
    eventT = rlnorm(n = n)
    for(i in 1:length(eventT)){
      private$queue$addEvent2Q(list(tEvent=eventT[i],tag="test",PAR=NULL))
      }
    },
  addQ = function(){
    private$queue$addEvent2Q(list(tEvent=500,tag="test",PAR=NULL))
  },
  eraseQueue = function(){
    private$queue = NULL
  },
  addQstuff = function(time){
    private$queue$addEvent2Q(list(tEvent=time,tag="test",PAR=NULL))
  },
  get_QueueN = function(){private$queue$get_queueN()},
  get_queue = function(){private$queue$get_EventQ()}
),private=list( queue = NULL))
