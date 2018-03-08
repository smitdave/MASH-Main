###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Resting
#     MBITES Team
#     March 2018
#
###############################################################################


# mbites_enterHouse <- function(){
#   p = private$site$
# }

mbites_restingSpot <- function(){
  if(self$isActive()){ # if mosquito not dead
    if(self$boutFailCheck()){ # check if they will just leave
      private$rspot = "l"
      private$search = TRUE
    } else {
      old_spot = private$rspot
       self$newSpot()
      if(old_spot != "i" & private$rspot == "i"){
        self$enterHouse()
      }
    }
  }
}


# check to see how many times i've failed my bout and gives a probability to leave
# even in the presence of resources
# returns a boolean indicating that the mosquito should go search if true, or stay if false
# this should be called from mbites_restingSpot!!!
mbites_boutFailCheck <- function(){
  p = dgeom(private$boutFail,MBITES:::Parameters$get_boutFail_p())
  if(runif(1) < p){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
