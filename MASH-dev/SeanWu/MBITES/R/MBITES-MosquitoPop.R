###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Mosquito Population
#     MBITES Team
#     February 2018
#
###############################################################################

###############################################################################
# MosquitoPop
###############################################################################


MosquitoPop <- R6::R6Class(classname = "MosquitoPop",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::HashMap,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(N){
                     super$initialize(N=N)
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     super$finalize()
                   } # end destructor


                 ),

                 # private members
                 private = list(

                   EggQ                = list(), # list of egg batches
                   ImagoQ              = list(), # list of newly emerging imagos
                   SiteP               = NULL # pointer to my enclosing Site (has-a relationship; Sites manage Resource lifespans)

                 )
) # end MosquitoPop class definition
