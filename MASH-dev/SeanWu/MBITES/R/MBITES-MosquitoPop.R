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
                   initialize = function(){
                     stop("initialize should never be called from abstract base class 'Aqua_Resource_Base'!")
                   }, # end constructor

                   # add egg batches to aquatic population
                   add_egg = function(){
                     stop("add_egg should never be called from abstract base class 'Aqua_Resource_Base'!")
                   },

                   # one day of aquatic population
                   one_day = function(){
                     stop("one_day should never be called from abstract base class 'Aqua_Resource_Base'!")
                   },

                   # send emerging imagos to adult population
                   push_imago = function(){
                     stop("push_imago should never be called from abstract base class 'Aqua_Resource_Base'!")
                   }

                 ),

                 # private members
                 private = list(

                   EggQ                = list(), # list of egg batches
                   ImagoQ              = list(), # list of newly emerging imagos
                   SiteP               = NULL # pointer to my enclosing Site (has-a relationship; Sites manage Resource lifespans)

                 )
) # end MosquitoPop class definition
