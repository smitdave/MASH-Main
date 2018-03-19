###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Generic SIS Model
#     MBITES Team
#     March 2018
#
###############################################################################


#' Generic SEI: Pathogen Class
#'
#' This is a generic SEI pathogen class without superinfection. Incubation times can be set to 0 to recover SI behavior.
#' Pathogen classes do not need explicit "recovery" dynamics as clearing them from a queue or vector in the host is akin
#' to killing this genetic clone and resetting the host to a "susceptible" state. Technically the pathogen only exists in
#' incubating or infectious states, but we keep the "SEI" designation for comprehension.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * field: i'm a field!
#'
#' @export
SEI_Pathogen <- R6::R6Class(classname = "SEI_Pathogen",
                 portable = TRUE,
                 cloneable = TRUE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Generic_Pathogen,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(){

                     super$initialize() # initialize base parts

                     futile.logger::flog.trace("SEI_Pathogen being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
                   }, # end constructor

                   # begin destructor
                   finalize = function(){

                     super$finalize() # destruct base parts

                     futile.logger::flog.trace("SEI_Pathogen being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
                   } # end destructor

                 ),

                 # private members
                 private = list(

                   infected = logical(1), # state: SI?
                   incubating = integer(1), # how long i have been incubating
                   incubation = integer(1) # how long i will incubate before infected = TRUE

                 )
) # end SEI_Pathogen class definition


###############################################################################
# Push Pathogen to Pedigree
###############################################################################

#' Generic SEI: Push a Pathogen to the Pedigree
#'
#' During a transmission event, push a pathogen to the pedigree.
#'
#' @param id id of this pathogen
#' @param parentID id of the parent of this pathogen
#' @param hID id of the human involved in this transmission event
#' @param mID id of the mosquito involved in this transmission event
#' @param tInf time of the infection
#' @param infEvent "M2H" or "H2M" for mosquito to human and human to mosquito transmission, respectively
#'
push2pedigree_SEI <- function(id,parentID,hID,mID,tInf,infEvent){
  pathogen = list(id=id,parentID=parentID,hID=hID,tInf=tInf,infEvent=infEvent)
  MBITES:::Pedigree$assign(key=id,value=pathogen)
}


###############################################################################
# Host Probing (probeHost): Mosquito -> Human Transmission
#   method bound to mosquito
###############################################################################


probeHost_SEI <- function(){
  if(private$pathogen$get_infected()){
    # based on pf dynamics; recombination occurs in the mosquito, therefore a simple clone
    # of the object is all that's needed
    pathogen = pathogen$clone()
    pathogen$set_infected(FALSE)
    pathogen$set_incubating(0L)
    pathogen$set_incubation()
    MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)$add_pathogen(pathogen)
  }
}

mosquito2human_SEI <- function(){

}


###############################################################################
# Host Feeding (feedHost): Human -> Mosquito Transmission
###############################################################################

bloodFeed_SEI <- function(){

}
