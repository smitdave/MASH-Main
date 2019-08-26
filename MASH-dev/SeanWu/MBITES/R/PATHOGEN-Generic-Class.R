###############################################################################
#         ____  ___  ________  ______  _____________   __
#        / __ \/   |/_  __/ / / / __ \/ ____/ ____/ | / /
#       / /_/ / /| | / / / /_/ / / / / / __/ __/ /  |/ /
#      / ____/ ___ |/ / / __  / /_/ / /_/ / /___/ /|  /
#     /_/   /_/  |_/_/ /_/ /_/\____/\____/_____/_/ |_/
#
#     PATHOGEN-Generic Class
#     MBITES Team
#     March 2018
#
###############################################################################

#' PATHOGEN: Generic Models
#'
#' All pathogen objects need to be able to communicate with a pathogen pedigree, which
#' links all clonal variants together through a \code{id} and \code{parentID} field.
#' Pathogen models link human and mosquito contacts which occur during feeding, see \code{\link{MBITES-HostEncounter}}
#' for more details on the "host encounter" process.
#'
#' All pathogen modules need to implement the following methods in humans:
#'  * add_pathogen(pathogen): during successful mosquito -> human transmission
#'
#' All pathogen modules need to implement the following methods in mosquitoes:
#'  * pathogenDynamics
#'  * probeHost
#'  * feedHost
#'
#' All pathogen modules need to implement the following methods in pathogens:
#'  * oneDay_human
#'
#'
#'
#' @name PathogenGeneric
NULL
#> NULL


#' Pathogen: Generic Class
#'
#' i'm a class!
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
Generic_Pathogen <- R6::R6Class(classname = "Generic_Pathogen",
                 portable = TRUE,
                 cloneable = TRUE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(parentID = NULL){

                     private$id = MBITES:::Pedigree$get_pathogen_id()
                     if(is.null(parentID)){
                       private$parentID = 0L
                     }

                     # futile.logger::flog.trace("Generic_Pathogen being born at self: %s , private: %s",pryr::address(self),pryr::address(private))
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     # futile.logger::flog.trace("Generic_Pathogen being killed at self: %s , private: %s",pryr::address(self),pryr::address(private))
                   }, # end destructor

                   oneDay_human = function(){
                     stop("oneDay_human should never be called from abstract base class 'Generic_Pathogen'!")
                   },

                   oneBout_mosquito = function(){
                     stop("oneBout_mosquito should never be called from abstract base class 'Generic_Pathogen'!")
                   }

                 ),

                 # private members
                 private = list(

                   id = integer(1), # pathogen id
                   parentID = integer(1) # parent id

                 )
) # end Generic_Pathogen class definition



# methods

get_id_Generic_Pathogen <- function(){
  return(private$id)
}

get_parentID_Generic_Pathogen <- function(){
  return(private$parentID)
}

Generic_Pathogen$set(which = "public",name = "get_id",
  value = get_id_Generic_Pathogen, overwrite = TRUE
)

Generic_Pathogen$set(which = "public",name = "get_parentID",
  value = get_parentID_Generic_Pathogen, overwrite = TRUE
)
